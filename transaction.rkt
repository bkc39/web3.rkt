#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         (only-in keccak keccak256)
         (only-in racket/list drop-right)
         (only-in racket/match define-match-expander match match-define)
         (only-in "abi/type.rkt" abi-value/c)
         (only-in "account.rkt"
                  recover-address
                  secp256k1-order
                  signature
                  signature-low-s?
                  signature?)
         (only-in (submod "account.rkt" private) sign-digest/who)
         (only-in "address.rkt" address->checksum-string checksum-address?)
         "private/exn.rkt"
         (only-in "private/hex.rkt" bytes->hex bytes->natural hex->bytes)
         (only-in "private/opaque.rkt" opaque-> opaque/c secret-key/c)
         "private/who.rkt"
         (only-in "rlp.rkt" natural->rlp-scalar rlp-encode)
         (only-in (submod "rlp.rkt" internal) rlp-decode/who))

(define uint64/c (flat-named-contract 'uint64/c (integer-in 0 (sub1 (expt 2 64)))))
(define uint256/c (flat-named-contract 'uint256/c (integer-in 0 (sub1 (expt 2 256)))))
(define to/c (or/c #f (abi-value/c 'address)))
(define storage-key/c
  (flat-named-contract 'storage-key/c (lambda (v) (and (bytes? v) (= (bytes-length v) 32)))))
(define access-list/c
  (flat-named-contract 'access-list/c
                       (listof (cons/c (abi-value/c 'address) (listof storage-key/c)))))
(define transaction-hash/c
  (flat-named-contract 'transaction-hash/c
                       (lambda (v) (and (string? v) (regexp-match? #px"^0x[0-9a-f]{64}$" v)))))
(define digest-bytes/c
  (flat-named-contract 'digest-bytes/c (lambda (v) (and (bytes? v) (= (bytes-length v) 32)))))

(provide (struct-out exn:fail:web3:raw-transaction)
         signed-transaction
         (contract-out
          [access-list/c flat-contract?]
          [transaction? (-> any/c boolean?)]
          [legacy-transaction? (-> any/c boolean?)]
          [eip2930-transaction? (-> any/c boolean?)]
          [eip1559-transaction? (-> any/c boolean?)]
          [legacy-transaction
           (->* (#:chain-id (or/c #f uint64/c) #:nonce uint64/c #:gas-price uint256/c
                 #:gas-limit uint64/c #:to to/c)
                (#:value uint256/c #:data bytes?)
                legacy-transaction?)]
          [eip2930-transaction
           (->* (#:chain-id uint64/c #:nonce uint64/c #:gas-price uint256/c
                 #:gas-limit uint64/c #:to to/c)
                (#:value uint256/c #:data bytes? #:access-list access-list/c)
                eip2930-transaction?)]
          [eip1559-transaction
           (->i (#:chain-id [chain-id uint64/c]
                 #:nonce [nonce uint64/c]
                 #:max-priority-fee-per-gas [priority uint256/c]
                 #:max-fee-per-gas [max-fee uint256/c]
                 #:gas-limit [gas-limit uint64/c]
                 #:to [to to/c])
                (#:value [value uint256/c]
                 #:data [data bytes?]
                 #:access-list [access-list access-list/c])
                #:pre/name (priority max-fee)
                "max-priority-fee-per-gas is at most max-fee-per-gas"
                (<= priority max-fee)
                [_ eip1559-transaction?])]
          [transaction-type (-> transaction? (or/c 0 1 2))]
          [transaction-chain-id (-> transaction? (or/c #f uint64/c))]
          [transaction-nonce (-> transaction? uint64/c)]
          [transaction-gas-price (-> (or/c legacy-transaction? eip2930-transaction?) uint256/c)]
          [transaction-max-priority-fee-per-gas (-> eip1559-transaction? uint256/c)]
          [transaction-max-fee-per-gas (-> eip1559-transaction? uint256/c)]
          [transaction-gas-limit (-> transaction? uint64/c)]
          [transaction-to (-> transaction? (or/c #f checksum-address?))]
          [transaction-value (-> transaction? uint256/c)]
          [transaction-data (-> transaction? (and/c bytes? immutable?))]
          [transaction-access-list
           (-> (or/c eip2930-transaction? eip1559-transaction?) access-list/c)]
          [transaction->bytes (-> transaction? bytes?)]
          [transaction-signing-hash (-> transaction? digest-bytes/c)]
          [sign-transaction (opaque-> secret-key/c transaction/c signed-transaction?)]
          [signed-transaction? (-> any/c boolean?)]
          [signed-transaction-transaction (-> signed-transaction? transaction?)]
          [signed-transaction-signature (-> signed-transaction? signature?)]
          [signed-transaction-sender (-> signed-transaction? checksum-address?)]
          [signed-transaction-hash (-> signed-transaction? transaction-hash/c)]
          [signed-transaction->bytes (-> signed-transaction? bytes?)]
          [bytes->signed-transaction (-> bytes? signed-transaction?)]))

(module* internal #f
  (provide bytes->signed-transaction/who
           raw-transaction-reason/c
           uint64/c
           uint256/c
           access-list/c))

(define raw-transaction-phrases
  (hasheq 'type "not a transaction"
          'shape "malformed transaction"
          'leading-zero "leading zero"
          'range "out of range"
          'length "wrong length"
          'high-s "high s"
          'no-sender "no sender"))

(define raw-transaction-reason/c
  (flat-named-contract 'raw-transaction-reason/c
                       (apply or/c (hash-keys raw-transaction-phrases #t))))

(define kind-names
  (hasheqv 0 "legacy-transaction" 1 "eip2930-transaction" 2 "eip1559-transaction"))

(define wire-fields
  (hasheqv 0 '(nonce gas-price gas-limit to value data v r s)
           1 '(chain-id nonce gas-price gas-limit to value data access-list y-parity r s)
           2 '(chain-id nonce max-priority-fee-per-gas max-fee-per-gas gas-limit to value data
                        access-list y-parity r s)))

(define field-kinds
  (hasheq 'chain-id 'u64
          'nonce 'u64
          'gas-price 'u256
          'max-priority-fee-per-gas 'u256
          'max-fee-per-gas 'u256
          'gas-limit 'u64
          'to 'to
          'value 'u256
          'data 'data
          'access-list 'access-list
          'v 'v
          'y-parity 'y-parity
          'r 'curve-scalar
          's 'curve-scalar))

(define (unsigned-fields type)
  (drop-right (hash-ref wire-fields type) 3))

(struct transaction (type chain-id nonce gas-price max-priority-fee-per-gas max-fee-per-gas
                          gas-limit to value data access-list payload preimage)
  #:constructor-name unsigned-transaction
  #:property prop:equal+hash
  (list (lambda (this other _) (equal? (transaction-preimage this) (transaction-preimage other)))
        (lambda (this _) (equal-hash-code (transaction-preimage this)))
        (lambda (this _) (equal-secondary-hash-code (transaction-preimage this))))
  #:property prop:custom-write
  (lambda (tx port _)
    (define chain-id (transaction-chain-id tx))
    (write-string (format "#<~a~a nonce ~a>"
                          (hash-ref kind-names (transaction-type tx))
                          (if chain-id (format " chain-id ~a" chain-id) "")
                          (transaction-nonce tx))
                  port)))

(struct signed-transaction (transaction signature sender hash raw)
  #:name signed-struct
  #:constructor-name sealed-transaction
  #:property prop:equal+hash
  (list (lambda (this other _)
          (equal? (signed-transaction-raw this) (signed-transaction-raw other)))
        (lambda (this _) (equal-hash-code (signed-transaction-raw this)))
        (lambda (this _) (equal-secondary-hash-code (signed-transaction-raw this))))
  #:property prop:custom-write
  (lambda (stx port _)
    (write-string (format "#<signed-transaction ~a>" (signed-transaction-hash stx)) port)))

(define transaction/c (opaque/c 'transaction? "a transaction" transaction?))

(define (legacy-transaction? v)
  (and (transaction? v) (eqv? (transaction-type v) 0)))

(define (eip2930-transaction? v)
  (and (transaction? v) (eqv? (transaction-type v) 1)))

(define (eip1559-transaction? v)
  (and (transaction? v) (eqv? (transaction-type v) 2)))

(define-module-boundary-contract checked-signed-transaction attach-signature
  (-> transaction? signature? signed-transaction?)
  #:name-for-blame signed-transaction)

(define-match-expander signed-transaction
  (syntax-parser
    [(_ tx sig) #'(signed-struct tx sig _ _ _)])
  (syntax-parser
    [_:id #'checked-signed-transaction]
    [(_ . args) #'(checked-signed-transaction . args)]))

(define (legacy-transaction #:chain-id chain-id #:nonce nonce #:gas-price gas-price
                            #:gas-limit gas-limit #:to to #:value [value 0] #:data [data #""])
  (error 'unimplemented))

(define (eip2930-transaction #:chain-id chain-id #:nonce nonce #:gas-price gas-price
                             #:gas-limit gas-limit #:to to #:value [value 0] #:data [data #""]
                             #:access-list [access-list '()])
  (error 'unimplemented))

(define (eip1559-transaction #:chain-id chain-id #:nonce nonce
                             #:max-priority-fee-per-gas priority #:max-fee-per-gas max-fee
                             #:gas-limit gas-limit #:to to #:value [value 0] #:data [data #""]
                             #:access-list [access-list '()])
  (error 'unimplemented))

(define (transaction->bytes tx)
  (error 'unimplemented))

(define (transaction-signing-hash tx)
  (error 'unimplemented))

(define (sign-transaction key tx)
  (error 'unimplemented))

(define (attach-signature tx sig)
  (error 'unimplemented))

(define (signed-transaction->bytes stx)
  (error 'unimplemented))

(define/who (bytes->signed-transaction who bs)
  (error 'unimplemented))
