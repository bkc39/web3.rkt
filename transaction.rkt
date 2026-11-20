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
  (build 0 (hasheq 'chain-id chain-id 'nonce nonce 'gas-price gas-price 'gas-limit gas-limit
                   'to (stored-address to) 'value value 'data (bytes->immutable-bytes data))))

(define (eip2930-transaction #:chain-id chain-id #:nonce nonce #:gas-price gas-price
                             #:gas-limit gas-limit #:to to #:value [value 0] #:data [data #""]
                             #:access-list [access-list '()])
  (build 1 (hasheq 'chain-id chain-id 'nonce nonce 'gas-price gas-price 'gas-limit gas-limit
                   'to (stored-address to) 'value value 'data (bytes->immutable-bytes data)
                   'access-list (stored-access-list access-list))))

(define (eip1559-transaction #:chain-id chain-id #:nonce nonce
                             #:max-priority-fee-per-gas priority #:max-fee-per-gas max-fee
                             #:gas-limit gas-limit #:to to #:value [value 0] #:data [data #""]
                             #:access-list [access-list '()])
  (build 2 (hasheq 'chain-id chain-id 'nonce nonce 'max-priority-fee-per-gas priority
                   'max-fee-per-gas max-fee 'gas-limit gas-limit 'to (stored-address to)
                   'value value 'data (bytes->immutable-bytes data)
                   'access-list (stored-access-list access-list))))

(define (stored-address address)
  (and address (string->immutable-string (address->checksum-string address))))

(define (stored-access-list access-list)
  (for/list ([entry (in-list access-list)])
    (match-define (cons address keys) entry)
    (cons (stored-address address) (map bytes->immutable-bytes keys))))

(define (build type fields)
  (define (field name)
    (hash-ref fields name #f))
  (define payload
    (for/list ([name (in-list (unsigned-fields type))])
      (field->item (hash-ref field-kinds name) (field name))))
  (unsigned-transaction type (field 'chain-id) (field 'nonce) (field 'gas-price)
                        (field 'max-priority-fee-per-gas) (field 'max-fee-per-gas)
                        (field 'gas-limit) (field 'to) (field 'value) (field 'data)
                        (field 'access-list) payload
                        (bytes->immutable-bytes
                         (encode-unsigned type (field 'chain-id) payload))))

(define (field->item kind value)
  (match kind
    [(or 'u64 'u256) (natural->rlp-scalar value)]
    ['to (if value (hex->bytes value) #"")]
    ['data value]
    ['access-list
     (for/list ([entry (in-list value)])
       (match-define (cons address keys) entry)
       (list (hex->bytes address) keys))]))

(define (encode-unsigned type chain-id payload)
  (cond
    [(positive? type) (bytes-append (bytes type) (rlp-encode payload))]
    [chain-id (rlp-encode (append payload (list (natural->rlp-scalar chain-id) #"" #"")))]
    [else (rlp-encode payload)]))

(define (encode-signed tx sig)
  (match-define (signature r s y-parity) sig)
  (define type (transaction-type tx))
  (define chain-id (transaction-chain-id tx))
  (define v
    (cond
      [(positive? type) y-parity]
      [chain-id (+ 35 (* 2 chain-id) y-parity)]
      [else (+ 27 y-parity)]))
  (define encoded
    (rlp-encode (append (transaction-payload tx) (map natural->rlp-scalar (list v r s)))))
  (if (positive? type) (bytes-append (bytes type) encoded) encoded))

(define (transaction->bytes tx)
  (bytes-copy (transaction-preimage tx)))

(define (transaction-signing-hash tx)
  (keccak256 (transaction-preimage tx)))

(define (sign-transaction key tx)
  (define digest (transaction-signing-hash tx))
  (attach-signature/digest 'sign-transaction tx (sign-digest/who 'sign-transaction key digest)
                           digest))

(define (attach-signature tx sig)
  (attach-signature/digest 'signed-transaction tx sig (transaction-signing-hash tx)))

(define (attach-signature/digest who tx sig digest)
  (define low-s? (signature-low-s? sig))
  (define sender (and low-s? (recover-address sig digest)))
  (cond
    [(not low-s?) (raise-signature-error who "s is more than half the curve order (EIP-2)")]
    [(not sender) (raise-signature-error who "the signature recovers no address")]
    [else (seal tx sig sender)]))

(define (seal tx sig sender)
  (define raw (bytes->immutable-bytes (encode-signed tx sig)))
  (sealed-transaction tx sig sender (string->immutable-string (bytes->hex (keccak256 raw))) raw))

(define (signed-transaction->bytes stx)
  (bytes-copy (signed-transaction-raw stx)))

(define (raise-signature-error who detail)
  (raise (exn:fail:web3:signature (format "~a: ~a" who detail) (current-continuation-marks))))

(define (raise-raw who reason field detail)
  (raise (exn:fail:web3:raw-transaction
          (format "~a: ~a: ~a" who (hash-ref raw-transaction-phrases reason) detail)
          (current-continuation-marks)
          field
          reason)))

(define/who (bytes->signed-transaction who bs)
  (define type (envelope-type who bs))
  (define items (fields-of who type (rlp-decode/who who bs (if (zero? type) 0 1))))
  (define fields
    (for/fold ([fields #hasheq()])
              ([name (in-list (hash-ref wire-fields type))]
               [item (in-list items)])
      (hash-set fields name (read-field who name item))))
  (define (field name)
    (hash-ref fields name #f))
  (define legacy-v (field 'v))
  (define chain-id (if legacy-v (v-chain-id legacy-v) (field 'chain-id)))
  (define sig
    (signature (field 'r) (field 's) (if legacy-v (v-y-parity legacy-v) (field 'y-parity))))
  (define priority (field 'max-priority-fee-per-gas))
  (cond
    [(and priority (> priority (field 'max-fee-per-gas)))
     (raise-raw who 'range 'max-priority-fee-per-gas
                "max-priority-fee-per-gas is more than max-fee-per-gas")]
    [(not (signature-low-s? sig))
     (raise-raw who 'high-s 's "s is more than half the curve order (EIP-2)")]
    [else
     (define tx (build type (hash-set fields 'chain-id chain-id)))
     (define sender (recover-address sig (transaction-signing-hash tx)))
     (unless sender
       (raise-raw who 'no-sender #f "the signature recovers no address"))
     (seal tx sig sender)]))

(define (envelope-type who bs)
  (define first-byte (and (positive? (bytes-length bs)) (bytes-ref bs 0)))
  (cond
    [(not first-byte) (raise-raw who 'type #f "no bytes")]
    [(>= first-byte #xc0) 0]
    [(memv first-byte '(1 2)) first-byte]
    [(< first-byte #x80) (raise-raw who 'type #f (format "type ~a is not supported" first-byte))]
    [else
     (raise-raw who 'type #f
                (format "first byte 0x~a is neither a type nor a list"
                        (number->string first-byte 16)))]))

(define (fields-of who type item)
  (define expected (length (hash-ref wire-fields type)))
  (define count (and (list? item) (length item)))
  (cond
    [(not count)
     (raise-raw who 'shape #f (format "a type ~a transaction is a list, not a byte string" type))]
    [(= count expected) item]
    [(zero? type)
     (raise-raw who 'shape #f (format "a legacy transaction has ~a fields, not ~a" expected count))]
    [else
     (raise-raw who 'shape #f
                (format "a type ~a transaction has ~a fields, not ~a" type expected count))]))

(define (read-field who name item)
  (define kind (hash-ref field-kinds name))
  (cond
    [(eq? kind 'access-list) (read-access-list who item)]
    [(list? item) (raise-raw who 'shape name (format "~a is a list, not a byte string" name))]
    [(eq? kind 'data) (bytes->immutable-bytes item)]
    [(eq? kind 'to) (read-to who item)]
    [(and (positive? (bytes-length item)) (zero? (bytes-ref item 0)))
     (raise-raw who 'leading-zero name (format "in ~a" name))]
    [else (read-scalar who name kind item)]))

(define (v-chain-id legacy-v)
  (and (>= legacy-v 35) (quotient (- legacy-v 35) 2)))

(define (v-y-parity legacy-v)
  (if (>= legacy-v 35) (remainder (- legacy-v 35) 2) (- legacy-v 27)))

(define (legacy-v? legacy-v)
  (or (= legacy-v 27)
      (= legacy-v 28)
      (and (>= legacy-v 35) (< (v-chain-id legacy-v) (expt 2 64)))))

(define (curve-scalar? scalar)
  (< 0 scalar secp256k1-order))

(define (y-parity? parity)
  (<= parity 1))

(define scalar-rules
  (hasheq 'u64 (list 8 values "~a is 2^64 or more")
          'u256 (list 32 values "~a is 2^256 or more")
          'v (list 9 legacy-v?
                   "~a is not 27, 28, or 35 + 2 * chain-id + y-parity with chain-id below 2^64")
          'y-parity (list 1 y-parity? "~a is not 0 or 1")
          'curve-scalar (list 32 curve-scalar? "~a is zero or not less than the curve order")))

(define (read-scalar who name kind item)
  (match-define (list max-size valid? template) (hash-ref scalar-rules kind))
  (define natural (and (<= (bytes-length item) max-size) (bytes->natural item)))
  (if (and natural (valid? natural))
      natural
      (raise-raw who 'range name (format template name))))

(define (read-to who item)
  (match (bytes-length item)
    [0 #f]
    [20 (stored-address (bytes->hex item))]
    [size (raise-raw who 'length 'to (format "to has ~a bytes, not 0 or 20" size))]))

(define (access-entry? entry)
  (match entry
    [(list (? bytes?) (list (? bytes?) ...)) #t]
    [_ #f]))

(define (read-access-list who item)
  (define misshapen
    (and (list? item)
         (for/first ([entry (in-list item)] [index (in-naturals)] #:unless (access-entry? entry))
           index)))
  (cond
    [(bytes? item)
     (raise-raw who 'shape 'access-list "access-list is a byte string, not a list")]
    [misshapen
     (raise-raw who 'shape 'access-list
                (format "access-list entry ~a is not an address and a list of storage keys"
                        misshapen))]
    [else
     (for/list ([entry (in-list item)] [index (in-naturals)])
       (read-access-entry who index entry))]))

(define (read-access-entry who index entry)
  (match-define (list address keys) entry)
  (define bad-key (for/first ([key (in-list keys)] #:unless (= (bytes-length key) 32)) key))
  (cond
    [(not (= (bytes-length address) 20))
     (raise-raw who 'length 'access-list
                (format "access-list entry ~a has an address of ~a bytes, not 20"
                        index (bytes-length address)))]
    [bad-key
     (raise-raw who 'length 'access-list
                (format "access-list entry ~a has a storage key of ~a bytes, not 32"
                        index (bytes-length bad-key)))]
    [else (cons (stored-address (bytes->hex address)) (map bytes->immutable-bytes keys))]))
