#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         (only-in keccak keccak256)
         (only-in racket/list drop-right)
         (only-in racket/match define-match-expander match match* match-define match-lambda)
         (only-in "account.rkt"
                  recover-address
                  secp256k1-order
                  signature
                  signature-low-s?
                  signature?)
         (only-in (submod "account.rkt" private) sign-digest/who)
         (only-in "address.rkt" address->checksum-string checksum-address?)
         "private/exn.rkt"
         (only-in "private/field-contracts.rkt"
                  access-list/c bytes32/c hash-string/c to/c uint256/c uint64/c)
         (only-in "private/hex.rkt" bytes->hex bytes->natural hex->bytes natural->hex)
         (only-in "private/opaque.rkt" address/c opaque-> opaque/c secret-key/c)
         "private/who.rkt"
         (only-in "rlp.rkt" natural->rlp-scalar rlp-encode)
         (only-in (submod "rlp.rkt" internal) rlp-decode/who))

(provide (struct-out exn:fail:web3:raw-transaction)
         signed-transaction
         (contract-out
          [uint64/c flat-contract?]
          [uint256/c flat-contract?]
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
          [transaction-signing-hash (-> transaction? bytes32/c)]
          [sign-transaction (opaque-> secret-key/c transaction/c signed-transaction?)]
          [signed-transaction? (-> any/c boolean?)]
          [signed-transaction-transaction (-> signed-transaction? transaction?)]
          [signed-transaction-signature (-> signed-transaction? signature?)]
          [signed-transaction-sender (-> signed-transaction? checksum-address?)]
          [signed-transaction-hash (-> signed-transaction? hash-string/c)]
          [signed-transaction->bytes (-> signed-transaction? bytes?)]
          [bytes->signed-transaction (-> bytes? signed-transaction?)]
          [transaction->jsexpr (opaque-> transaction/c #:from address/c hash?)]))

(module* internal #f
  (provide bytes->signed-transaction/who
           raw-transaction-reason/c
           transaction/c
           transaction-difference
           (rename-out [hash-string/c transaction-hash/c])
           wire-fields))

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

(define (stored-fields type)
  (if (zero? type) (append (unsigned-fields type) '(chain-id)) (unsigned-fields type)))

(define (json-key name)
  (match (regexp-split #rx"-" (symbol->string name))
    ['("gas" "limit") 'gas]
    [(cons head tail) (string->symbol (apply string-append head (map string-titlecase tail)))]))

(define (equal+hash-by key)
  (list (lambda (this other _) (equal? (key this) (key other)))
        (lambda (this _) (equal-hash-code (key this)))
        (lambda (this _) (equal-secondary-hash-code (key this)))))

(struct transaction (type fields payload preimage)
  #:constructor-name unsigned-transaction
  #:property prop:equal+hash (equal+hash-by (lambda (tx) (transaction-preimage tx)))
  #:property prop:custom-write
  (lambda (tx port _)
    (define chain-id (transaction-field tx 'chain-id))
    (write-string (format "#<~a~a nonce ~a>"
                          (hash-ref kind-names (transaction-type tx))
                          (if chain-id (format " chain-id ~a" chain-id) "")
                          (transaction-field tx 'nonce))
                  port)))

(struct signed-transaction (transaction signature sender hash raw)
  #:name signed-struct
  #:constructor-name sealed-transaction
  #:property prop:equal+hash (equal+hash-by (lambda (stx) (signed-transaction-raw stx)))
  #:property prop:custom-write
  (lambda (stx port _)
    (write-string (format "#<signed-transaction ~a>" (signed-transaction-hash stx)) port)))

(define transaction/c (opaque/c 'transaction? "a transaction" transaction?))

(define (transaction-of-type? v type)
  (and (transaction? v) (eqv? (transaction-type v) type)))

(define (legacy-transaction? v)
  (transaction-of-type? v 0))

(define (eip2930-transaction? v)
  (transaction-of-type? v 1))

(define (eip1559-transaction? v)
  (transaction-of-type? v 2))

(define (legacy-transaction #:chain-id chain-id #:nonce nonce #:gas-price gas-price
                            #:gas-limit gas-limit #:to to #:value [value 0] #:data [data #""])
  (build 0 (hasheq 'chain-id chain-id 'nonce nonce 'gas-price gas-price 'gas-limit gas-limit
                   'to to 'value value 'data data)))

(define (eip2930-transaction #:chain-id chain-id #:nonce nonce #:gas-price gas-price
                             #:gas-limit gas-limit #:to to #:value [value 0] #:data [data #""]
                             #:access-list [access-list '()])
  (build 1 (hasheq 'chain-id chain-id 'nonce nonce 'gas-price gas-price 'gas-limit gas-limit
                   'to to 'value value 'data data 'access-list access-list)))

(define (eip1559-transaction #:chain-id chain-id #:nonce nonce
                             #:max-priority-fee-per-gas priority #:max-fee-per-gas max-fee
                             #:gas-limit gas-limit #:to to #:value [value 0] #:data [data #""]
                             #:access-list [access-list '()])
  (build 2 (hasheq 'chain-id chain-id 'nonce nonce 'max-priority-fee-per-gas priority
                   'max-fee-per-gas max-fee 'gas-limit gas-limit 'to to 'value value 'data data
                   'access-list access-list)))

(define (build type given)
  (define fields
    (for/hasheq ([name (in-list (stored-fields type))])
      (values name (normalise (hash-ref field-kinds name) (hash-ref given name)))))
  (define payload
    (for/list ([name (in-list (unsigned-fields type))])
      (field->item (hash-ref field-kinds name) (hash-ref fields name))))
  (unsigned-transaction type fields payload
                        (bytes->immutable-bytes
                         (encode-unsigned type (hash-ref fields 'chain-id) payload))))

(define (normalise kind value)
  (match kind
    ['to (stored-address value)]
    ['data (bytes->immutable-bytes value)]
    ['access-list (map stored-access-entry value)]
    [_ value]))

(define (stored-address address)
  (and address (string->immutable-string (address->checksum-string address))))

(define (stored-access-entry entry)
  (match-define (cons address keys) entry)
  (cons (stored-address address) (map bytes->immutable-bytes keys)))

(define (field->item kind value)
  (match kind
    [(or 'u64 'u256) (natural->rlp-scalar value)]
    ['to (if value (hex->bytes value) #"")]
    ['data value]
    ['access-list
     (for/list ([entry (in-list value)])
       (match-define (cons address keys) entry)
       (list (hex->bytes address) keys))]))

(define (transaction-field tx name)
  (hash-ref (transaction-fields tx) name))

(define (transaction-chain-id tx)
  (transaction-field tx 'chain-id))

(define (transaction-nonce tx)
  (transaction-field tx 'nonce))

(define (transaction-gas-price tx)
  (transaction-field tx 'gas-price))

(define (transaction-max-priority-fee-per-gas tx)
  (transaction-field tx 'max-priority-fee-per-gas))

(define (transaction-max-fee-per-gas tx)
  (transaction-field tx 'max-fee-per-gas))

(define (transaction-gas-limit tx)
  (transaction-field tx 'gas-limit))

(define (transaction-to tx)
  (transaction-field tx 'to))

(define (transaction-value tx)
  (transaction-field tx 'value))

(define (transaction-data tx)
  (transaction-field tx 'data))

(define (transaction-access-list tx)
  (transaction-field tx 'access-list))

(define (envelope type items)
  (define encoded (rlp-encode items))
  (if (zero? type) encoded (bytes-append (bytes type) encoded)))

(define (encode-unsigned type chain-id payload)
  (envelope type (if (and (zero? type) chain-id)
                     (append payload (list (natural->rlp-scalar chain-id) #"" #""))
                     payload)))

(define (encode-signed tx sig)
  (match-define (signature r s y-parity) sig)
  (define type (transaction-type tx))
  (define chain-id (transaction-chain-id tx))
  (define v
    (cond
      [(positive? type) y-parity]
      [chain-id (+ 35 (* 2 chain-id) y-parity)]
      [else (+ 27 y-parity)]))
  (envelope type (append (transaction-payload tx) (map natural->rlp-scalar (list v r s)))))

(define (transaction->bytes tx)
  (bytes-copy (transaction-preimage tx)))

(define (transaction-signing-hash tx)
  (keccak256 (transaction-preimage tx)))

(define (sign-transaction key tx)
  (define digest (transaction-signing-hash tx))
  (attach-signature/digest 'sign-transaction tx (sign-digest/who 'sign-transaction key digest)
                           digest))

(define attach-signature
  (procedure-rename
   (lambda (tx sig)
     (attach-signature/digest 'signed-transaction tx sig (transaction-signing-hash tx)))
   'signed-transaction))

(define-module-boundary-contract checked-signed-transaction attach-signature
  (-> transaction? signature? signed-transaction?)
  #:name-for-blame signed-transaction)

(define-match-expander signed-transaction
  (syntax-parser
    [(_ tx sig) #'(signed-struct tx sig _ _ _)])
  (syntax-parser
    [_:id #'checked-signed-transaction]
    [(_ . args) #'(checked-signed-transaction . args)]))

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

(define (transaction->jsexpr tx #:from from)
  (define type (transaction-type tx))
  (for/fold ([json (hasheq 'type (natural->hex type) 'from (address->checksum-string from))])
            ([name (in-list (stored-fields type))])
    (define value (transaction-field tx name))
    (if value
        (hash-set json (json-key name) (field->json (hash-ref field-kinds name) value))
        json)))

(define (field->json kind value)
  (match kind
    [(or 'u64 'u256) (natural->hex value)]
    ['to value]
    ['data (bytes->hex value)]
    ['access-list (map access-entry->json value)]))

(define (access-entry->json entry)
  (match-define (cons address keys) entry)
  (hasheq 'address address 'storageKeys (map bytes->hex keys)))

(define (transaction-difference a b)
  (define type (transaction-type a))
  (if (eqv? type (transaction-type b))
      (for/first ([name (in-list (stored-fields type))]
                  #:unless (equal? (transaction-field a name) (transaction-field b name)))
        name)
      'type))

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
  (define wire (read-fields who type (rlp-decode/who who bs (if (zero? type) 0 1))))
  (define sig (wire->signature wire))
  (define priority (hash-ref wire 'max-priority-fee-per-gas #f))
  (cond
    [(and priority (> priority (hash-ref wire 'max-fee-per-gas)))
     (raise-raw who 'range 'max-priority-fee-per-gas
                "max-priority-fee-per-gas is more than max-fee-per-gas")]
    [(not (signature-low-s? sig))
     (raise-raw who 'high-s 's "s is more than half the curve order (EIP-2)")]
    [else (seal-wire who type wire sig)]))

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

(define (read-fields who type decoded)
  (for/hasheq ([name (in-list (hash-ref wire-fields type))]
               [item (in-list (wire-items who type decoded))])
    (values name (read-field who name item))))

(define (wire-items who type decoded)
  (define expected (length (hash-ref wire-fields type)))
  (define described (if (zero? type) "a legacy transaction" (format "a type ~a transaction" type)))
  (match decoded
    [(? bytes?) (raise-raw who 'shape #f (format "~a is a list, not a byte string" described))]
    [_ #:when (= (length decoded) expected) decoded]
    [_ (raise-raw who 'shape #f
                  (format "~a has ~a fields, not ~a" described expected (length decoded)))]))

(define (read-field who name item)
  (match* ((hash-ref field-kinds name) item)
    [('access-list _) (read-access-list who item)]
    [(_ (? list?)) (raise-raw who 'shape name (format "~a is a list, not a byte string" name))]
    [('data _) item]
    [('to _) (read-to who item)]
    [(_ (regexp #rx#"^\0")) (raise-raw who 'leading-zero name (format "in ~a" name))]
    [(kind _) (read-scalar who name kind item)]))

(define (wire->signature wire)
  (match wire
    [(hash-table ('v legacy-v) ('r r) ('s s) _ ...) (signature r s (v-y-parity legacy-v))]
    [(hash-table ('y-parity y-parity) ('r r) ('s s) _ ...) (signature r s y-parity)]))

(define (wire-chain-id wire)
  (match wire
    [(hash-table ('v legacy-v) _ ...) (v-chain-id legacy-v)]
    [(hash-table ('chain-id chain-id) _ ...) chain-id]))

(define (seal-wire who type wire sig)
  (define tx (build type (hash-set wire 'chain-id (wire-chain-id wire))))
  (define sender (recover-address sig (transaction-signing-hash tx)))
  (if sender
      (seal tx sig sender)
      (raise-raw who 'no-sender #f "the signature recovers no address")))

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
  (hasheq 'u64 (list 8 exact-nonnegative-integer? "~a is 2^64 or more")
          'u256 (list 32 exact-nonnegative-integer? "~a is 2^256 or more")
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
    [20 (bytes->hex item)]
    [size (raise-raw who 'length 'to (format "to has ~a bytes, not 0 or 20" size))]))

(define access-entry?
  (match-lambda
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
    [else (cons (bytes->hex address) keys)]))
