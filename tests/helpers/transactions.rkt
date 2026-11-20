#lang racket/base

(require racket/list
         racket/match
         rackunit
         (only-in racket/string string-join)
         (only-in web3/account hex-string->secret-key secp256k1-order signature)
         (only-in web3/rlp natural->rlp-scalar rlp-decode rlp-encode)
         (only-in web3/rpc bytes->hex-string hex-string->bytes hex-string->number)
         web3/transaction
         (only-in (submod web3/transaction internal) wire-fields)
         (only-in web3/units ether->wei gwei->wei)
         (only-in web3/private/hex bytes->natural)
         (only-in "../abi-gen.rkt" random-address random-bytes)
         (only-in "../util.rkt" anvil-bob pick zero-address)
         (only-in web3/address address->checksum-string)
         (only-in "accounts.rkt" anvil-keys random-natural))

(provide base-names
         bases
         check-node-fields
         edge-transactions
         edit-raw
         example-eip1559
         example-key
         example-legacy
         example-transactions
         high-s-twin
         hex-chunks
         model-raw
         model-unsigned
         nick-raw
         no-point-r
         random-partial
         random-transaction
         slot-1
         transaction-fields
         transaction-kind
         transaction-of)

(define (hex-chunks . chunks)
  (hex-string->bytes (string-append "0x" (string-join chunks ""))))

(define example-key
  (match anvil-keys
    [(cons key-hex _) (hex-string->secret-key key-hex)]))

(define slot-1 (bytes-append (make-bytes 31 0) (bytes 1)))

(define example-transactions
  (list (legacy-transaction #:chain-id 31337 #:nonce 0 #:gas-price (gwei->wei 30)
                            #:gas-limit 21000 #:to anvil-bob #:value (ether->wei "1.5"))
        (eip2930-transaction #:chain-id 31337 #:nonce 0 #:gas-price (gwei->wei 30)
                             #:gas-limit 30000 #:to anvil-bob #:value (ether->wei "1.5")
                             #:access-list (list (list anvil-bob slot-1)))
        (eip1559-transaction #:chain-id 31337 #:nonce 0
                             #:max-priority-fee-per-gas (gwei->wei 2)
                             #:max-fee-per-gas (gwei->wei 30)
                             #:gas-limit 21000 #:to anvil-bob #:value (ether->wei "1.5"))))

(match-define (list example-legacy _ example-eip1559) example-transactions)

(define nick-raw
  (hex-chunks "f8a58085174876e800830186a08080b853604580600e600039806000f350fe7fffffffffff"
              "ffffffffffffffffffffffffffffffffffffffffffffffffffffe03601600081602082378035"
              "828234f58015156039578182fd5b8082525050506014600cf31ba0222222222222222222222222"
              "2222222222222222222222222222222222222222a0222222222222222222222222222222222222"
              "2222222222222222222222222222"))

(define base-names '(legacy eip2930 eip1559 nick))

(define bases
  (append (for/list ([tx (in-list example-transactions)])
            (signed-transaction->bytes (sign-transaction example-key tx)))
          (list nick-raw)))

(define (model-fields tx)
  (define to (match (transaction-to tx) [#f #""] [address (hex-string->bytes address)]))
  (define tail
    (list to (natural->rlp-scalar (transaction-value tx)) (transaction-data tx)))
  (define access-list
    (match (transaction-type tx)
      [0 '()]
      [_ (for/list ([entry (in-list (transaction-access-list tx))])
           (match-define (cons address keys) entry)
           (list (hex-string->bytes address) keys))]))
  (define (scalars . ns)
    (map natural->rlp-scalar ns))
  (match (transaction-type tx)
    [0 (append (scalars (transaction-nonce tx) (transaction-gas-price tx)
                        (transaction-gas-limit tx))
               tail)]
    [1 (append (scalars (transaction-chain-id tx) (transaction-nonce tx)
                        (transaction-gas-price tx) (transaction-gas-limit tx))
               tail
               (list access-list))]
    [2 (append (scalars (transaction-chain-id tx) (transaction-nonce tx)
                        (transaction-max-priority-fee-per-gas tx)
                        (transaction-max-fee-per-gas tx) (transaction-gas-limit tx))
               tail
               (list access-list))]))

(define (model-unsigned tx)
  (define chain-id (transaction-chain-id tx))
  (match (transaction-type tx)
    [0 #:when chain-id
       (rlp-encode (append (model-fields tx) (list (natural->rlp-scalar chain-id) #"" #"")))]
    [0 (rlp-encode (model-fields tx))]
    [type (bytes-append (bytes type) (rlp-encode (model-fields tx)))]))

(define (model-raw tx sig)
  (match-define (signature r s y) sig)
  (define chain-id (transaction-chain-id tx))
  (define (signed v)
    (rlp-encode (append (model-fields tx) (map natural->rlp-scalar (list v r s)))))
  (match (transaction-type tx)
    [0 #:when chain-id (signed (+ 35 (* 2 chain-id) y))]
    [0 (signed (+ 27 y))]
    [type (bytes-append (bytes type) (signed y))]))

(define u64-max (sub1 (expt 2 64)))
(define u256-max (sub1 (expt 2 256)))

(define (random-u64 prng)
  (if (zero? (random 2 prng))
      (pick prng (list 0 1 127 128 255 256 u64-max))
      (random-natural prng 8)))

(define (random-u256 prng [max-bytes 32])
  (if (zero? (random 2 prng))
      (pick prng (list 0 1 (sub1 (expt 2 (* 8 max-bytes)))))
      (random-natural prng (random (add1 max-bytes) prng))))

(define (random-chain-id prng)
  (if (zero? (random 2 prng))
      (pick prng (list 1 31337 0 u64-max))
      (random-natural prng 8)))

(define (random-to prng)
  (match (random 20 prng)
    [(or 0 1 2) #f]
    [3 zero-address]
    [4 (string-append "0x0000" (substring (random-address prng) 6))]
    [_ (random-address prng)]))

(define (random-data prng)
  (define size
    (if (zero? (random 2 prng)) (pick prng '(0 1 55 56 255 256 1024)) (random 301 prng)))
  (if (zero? (random 10 prng))
      (bytes (random #x80 prng))
      (random-bytes prng size)))

(define (random-access-list prng)
  (for/fold ([entries '()] #:result (reverse entries))
            ([_ (in-range (random 4 prng))])
    (define reused (and (pair? entries) (zero? (random 3 prng)) (pick prng entries)))
    (define address
      (match reused
        [(cons used _) used]
        [#f (random-address prng)]))
    (cons (cons address (for/list ([_ (in-range (random 4 prng))]) (random-bytes prng 32)))
          entries)))

(define (random-transaction prng kind #:fee-bytes [fee-bytes 32])
  (match-define (list priority max-fee)
    (sort (list (random-u256 prng fee-bytes) (random-u256 prng fee-bytes)) <))
  (transaction-of kind
                  (hasheq 'chain-id (random-chain-id prng)
                          'nonce (random-u64 prng)
                          'gas-price (random-u256 prng fee-bytes)
                          'max-priority-fee-per-gas priority
                          'max-fee-per-gas max-fee
                          'gas-limit (random-u64 prng)
                          'to (random-to prng)
                          'value (random-u256 prng)
                          'data (random-data prng)
                          'access-list (random-access-list prng))))

(define defaults
  (hasheq 'chain-id 1 'nonce 0 'gas-price 0 'max-priority-fee-per-gas 0 'max-fee-per-gas 0
          'gas-limit 21000 'to anvil-bob 'value 0 'data #"" 'access-list '()))

(define kind-fields
  '((pre-155 0 (chain-id #f) nonce gas-price gas-limit to value data)
    (legacy 0 chain-id nonce gas-price gas-limit to value data)
    (eip2930 1 chain-id nonce gas-price gas-limit to value data access-list)
    (eip1559 2 chain-id nonce max-priority-fee-per-gas max-fee-per-gas gas-limit to value data
             access-list)))

(define constructors
  (hasheqv 0 legacy-transaction 1 eip2930-transaction 2 eip1559-transaction))

(define accessors
  (hasheq 'chain-id transaction-chain-id
          'nonce transaction-nonce
          'gas-price transaction-gas-price
          'max-priority-fee-per-gas transaction-max-priority-fee-per-gas
          'max-fee-per-gas transaction-max-fee-per-gas
          'gas-limit transaction-gas-limit
          'to transaction-to
          'value transaction-value
          'data transaction-data
          'access-list transaction-access-list))

(define (field-name field)
  (match field
    [(list name _) name]
    [name name]))

(define (transaction-of kind given)
  (match-define (list _ type fields ...) (assq kind kind-fields))
  (define arguments
    (for/hash ([field (in-list fields)])
      (match field
        [(list name fixed) (values (string->keyword (symbol->string name)) fixed)]
        [name (values (string->keyword (symbol->string name))
                      (hash-ref given name (lambda () (hash-ref defaults name))))])))
  (define keywords (sort (hash-keys arguments) keyword<?))
  (keyword-apply (hash-ref constructors type)
                 keywords
                 (for/list ([keyword (in-list keywords)]) (hash-ref arguments keyword))
                 '()))

(define (transaction-kind tx)
  (match (transaction-type tx)
    [0 (if (transaction-chain-id tx) 'legacy 'pre-155)]
    [1 'eip2930]
    [2 'eip1559]))

(define (transaction-fields tx)
  (match-define (list _ _ fields ...) (assq (transaction-kind tx) kind-fields))
  (for/hasheq ([name (in-list (map field-name fields))])
    (values name ((hash-ref accessors name) tx))))

(define edge-rows
  `((pre-155) (legacy) (eip2930) (eip1559)
    (pre-155 nonce ,u64-max gas-price ,u256-max gas-limit ,u64-max value ,u256-max)
    (legacy chain-id 0) (legacy chain-id ,u64-max) (legacy chain-id 31337 to #f)
    (legacy nonce 127) (legacy nonce 128) (legacy nonce 255) (legacy nonce 256)
    (eip2930 chain-id 0) (eip2930 chain-id ,u64-max gas-price ,u256-max value ,u256-max)
    (eip2930 to #f data ,(make-bytes 1024 #x60))
    (eip2930 access-list ((,anvil-bob) (,anvil-bob ,(make-bytes 32 1) ,(make-bytes 32 1))))
    (eip2930 to ,zero-address access-list ((,zero-address ,(make-bytes 32 0))))
    (eip1559 chain-id 0) (eip1559 chain-id ,u64-max nonce ,u64-max gas-limit ,u64-max)
    (eip1559 max-priority-fee-per-gas ,u256-max max-fee-per-gas ,u256-max)
    (eip1559 max-priority-fee-per-gas 0 max-fee-per-gas ,u256-max value ,u256-max)
    (eip1559 max-priority-fee-per-gas 1 max-fee-per-gas 1)
    (eip1559 data #"\x7f") (eip1559 data #"\x80") (eip1559 data #"\0")
    (eip1559 data ,(make-bytes 55 1)) (eip1559 data ,(make-bytes 56 1))
    (eip1559 data ,(make-bytes 255 1)) (eip1559 data ,(make-bytes 256 1))
    (eip1559 to #f data #"\x60\x80"
             access-list ((,anvil-bob) (,zero-address) (,anvil-bob ,(make-bytes 32 255))))))

(define edge-transactions
  (for/list ([row (in-list edge-rows)])
    (match-define (cons kind settings) row)
    (transaction-of kind (apply hasheq settings))))

(define (decode-raw raw)
  (match (bytes-ref raw 0)
    [(or 1 2) (values (bytes-ref raw 0) (rlp-decode (subbytes raw 1)))]
    [_ (values 0 (rlp-decode raw))]))

(define (encode-raw type items)
  (define encoded (rlp-encode items))
  (if (zero? type) encoded (bytes-append (bytes type) encoded)))

(define (field-index type field)
  (index-of (hash-ref wire-fields type) field))

(define (edit-item value)
  (match value
    [(? exact-integer?) (natural->rlp-scalar value)]
    [(? string?) (hex-string->bytes value)]
    [(? bytes?) value]
    [(? list?) (map edit-item value)]))

(define (list-header size)
  (if (< size 56)
      (bytes (+ #xc0 size))
      (let ([length-bytes (natural->rlp-scalar size)])
        (bytes-append (bytes (+ #xf7 (bytes-length length-bytes))) length-bytes))))

(define (splice-field type items field encoding)
  (define index (field-index type field))
  (define payload
    (apply bytes-append
           (for/list ([item (in-list items)] [i (in-naturals)])
             (if (= i index) encoding (rlp-encode item)))))
  (define body (bytes-append (list-header (bytes-length payload)) payload))
  (if (zero? type) body (bytes-append (bytes type) body)))

(define (edit-raw base edit)
  (define-values (type items) (decode-raw base))
  (match edit
    [`(seq ,first-edit . ,more) (for/fold ([raw (edit-raw base first-edit)]) ([e (in-list more)])
                                  (edit-raw raw e))]
    [`(set ,field ,value) (encode-raw type (list-set items (field-index type field)
                                                     (edit-item value)))]
    [`(splice ,field ,hex) (splice-field type items field (hex-string->bytes hex))]
    [`(count ,k) (encode-raw type (if (< k (length items))
                                      (take items k)
                                      (append items (make-list (- k (length items)) #""))))]
    [`(high-s) (high-s-twin base)]
    [`(type ,t) (bytes-append (bytes t) (if (zero? type) base (subbytes base 1)))]
    [`(raw ,hex) (hex-string->bytes hex)]))

(define (high-s-twin raw)
  (define-values (type items) (decode-raw raw))
  (match-define (list v r s) (map bytes->natural (take-right items 3)))
  (define flipped-v
    (cond
      [(positive? type) (- 1 v)]
      [(< v 35) (- 55 v)]
      [(even? (- v 35)) (add1 v)]
      [else (sub1 v)]))
  (encode-raw type (append (drop-right items 3)
                           (map natural->rlp-scalar
                                (list flipped-v r (- secp256k1-order s))))))

(define field-prime (- (expt 2 256) (expt 2 32) 977))

(define (modular-power base exponent modulus)
  (let loop ([base (modulo base modulus)] [exponent exponent] [result 1])
    (cond
      [(zero? exponent) result]
      [(odd? exponent)
       (loop (modulo (* base base) modulus) (quotient exponent 2) (modulo (* result base) modulus))]
      [else (loop (modulo (* base base) modulus) (quotient exponent 2) result)])))

(define (no-point-r)
  (for/first ([r (in-naturals 1)]
              #:unless (= 1 (modular-power (+ (* r r r) 7) (quotient (sub1 field-prime) 2)
                                           field-prime)))
    r))

(define (quantity text)
  (hex-string->number text))

(define (check-node-fields stx node)
  (define tx (signed-transaction-transaction stx))
  (match-define (signature r s y-parity) (signed-transaction-signature stx))
  (define (field key) (hash-ref node key 'null))
  (define name (format "~s" stx))
  (check-equal? (string-downcase (field 'from)) (string-downcase (signed-transaction-sender stx))
                name)
  (check-equal? (field 'hash) (signed-transaction-hash stx) name)
  (check-equal? (quantity (field 'type)) (transaction-type tx) name)
  (check-equal? (quantity (field 'nonce)) (transaction-nonce tx) name)
  (check-equal? (match (field 'chainId) ['null #f] [id (quantity id)]) (transaction-chain-id tx)
                name)
  (check-equal? (field 'to) (match (transaction-to tx) [#f 'null] [to (string-downcase to)]) name)
  (check-equal? (quantity (field 'value)) (transaction-value tx) name)
  (check-equal? (field 'input) (bytes->hex-string (transaction-data tx)) name)
  (check-equal? (quantity (field 'gas)) (transaction-gas-limit tx) name)
  (check-equal? (quantity (field 'r)) r name)
  (check-equal? (quantity (field 's)) s name)
  (match (transaction-type tx)
    [0
     (check-equal? (quantity (field 'gasPrice)) (transaction-gas-price tx) name)
     (check-equal? (quantity (field 'v))
                   (match (transaction-chain-id tx)
                     [#f (+ 27 y-parity)]
                     [chain-id (+ 35 (* 2 chain-id) y-parity)])
                   name)]
    [type
     (check-equal? (quantity (field 'yParity)) y-parity name)
     (check-equal? (field 'accessList)
                   (for/list ([entry (in-list (transaction-access-list tx))])
                     (match-define (cons address keys) entry)
                     (hasheq 'address (string-downcase address)
                             'storageKeys (map bytes->hex-string keys)))
                   name)
     (if (= type 1)
         (check-equal? (quantity (field 'gasPrice)) (transaction-gas-price tx) name)
         (check-equal? (list (quantity (field 'maxFeePerGas))
                             (quantity (field 'maxPriorityFeePerGas)))
                       (list (transaction-max-fee-per-gas tx)
                             (transaction-max-priority-fee-per-gas tx))
                       name))]))

(define (maybe prng kw make)
  (if (zero? (random 2 prng)) '() (list kw (make))))

(define (random-checksummed-access-list prng)
  (for/list ([entry (in-list (random-access-list prng))])
    (match-define (cons address keys) entry)
    (cons (address->checksum-string address) keys)))

(define (random-partial prng)
  (define type (random 3 prng))
  (define cap (+ (expt 10 9) (random-natural prng 8)))
  (define (access-list)
    (maybe prng '#:access-list (lambda () (random-checksummed-access-list prng))))
  (append
   (maybe prng '#:type (lambda () type))
   (maybe prng '#:nonce (lambda () (random-u64 prng)))
   (maybe prng '#:gas-limit (lambda () (random-u64 prng)))
   (maybe prng '#:chain-id (lambda () 31337))
   (maybe prng '#:value (lambda () (random-u256 prng)))
   (maybe prng '#:data (lambda () (random-data prng)))
   (match type
     [0 (maybe prng '#:gas-price (lambda () (random-u256 prng)))]
     [1 (append (maybe prng '#:gas-price (lambda () (random-u256 prng))) (access-list))]
     [2 (append (maybe prng '#:max-priority-fee-per-gas
                       (lambda () (modulo (random-natural prng 8) (add1 cap))))
                (maybe prng '#:max-fee-per-gas (lambda () cap))
                (access-list))])))
