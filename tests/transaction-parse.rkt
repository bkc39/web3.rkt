#lang racket/base

(module+ test
  (require (only-in racket/contract flat-contract-predicate)
           racket/list
           racket/match
           rackunit
           (only-in web3/account exn:fail:web3:signature? secp256k1-order)
           (only-in web3/rlp exn:fail:web3:rlp-offset exn:fail:web3:rlp-reason exn:fail:web3:rlp?)
           (only-in web3/rpc bytes->hex-string exn:fail:web3?)
           web3/transaction
           (only-in (submod web3/transaction internal)
                    bytes->signed-transaction/who
                    raw-transaction-reason/c
                    wire-fields)
           (only-in "abi-gen.rkt" random-bytes)
           "bounded.rkt"
           (only-in "helpers/accounts.rkt" random-key)
           (only-in "helpers/rlp.rkt" input-name mutations)
           "helpers/transactions.rkt"
           (only-in "util.rkt" anvil-bob pick seeded))

  (define reason? (flat-contract-predicate raw-transaction-reason/c))

  (define fields
    '(#f chain-id nonce gas-price max-priority-fee-per-gas max-fee-per-gas gas-limit to value
         data access-list v y-parity r s))

  (define base-raws
    (for/hasheq ([name (in-list base-names)] [raw (in-list bases)])
      (values name raw)))

  (define base-types
    (hasheq 'legacy 0 'eip2930 1 'eip1559 2 'nick 0))

  (define (base-fields base)
    (hash-ref wire-fields (hash-ref base-types base)))

  (define eip1559-raw (hash-ref base-raws 'eip1559))
  (define legacy-raw (hash-ref base-raws 'legacy))
  (define u64-limit (expt 2 64))
  (define u256-limit (expt 2 256))
  (define address-19 (bytes->hex-string (make-bytes 19 1)))
  (define address-21 (bytes->hex-string (make-bytes 21 1)))
  (define key-31 (bytes->hex-string (make-bytes 31 1)))
  (define key-33 (bytes->hex-string (make-bytes 33 1)))
  (define key-32 (bytes->hex-string (make-bytes 32 1)))
  (define bob (bytes->hex-string (make-bytes 20 #x70)))

  (define envelope-rows
    `((empty legacy (raw "0x") raw type #f
             "bytes->signed-transaction: not a transaction: no bytes")
      (type-0 legacy (raw "0x00") raw type #f)
      (type-3 legacy (raw "0x03") raw type #f
              "bytes->signed-transaction: not a transaction: type 3 is not supported")
      (type-4 legacy (raw "0x04") raw type #f)
      (type-7f legacy (raw "0x7f") raw type #f)
      (type-0-body eip1559 (type 0) raw type #f)
      (type-3-body eip1559 (type 3) raw type #f)
      (type-4-body eip1559 (type 4) raw type #f)
      (type-7f-body eip1559 (type #x7f) raw type #f)
      (byte-string legacy (raw "0x80") raw type #f
                   ,(string-append "bytes->signed-transaction: not a transaction: "
                                   "first byte 0x80 is neither a type nor a list"))
      (long-byte-string legacy (raw "0xb8") raw type #f
                        ,(string-append "bytes->signed-transaction: not a transaction: "
                                        "first byte 0xb8 is neither a type nor a list"))
      (long-byte-string-1 legacy (raw "0xb801") raw type #f)
      (type-1-alone legacy (raw "0x01") rlp short 1)
      (type-2-alone legacy (raw "0x02") rlp short 1)
      (type-2-empty-list legacy (raw "0x02c0") raw shape #f
                         ,(string-append "bytes->signed-transaction: malformed transaction: "
                                         "a type 2 transaction has 12 fields, not 0"))
      (type-2-byte-string legacy (raw "0x0280") raw shape #f
                          ,(string-append "bytes->signed-transaction: malformed transaction: "
                                          "a type 2 transaction is a list, not a byte string"))))

  (define count-rows
    `((legacy-8 legacy (count 8) raw shape #f
                ,(string-append "bytes->signed-transaction: malformed transaction: "
                                "a legacy transaction has 9 fields, not 8"))
      (legacy-10 legacy (count 10) raw shape #f)
      (type-1-10 eip2930 (count 10) raw shape #f)
      (type-1-12 eip2930 (count 12) raw shape #f
                 ,(string-append "bytes->signed-transaction: malformed transaction: "
                                 "a type 1 transaction has 11 fields, not 12"))
      (type-2-11 eip1559 (count 11) raw shape #f)
      (type-2-13 eip1559 (count 13) raw shape #f)))

  (define list-field-rows
    (for*/list ([base (in-list base-names)]
                [field (in-list (base-fields base))]
                #:unless (eq? field 'access-list))
      `(list-field ,base (set ,field ()) raw shape ,field
                   ,(format (string-append "bytes->signed-transaction: malformed transaction: "
                                           "~a is a list, not a byte string")
                            field))))

  (define access-list-shape-rows
    (for*/list ([base (in-list '(eip2930 eip1559))]
                [row (in-list
                      `((byte-string "0x"
                                     ,(string-append "bytes->signed-transaction: "
                                                     "malformed transaction: "
                                                     "access-list is a byte string, not a list"))
                        (entry-list (())
                                    ,(string-append "bytes->signed-transaction: "
                                                    "malformed transaction: access-list entry 0 "
                                                    "is not an address and a list of storage keys"))
                        (entry-three-items ((,bob () "0x")))
                        (entry-key-string ((,bob "0x")))
                        (entry-two-lists ((,bob ()) (() ())))))])
      (match row
        [(list name value message)
         `(,name ,base (set access-list ,value) raw shape access-list ,message)]
        [(list name value) `(,name ,base (set access-list ,value) raw shape access-list)])))

  (define scalar-fields
    '(chain-id nonce gas-price max-priority-fee-per-gas max-fee-per-gas gas-limit value v
               y-parity r s))

  (define leading-zero-rows
    (for*/list ([base (in-list base-names)]
                [field (in-list (base-fields base))]
                #:when (memq field scalar-fields)
                [value (in-list '("0x00" "0x0001"))])
      `(leading-zero ,base (set ,field ,value) raw leading-zero ,field
                     ,(format "bytes->signed-transaction: leading zero: in ~a" field))))

  (define single-byte-rows
    '((single-byte eip1559 (splice nonce "0x8100") rlp single-byte 6)
      (single-byte legacy (splice nonce "0x8100") rlp single-byte 2)))

  (define (range-message field)
    (format "bytes->signed-transaction: out of range: ~a"
            (match field
              [(or 'nonce 'gas-limit 'chain-id) (format "~a is 2^64 or more" field)]
              [(or 'r 's) (format "~a is zero or not less than the curve order" field)]
              ['y-parity "y-parity is not 0 or 1"]
              ['v "v is not 27, 28, or 35 + 2 * chain-id + y-parity with chain-id below 2^64"]
              [_ (format "~a is 2^256 or more" field)])))

  (define range-rows
    (append
     (for*/list ([base (in-list base-names)]
                 [field (in-list (base-fields base))]
                 [value (in-list
                         (match field
                           [(or 'nonce 'gas-limit 'chain-id) (list u64-limit)]
                           [(or 'value 'gas-price 'max-priority-fee-per-gas 'max-fee-per-gas)
                            (list u256-limit)]
                           ['y-parity (list 2)]
                           ['v (list 0 1 26 29 30 31 32 33 34 (+ 35 (expt 2 65)))]
                           [(or 'r 's) (list 0 secp256k1-order)]
                           [_ '()]))])
       `(range ,base (set ,field ,value) raw range ,field ,(range-message field)))
     `((priority-above-max eip1559 (set max-priority-fee-per-gas 30000000001)
                           raw range max-priority-fee-per-gas
                           ,(string-append
                             "bytes->signed-transaction: out of range: "
                             "max-priority-fee-per-gas is more than max-fee-per-gas")))))

  (define length-rows
    (append
     (for*/list ([base (in-list base-names)]
                 [size (in-list '(1 19 21))])
       `(to-length ,base (set to ,(bytes->hex-string (make-bytes size 1))) raw length to
                   ,(format "bytes->signed-transaction: wrong length: to has ~a bytes, not 0 or 20"
                            size)))
     (for*/list ([base (in-list '(eip2930 eip1559))]
                 [row (in-list
                       `((((,address-19 ()))
                          ,(string-append "bytes->signed-transaction: wrong length: access-list "
                                          "entry 0 has an address of 19 bytes, not 20"))
                         (((,bob ()) (,address-21 ())))
                         (((,bob (,key-32 ,key-31)))
                          ,(string-append "bytes->signed-transaction: wrong length: access-list "
                                          "entry 0 has a storage key of 31 bytes, not 32"))
                         (((,bob (,key-33))))))])
       (match-define (cons access-list expected) row)
       `(access-list-length ,base (set access-list ,access-list) raw length access-list
                            . ,expected))))

  (define high-s-message
    "bytes->signed-transaction: high s: s is more than half the curve order (EIP-2)")

  (define high-s-rows
    (for/list ([base (in-list base-names)])
      `(high-s ,base (high-s) raw high-s s ,high-s-message)))

  (define no-sender-rows
    (for/list ([base (in-list base-names)])
      `(no-sender ,base (set r ,(no-point-r)) raw no-sender #f
                  "bytes->signed-transaction: no sender: the signature recovers no address")))

  (define (cut-hex raw)
    (bytes->hex-string (subbytes raw 0 (sub1 (bytes-length raw)))))

  (define rlp-rows
    `((cut eip1559 (raw ,(cut-hex eip1559-raw)) rlp short 1
           ,(string-append "bytes->signed-transaction: input too short: "
                           "119 bytes needed, 118 bytes left\n  offset: 1"))
      (appended eip1559 (raw ,(string-append (bytes->hex-string eip1559-raw) "00"))
                rlp trailing 120)
      (appended legacy (raw ,(string-append (bytes->hex-string legacy-raw) "00"))
                rlp trailing 112)))

  (define order-rows
    '((zero-nonce-and-short-to eip1559 (seq (set nonce "0x00") (set to "0x01"))
                               raw leading-zero nonce)
      (count-and-high-s eip1559 (seq (high-s) (count 13)) raw shape #f)
      (zero-r-and-high-s eip1559 (seq (high-s) (set r 0)) raw range r)
      (priority-and-high-s eip1559 (seq (high-s) (set max-priority-fee-per-gas 30000000001))
                           raw range max-priority-fee-per-gas)
      (legacy-zero-r-and-high-s nick (seq (high-s) (set r 0)) raw range r)))

  (define all-rows
    (append envelope-rows count-rows list-field-rows access-list-shape-rows leading-zero-rows
            single-byte-rows range-rows length-rows high-s-rows no-sender-rows rlp-rows
            order-rows))

  (define (row-input row)
    (match-define (list* _ base edit _) row)
    (edit-raw (hash-ref base-raws base) edit))

  (define (fault thunk)
    (with-handlers ([exn:fail? values])
      (thunk)
      #f))

  (define message-pattern
    (pregexp (string-append "^bytes->signed-transaction: (?:not a transaction|malformed transaction"
                            "|leading zero|out of range|wrong length|high s|no sender"
                            "|input too short|non-canonical single byte|non-canonical size"
                            "|trailing bytes): ")))

  (define (check-row row)
    (match-define (list* name base edit class reason field message) row)
    (define input (row-input row))
    (define label (format "~a ~a ~s: ~a" name base edit (input-name input)))
    (define e (fault (lambda () (bytes->signed-transaction input))))
    (match class
      ['rlp
       (check-pred exn:fail:web3:rlp? e label)
       (when (exn:fail:web3:rlp? e)
         (check-equal? (exn:fail:web3:rlp-reason e) reason label)
         (check-equal? (exn:fail:web3:rlp-offset e) field label))]
      ['raw
       (check-pred exn:fail:web3:raw-transaction? e label)
       (when (exn:fail:web3:raw-transaction? e)
         (check-equal? (exn:fail:web3:raw-transaction-reason e) reason label)
         (check-equal? (exn:fail:web3:raw-transaction-field e) field label))])
    (when (exn:fail:web3? e)
      (check-regexp-match message-pattern (exn-message e) label)
      (check-true (< (string-length (exn-message e)) 200) label))
    (match message
      [(list text) (check-equal? (and e (exn-message e)) text label)]
      ['() (void)]))

  (test-case "P1: the first byte decides legacy, typed or refused"
    (for-each check-row envelope-rows))

  (test-case "P2: the field count, and lists where byte strings belong"
    (for-each check-row (append count-rows list-field-rows access-list-shape-rows)))

  (test-case "P3: scalars with a leading zero"
    (for-each check-row (append leading-zero-rows single-byte-rows)))

  (test-case "P4: values out of range"
    (for-each check-row range-rows))

  (test-case "P5: addresses and storage keys of the wrong length"
    (for-each check-row length-rows))

  (test-case "P6: a high s is refused"
    (for-each check-row high-s-rows)
    (check-equal? (bytes->hex-string (edit-raw eip1559-raw '(high-s)))
                  (string-append
                   "0x02f875827a698084773594008506fc23ac008252089470997970c51812dc3a010c7d01b50e0d"
                   "17dc79c88814d1120d7b16000080c080a0e4283c98410a9ee00d296ef6d31b47e6326541bf89e1"
                   "4640e3ff09b145517165a08f1e195a1d5848e50d009d87413417209344abf35e5e8ad07c4be617"
                   "bcfe2a22")))

  (test-case "P7: a signature that recovers no address is refused"
    (for-each check-row no-sender-rows))

  (test-case "P8: RLP faults keep offsets into the whole input"
    (for-each check-row rlp-rows))

  (test-case "P9: of two faults, the first in reading order is reported"
    (for-each check-row order-rows))

  (test-case "P10, P12: every refusal is a web3 error with a short message, never a signature error"
    (for ([row (in-list all-rows)])
      (define e (fault (lambda () (bytes->signed-transaction (row-input row)))))
      (check-true (or (exn:fail:web3:rlp? e) (exn:fail:web3:raw-transaction? e)) (format "~s" row))
      (check-false (exn:fail:web3:signature? e))))

  (test-case "P11: the internal entry point raises under the caller's name"
    (for ([input (list (bytes 3)
                       (edit-raw eip1559-raw `(set nonce ,u64-limit))
                       (subbytes eip1559-raw 0 10))])
      (define (parse)
        (bytes->signed-transaction/who 'eth-sign-transaction input))
      (check-regexp-match #rx"^eth-sign-transaction: " (exn-message (fault parse)))))

  (define fuzz-corpus
    (let ([prng (seeded 3 1 4 1 5 9)])
      (define signed
        (for/list ([_ (in-range 200)])
          (signed-transaction->bytes
           (sign-transaction (random-key prng)
                             (random-transaction prng (pick prng '(pre-155 legacy eip2930
                                                                            eip1559)))))))
      (append
       (for*/list ([raw (in-list (append bases signed))]
                   [_ (in-range 20)]
                   [mutation (in-list (mutations prng raw))])
         mutation)
       (for*/list ([raw (in-list bases)]
                   [n (in-range (bytes-length raw))])
         (subbytes raw 0 n))
       (for/list ([i (in-range 2000)])
         (define bs (random-bytes prng (random 301 prng)))
         (if (even? i) (bytes-append (bytes (add1 (random 2 prng))) bs) bs)))))

  (test-case "H1: any input parses within bounds, or raises an RLP or raw-transaction error"
    (for ([bs (in-list fuzz-corpus)])
      (define name (input-name bs))
      (match (call-bounded (lambda () (bytes->signed-transaction bs)))
        [(list 'ok stx) (check-equal? (signed-transaction->bytes stx) bs name)]
        [(list 'raised (? exn:fail:web3:rlp?)) (void)]
        [(list 'raised (? exn:fail:web3:raw-transaction? e))
         (check-pred reason? (exn:fail:web3:raw-transaction-reason e) name)
         (check-not-false (memq (exn:fail:web3:raw-transaction-field e) fields) name)]
        [other (fail (format "~a: ~e" name other))])))

  (test-case "H2: a megabyte of data, and a claimed length past the input"
    (define tx (transaction-of 'eip1559 (hasheq 'data (make-bytes (* 1024 1024) 7))))
    (define key example-key)
    (match (call-bounded (lambda ()
                           (define raw (signed-transaction->bytes (sign-transaction key tx)))
                           (equal? (signed-transaction->bytes (bytes->signed-transaction raw)) raw))
                         #:megabytes 128)
      [(list 'ok result) (check-true result)]
      [other (fail (format "~e" other))])
    (match (call-bounded (lambda () (bytes->signed-transaction (bytes 2 #xbb #xff #xff #xff #xff))))
      [(list 'raised (? exn:fail:web3:rlp? e))
       (check-equal? (exn:fail:web3:rlp-reason e) 'short)
       (check-equal? (exn:fail:web3:rlp-offset e) 1)]
      [other (fail (format "~e" other))])))
