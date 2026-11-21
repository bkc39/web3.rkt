#lang racket/base

(module+ test
  (require racket/format
           (only-in racket/list make-list)
           (only-in racket/match match match-define)
           (only-in racket/string string-join)
           rackunit
           (only-in web3/address checksum-address?)
           (only-in web3/eth/log jsexpr->log log->jsexpr)
           (only-in (submod web3/eth/log internal) log-fields)
           (only-in web3/eth/receipt jsexpr->receipt)
           (only-in web3/eth/transaction-info jsexpr->transaction-info)
           (only-in (submod web3/eth/transaction-info internal) transaction-info-fields)
           (only-in (submod web3/eth/receipt internal) receipt-fields)
           (only-in web3/rpc exn:fail:web3?)
           web3/private/record
           "helpers/results.rkt"
           "util.rkt")

  (define (random-bytes prng size)
    (apply bytes (for/list ([_ (in-range size)]) (random 256 prng))))

  (define (random-natural prng bits)
    (modulo (for/fold ([n 0]) ([_ (in-range (quotient (+ bits 7) 8))])
              (+ (* n 256) (random 256 prng)))
            (expt 2 bits)))

  (define (hex bs)
    (string-append "0x" (string-join (for/list ([b (in-bytes bs)])
                                       (~r b #:base 16 #:min-width 2 #:pad-string "0"))
                                     "")))

  (define (quantity n)
    (string-append "0x" (~r n #:base 16)))

  (define (mixed-case prng s)
    (list->string (for/list ([c (in-string s)])
                    (if (zero? (random 2 prng)) (char-upcase c) c))))

  (define (random-hash prng)
    (string-append "0x" (mixed-case prng (substring (hex (random-bytes prng 32)) 2))))

  (define (random-max-or-natural prng bits)
    (if (zero? (random 4 prng)) (sub1 (expt 2 bits)) (random-natural prng bits)))

  (define ((natural-sample bits) prng)
    (define n (random-max-or-natural prng bits))
    (list (quantity n) (lambda (v) (eqv? v n))))

  (define (lenient-sample prng)
    (define n (random-max-or-natural prng 64))
    (list (if (zero? (random 2 prng)) n (quantity n)) (lambda (v) (eqv? v n))))

  (define (boolean-sample prng)
    (define b (zero? (random 2 prng)))
    (list b (lambda (v) (eq? v b))))

  (define (address-sample prng)
    (define json (hex (random-bytes prng 20)))
    (list json (lambda (v) (and (checksum-address? v) (equal? (string-downcase v) json)))))

  (define (hash-sample prng)
    (define json (random-hash prng))
    (list json (lambda (v) (equal? v (string-downcase json)))))

  (define ((data-sample size) prng)
    (define bs (random-bytes prng (or size (random 70 prng))))
    (list (hex bs) (lambda (v) (equal? v bs))))

  (define ((hashes-sample most) prng)
    (define json (for/list ([_ (in-range (random (add1 most) prng))]) (random-hash prng)))
    (list json (lambda (v) (equal? v (map string-downcase json)))))

  (define (access-list-sample prng)
    (define entries
      (for/list ([_ (in-range (random 3 prng))])
        (cons (hex (random-bytes prng 20))
              (for/list ([_ (in-range (random 3 prng))]) (random-bytes prng 32)))))
    (list (for/list ([entry (in-list entries)])
            (match-define (cons address keys) entry)
            (hasheq 'address address 'storageKeys (map hex keys)))
          (lambda (v)
            (equal? (for/list ([entry (in-list v)])
                      (match-define (cons address keys) entry)
                      (cons (string-downcase address) keys))
                    entries))))

  (define (logs-sample prng)
    (define json (make-list (random 4 prng) log-json))
    (list json (lambda (v) (equal? (map log->jsexpr v) json))))

  (define samples
    (hash 'uint64 (natural-sample 64)
          'lenient-uint64 lenient-sample
          'quantity (natural-sample 256)
          'byte (natural-sample 8)
          'bit (natural-sample 1)
          'boolean boolean-sample
          'address address-sample
          'hash hash-sample
          'data (data-sample #f)
          '(fixed-data 256) (data-sample 256)
          '(list hash 4) (hashes-sample 4)
          '(list hash +inf.0) (hashes-sample 6)
          'access-list access-list-sample
          '(list (record log?) +inf.0) logs-sample))

  (define hostiles
    (hash 'uint64 `(42 "0xzz" (1) "0x" #t ,(quantity (expt 16 64)) ,(quantity (expt 2 64)))
          'lenient-uint64 `(-1 1.5 ,(expt 2 64) "0xzz" (1) "0x" #t ,(quantity (expt 2 64)))
          'quantity `(42 "0xzz" (1) "0x" #t ,(quantity (expt 16 64)))
          'byte `(42 "0xzz" (1) "0x" #t ,(quantity 256) ,(quantity (expt 2 64)))
          'bit `(42 "0xzz" (1) "0x" #t "0x2" ,(quantity (expt 2 64)))
          'boolean '(42 "0xzz" (1) "0x")
          'address '(42 "0xzz" (1) "0x" #t "0x12")
          'hash '(42 "0xzz" (1) "0x" #t "0x12")
          'data '(42 "0xzz" (1) #t "0x1")
          '(fixed-data 256) `(42 "0xzz" (1) "0x" #t ,(hex (make-bytes 255 0))
                                 ,(hex (make-bytes 257 0)))
          '(list hash 4) `(42 "0xzz" (1) "0x" #t ("0x12") ,(make-list 5 (hex (make-bytes 32 0))))
          '(list hash +inf.0) '(42 "0xzz" (1) "0x" #t ("0x12"))
          'access-list `(42 "0xzz" (1) "0x" #t (42)
                            (,(hasheq 'address "0x12" 'storageKeys '()))
                            (,(hasheq 'address (hex (make-bytes 20 1)) 'storageKeys '("0x12"))))
          '(list (record log?) +inf.0) `(42 "0xzz" (1) "0x" #t (,log-json 42)
                                            (,(hash-set log-json 'data "0x1")))))

  (define (kind-entry table kind)
    (hash-ref table (field-kind-name kind)
              (lambda () (error 'record "no test values for kind ~e" (field-kind-name kind)))))

  (define (field-value record index)
    (vector-ref (struct->vector record) (add1 index)))

  (define tables
    `([receipt ,jsexpr->receipt ,receipt-fields
               ,(hash-set receipt-json 'root (hash-ref root-receipt-json 'root))]
      [log ,jsexpr->log ,log-fields ,log-json]
      [transaction-info ,jsexpr->transaction-info ,transaction-info-fields
                        ,transaction-info-json/1]))

  (define nouns
    (hasheq 'transaction-info "transaction"))

  (define (table-rows table)
    (match-define (list _ _ rows _) table)
    rows)

  (define (indexed-rows rows)
    (for/list ([row (in-list rows)]
               [index (in-naturals)])
      (cons index row)))

  (define messages
    (hasheq
     'receipt
     '((transaction-hash "receipt transaction hash")
       (transaction-index "receipt transaction index")
       (block-hash "receipt block hash")
       (block-number "receipt block number")
       (from "receipt from")
       (to "receipt to")
       (type "receipt type")
       (status "receipt status")
       (root "receipt root")
       (cumulative-gas-used "receipt cumulative gas used")
       (gas-used "receipt gas used")
       (effective-gas-price "receipt effective gas price")
       (contract-address "receipt contract address")
       (logs "receipt logs|log (?:data )?in receipt logs [0-9]+")
       (logs-bloom "receipt logs bloom")
       (blob-gas-used "receipt blob gas used")
       (blob-gas-price "receipt blob gas price"))
     'log
     '((address "log address")
       (topics "log topics(?: [0-9]+)?")
       (data "log data")
       (block-number "log block number")
       (block-hash "log block hash")
       (block-timestamp "log block timestamp")
       (transaction-hash "log transaction hash")
       (transaction-index "log transaction index")
       (index "log index")
       (removed? "log removed"))
     'transaction-info
     '((hash "transaction hash")
       (block-hash "transaction block hash")
       (block-number "transaction block number")
       (block-timestamp "transaction block timestamp")
       (transaction-index "transaction transaction index")
       (from "transaction from")
       (type "transaction type")
       (chain-id "transaction chain id")
       (nonce "transaction nonce")
       (to "transaction to")
       (value "transaction value")
       (data "transaction data")
       (gas-limit "transaction gas limit")
       (gas-price "transaction gas price")
       (max-priority-fee-per-gas "transaction max priority fee per gas")
       (max-fee-per-gas "transaction max fee per gas")
       (max-fee-per-blob-gas "transaction max fee per blob gas")
       (access-list "transaction access list(?: [0-9]+(?: address| storage keys(?: [0-9]+)?)?)?")
       (blob-versioned-hashes "transaction blob versioned hashes(?: [0-9]+)?")
       (v "transaction v")
       (y-parity "transaction y parity")
       (r "transaction r")
       (s "transaction s"))))

  (define (field-message noun field)
    (match (assq field (hash-ref messages noun))
      [(list _ words) words]
      [#f (error 'record "no message for ~a ~a" noun field)]))

  (test-case "R2: every row reads its key with its kind"
    (define prng (seeded 41 42 43 44 45 46))
    (for* ([table (in-list tables)]
           [indexed (in-list (indexed-rows (table-rows table)))]
           [_ (in-range 20)])
      (match-define (list noun reader _ base) table)
      (match-define (cons index (field-row field key kind mode _)) indexed)
      (check-not-false (memq mode '(required optional default)) (~a noun " " field))
      (match-define (list json good?) ((kind-entry samples kind) prng))
      (define record (reader (hash-set base key json)))
      (check-true (good? (field-value record index)) (~a noun " " field " " (~e json)))
      (when (eq? mode 'optional)
        (check-false (field-value (reader (hash-remove base key)) index) (~a field))
        (check-false (field-value (reader (hash-set base key 'null)) index) (~a field)))))

  (define (check-malformed thunk pattern label)
    (define e (raised thunk))
    (check-pred exn:fail:web3? e label)
    (check-false (exn:fail:contract? e) label)
    (when e
      (check-regexp-match pattern (exn-message e) label)))

  (test-case "R3: hostile fields raise exn:fail:web3 naming the field"
    (for* ([table (in-list tables)]
           [row (in-list (table-rows table))])
      (match-define (list noun reader _ base) table)
      (match-define (field-row field key kind mode _) row)
      (define pattern
        (pregexp (format "^jsexpr->~a: malformed (?:~a) from the node: "
                         noun (field-message noun field))))
      (define nulls (if (eq? mode 'optional) '() '(null)))
      (define removed (if (eq? mode 'required) (list (hash-remove base key)) '()))
      (for ([bad (in-list (append (for/list ([v (in-list (append (kind-entry hostiles kind) nulls))])
                                    (hash-set base key v))
                                  removed))])
        (check-malformed (lambda () (reader bad)) pattern (~a noun " " field " " (~e bad))))))

  (test-case "R3: a record is a JSON object"
    (for* ([table (in-list tables)]
           [v (in-list '(42 null ()))])
      (match-define (list noun reader _ _) table)
      (check-malformed (lambda () (reader v))
                       (pregexp (format "^jsexpr->~a: malformed ~a from the node: "
                                        noun (hash-ref nouns noun noun)))
                       (~a noun " " (~e v)))))

  (define storage-key (hex (make-bytes 32 7)))
  (define entry (hasheq 'address (hash-ref log-json 'address) 'storageKeys (list storage-key)))

  (define (read-access-list v)
    (read-field access-list-field 'reader "access list" v))

  (test-case "an access list reads into addresses and storage keys"
    (define entries (read-access-list (list entry)))
    (check-true ((field-kind-contract access-list-field) entries))
    (check-equal? entries (list (cons "0x5FbDB2315678afecb367f032d93F642f64180aa3"
                                   (list (make-bytes 32 7)))))
    (for ([bad (in-list `(42 (42) (,(hash-remove entry 'storageKeys))
                          (,(hash-set entry 'address "0x12"))
                          (,(hash-set entry 'storageKeys (list "0x12")))))])
      (check-malformed (lambda () (read-access-list bad))
                       #px"^reader: malformed access list"
                       (~e bad)))))
