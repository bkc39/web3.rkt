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

  ;; A random value of a kind: the JSON the node sends and a check of what reads.
  (define (sample prng kind)
    (match (field-kind-name kind)
      ['uint64 (natural-sample prng 64)]
      ['quantity (natural-sample prng 256)]
      ['byte (natural-sample prng 8)]
      ['bit (natural-sample prng 1)]
      ['boolean (define b (zero? (random 2 prng))) (list b (lambda (v) (eq? v b)))]
      ['address
       (define json (hex (random-bytes prng 20)))
       (list json (lambda (v) (and (checksum-address? v) (equal? (string-downcase v) json))))]
      ['hash (define json (random-hash prng))
             (list json (lambda (v) (equal? v (string-downcase json))))]
      ['data (data-sample prng (random 70 prng))]
      [(list 'fixed-data size) (data-sample prng size)]
      ['topics
       (define json (for/list ([_ (in-range (random 5 prng))]) (random-hash prng)))
       (list json (lambda (v) (equal? v (map string-downcase json))))]
      [(list 'list (list 'record 'log?))
       (define json (make-list (random 4 prng) log-json))
       (list json (lambda (v) (equal? (map log->jsexpr v) json)))]))

  (define (natural-sample prng bits)
    (define n (if (zero? (random 4 prng)) (sub1 (expt 2 bits)) (random-natural prng bits)))
    (list (quantity n) (lambda (v) (eqv? v n))))

  (define (data-sample prng size)
    (define bs (random-bytes prng size))
    (list (hex bs) (lambda (v) (equal? v bs))))

  (define (field-value record index)
    (vector-ref (struct->vector record) (add1 index)))

  (define tables
    `([receipt ,jsexpr->receipt ,receipt-fields
               ,(hash-set receipt-json 'root (hash-ref root-receipt-json 'root))]
      [log ,jsexpr->log ,log-fields ,log-json]))

  (test-case "R2: every row reads its key with its kind"
    (define prng (seeded 41 42 43 44 45 46))
    (for* ([table (in-list tables)]
           [indexed (in-list (for/list ([row (in-list (caddr table))]
                                        [index (in-naturals)])
                               (cons index row)))]
           [_ (in-range 20)])
      (match-define (list noun reader _ base) table)
      (match-define (list index field key kind mode _) indexed)
      (match-define (list json good?) (sample prng kind))
      (define record (reader (hash-set base key json)))
      (check-true (good? (field-value record index)) (~a noun " " field " " (~e json)))
      (when (eq? mode 'optional)
        (check-false (field-value (reader (hash-remove base key)) index) (~a field))
        (check-false (field-value (reader (hash-set base key 'null)) index) (~a field)))))

  (define (field-words field)
    (regexp-replace* #rx"-" (regexp-replace #rx"[?]$" (symbol->string field) "") " "))

  (define (hostile kind mode)
    (append '(42 "0xzz" (1))
            (if (eq? (field-kind-name kind) 'data) '() '("0x"))
            (if (eq? (field-kind-name kind) 'boolean) '() '(#t))
            (if (memq mode '(required default)) '(null) '())
            (match (field-kind-name kind)
              [(or 'uint64 'byte 'bit 'quantity) (list (quantity (expt 16 64)))]
              [_ '()])
            (match (field-kind-name kind)
              [(or 'uint64 'byte 'bit) (list (quantity (expt 2 64)))]
              [_ '()])
            (match (field-kind-name kind)
              ['bit '("0x2")]
              [(list 'fixed-data size) (map (lambda (n) (hex (make-bytes n 0))) (list (sub1 size)
                                                                                     (add1 size)))]
              [(list 'list _) (list (list log-json 42) (list (hash-set log-json 'data "0x1")))]
              [_ '()])))

  (define (check-malformed thunk pattern label)
    (define e (raised thunk))
    (check-pred exn:fail:web3? e label)
    (check-false (exn:fail:contract? e) label)
    (when e
      (check-regexp-match pattern (exn-message e) label)))

  (test-case "R3: hostile fields raise exn:fail:web3 naming the field"
    (for* ([table (in-list tables)]
           [row (in-list (caddr table))])
      (match-define (list noun reader _ base) table)
      (match-define (list field key kind mode _) row)
      (define pattern
        (pregexp (format "^jsexpr->~a: malformed (?:.* in )?~a ~a(?: [0-9]+)? from the node: "
                         noun noun (field-words field))))
      (define removed (if (eq? mode 'required) (list (hash-remove base key)) '()))
      (for ([bad (in-list (append (for/list ([v (in-list (hostile kind mode))])
                                    (hash-set base key v))
                                  removed))])
        (check-malformed (lambda () (reader bad)) pattern (~a noun " " field " " (~e bad))))))

  (test-case "R3: a record is a JSON object"
    (for* ([table (in-list tables)]
           [v (in-list '(42 null ()))])
      (match-define (list noun reader _ _) table)
      (check-malformed (lambda () (reader v))
                       (pregexp (format "^jsexpr->~a: malformed ~a from the node: " noun noun))
                       (~a noun " " (~e v)))))

  (define storage-key (hex (make-bytes 32 7)))
  (define entry (hasheq 'address (hash-ref log-json 'address) 'storageKeys (list storage-key)))

  (define (read-access-list v)
    ((field-kind-reader access-list-field) 'reader "access list" v))

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
