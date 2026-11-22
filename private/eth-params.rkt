#lang racket/base

(require racket/contract/base
         (only-in racket/contract
                  blame-add-context
                  build-compound-type-name
                  contract-first-order
                  get/build-late-neg-projection
                  make-chaperone-contract
                  raise-blame-error)
         (only-in json jsexpr?)
         (only-in "../rpc.rkt" number->hex-string)
         (only-in "argument.rkt" address/c one-or-more/c)
         (only-in "hex.rkt" bytes->hex natural->bytes))

(provide block-param/c
         block-param->string
         filter-object/c
         objects/c
         percentiles->jsexpr
         reward-percentiles/c
         storage-slot/c
         storage-slots->jsexpr
         transaction-object/c)

(define block-param/c
  (flat-named-contract
   'block-param/c
   (or/c exact-nonnegative-integer? "latest" "earliest" "pending" "safe" "finalized")))

(define (block-param->string block)
  (if (exact-nonnegative-integer? block) (number->hex-string block) block))

(define (percentiles->jsexpr percentiles)
  (for/list ([percentile (in-list percentiles)])
    (if (exact-integer? percentile) percentile (exact->inexact percentile))))

(define (increasing-percentiles? percentiles)
  (apply < -inf.0 (percentiles->jsexpr percentiles)))

(define reward-percentiles/c
  (flat-named-contract 'reward-percentiles/c
                       (and/c (listof (real-in 0 100)) increasing-percentiles?)))

(define storage-slot/c (flat-named-contract 'storage-slot/c (integer-in 0 (sub1 (expt 2 256)))))

(define (storage-slots->jsexpr slots)
  (for/list ([slot (in-list slots)])
    (bytes->hex (natural->bytes slot 32))))

(define absent (string->uninterned-symbol "absent"))

;; The fields of an object that the library checks; the node judges the rest, and any value that
;; is not a hash table or a list where one is looked into.
(define (fields/c name fields #:object? [object? #f])
  (define checks
    (for/hasheq ([(key field/c) (in-hash fields)])
      (values key (contract-first-order field/c))))
  (define (well-formed? v)
    (or (not object?) (and (hash? v) (jsexpr? v))))
  (make-chaperone-contract
   #:name name
   #:first-order
   (lambda (v)
     (and (well-formed? v)
          (or (not (hash? v))
              (for/and ([(key ok?) (in-hash checks)])
                (define field (hash-ref v key absent))
                (or (eq? field absent) (ok? field))))))
   #:late-neg-projection
   (lambda (blame)
     (define projections
       (for/hasheq ([(key field/c) (in-hash fields)])
         (values key ((get/build-late-neg-projection field/c)
                      (blame-add-context blame (format "the ~a field of" key))))))
     (lambda (v neg)
       (unless (well-formed? v)
         (raise-blame-error blame #:missing-party neg v '(expected: "~a" given: "~e") name v))
       (when (hash? v)
         (for ([(key project) (in-hash projections)])
           (define field (hash-ref v key absent))
           (unless (eq? field absent)
             (project field neg))))
       v))))

(define (entries/c entry/c)
  (define entry? (contract-first-order entry/c))
  (define project (get/build-late-neg-projection entry/c))
  (make-chaperone-contract
   #:name (build-compound-type-name 'entries/c entry/c)
   #:first-order (lambda (v) (or (not (list? v)) (andmap entry? v)))
   #:late-neg-projection
   (lambda (blame)
     (define each (project (blame-add-context blame "an element of")))
     (lambda (v neg)
       (when (list? v)
         (for ([entry (in-list v)])
           (each entry neg)))
       v))))

(define transaction-object/c
  (fields/c 'transaction-object/c
            (hasheq 'from address/c
                    'to (or/c 'null address/c)
                    'accessList
                    (entries/c (fields/c 'access-list-entry (hasheq 'address address/c))))
            #:object? #t))

(define filter-object/c
  (fields/c 'filter-object/c
            (hasheq 'address
                    (or/c null? (one-or-more/c address/c "an address or a list of addresses")))
            #:object? #t))

(define objects/c (listof hash?))
