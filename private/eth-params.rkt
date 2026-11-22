#lang racket/base

(require racket/contract/base
         (only-in json jsexpr?)
         (only-in "../address.rkt" address?)
         (only-in "../rpc.rkt" number->hex-string)
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

(define (field-ok? object key ok?)
  (define v (hash-ref object key absent))
  (or (eq? v absent) (ok? v)))

(define (to-ok? v)
  (or (eq? v 'null) (address? v)))

(define (addresses-ok? v)
  (or (address? v) (and (list? v) (andmap address? v))))

(define (access-list-ok? v)
  (or (not (list? v))
      (for/and ([entry (in-list v)])
        (or (not (hash? entry)) (field-ok? entry 'address address?)))))

(define (object-contract name checks)
  (flat-named-contract
   name
   (lambda (v)
     (and (hash? v)
          (jsexpr? v)
          (for/and ([(key ok?) (in-hash checks)])
            (field-ok? v key ok?))))))

(define transaction-object/c
  (object-contract 'transaction-object/c
                   (hasheq 'from address? 'to to-ok? 'accessList access-list-ok?)))

(define filter-object/c (object-contract 'filter-object/c (hasheq 'address addresses-ok?)))

(define objects/c (listof hash?))
