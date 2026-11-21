#lang racket/base

(require racket/contract/base
         (only-in json jsexpr?)
         (only-in "../rpc.rkt" number->hex-string)
         (only-in "hex.rkt" bytes->hex natural->bytes))

(provide block-param/c
         block-param->string
         object/c
         objects/c
         percentiles->jsexpr
         reward-percentiles/c
         storage-slot/c
         storage-slots->jsexpr)

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

(define object/c (and/c hash? jsexpr?))

(define objects/c (listof hash?))
