#lang racket/base

(require racket/contract/base
         "private/exn.rkt")

(define amount/c (or/c string? (and/c rational? exact?)))
(define unit/c (or/c 'wei 'gwei 'ether (integer-in 0 255)))

(provide (struct-out exn:fail:web3:units)
         (contract-out
          [units->wei (-> amount/c unit/c exact-integer?)]
          [ether->wei (-> amount/c exact-integer?)]
          [gwei->wei (-> amount/c exact-integer?)]
          [wei->string (->* (exact-integer?)
                            (unit/c #:min-fraction-digits (integer-in 0 255))
                            string?)]))

(define (units->wei amount unit)
  (error 'unimplemented))

(define (ether->wei amount)
  (error 'unimplemented))

(define (gwei->wei amount)
  (error 'unimplemented))

(define (wei->string n [unit 'ether] #:min-fraction-digits [k 0])
  (error 'unimplemented))
