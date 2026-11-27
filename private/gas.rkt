#lang racket/base

(provide floor-gas
         intrinsic-gas)

(define (intrinsic-gas data #:creation? [creation? #f] #:access-list [access-list '()])
  (error 'unimplemented))

(define (floor-gas data)
  (error 'unimplemented))
