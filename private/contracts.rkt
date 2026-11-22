#lang racket/base

(require racket/contract/base
         (only-in "../abi/type.rkt" abi-signature?)
         (only-in "../address.rkt" address?)
         (only-in "../rpc.rkt" provider?)
         (only-in "argument.rkt" address/c)
         (only-in "field-contracts.rkt" to/c)
         (only-in "signer.rkt" signer?))

(provide errors/c
         from/c
         provider/c
         sender?
         sender/c
         to/c)

(define (sender? v)
  (or (address? v) (signer? v)))

(define from/c (or/c #f signer? address/c))
(define sender/c (or/c signer? address/c))
(define errors/c (listof (or/c string? abi-signature?)))
(define provider/c (or/c #f provider?))
