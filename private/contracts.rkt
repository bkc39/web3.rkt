#lang racket/base

(require racket/contract/base
         (only-in "../abi/type.rkt" abi-signature?)
         (only-in "../address.rkt" address?)
         (only-in "../rpc.rkt" provider?)
         (only-in "field-contracts.rkt" to/c)
         (only-in "opaque.rkt" opaque/c)
         (only-in "signer.rkt" signer?))

(provide errors/c
         from/c
         provider/c
         sender?
         sender/c
         to/c)

(define (sender? v)
  (or (address? v) (signer? v)))

(define from/c
  (opaque/c 'from/c "#f, an address or a signer" (lambda (v) (or (not v) (sender? v)))))
(define sender/c (opaque/c 'sender/c "an address or a signer" sender?))
(define errors/c (listof (or/c string? abi-signature?)))
(define provider/c (or/c #f provider?))
