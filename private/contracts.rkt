#lang racket/base

(require racket/contract/base
         (only-in json jsexpr?)
         (only-in "../abi/type.rkt" abi-signature? abi-value? abi-value/c)
         (only-in "../rpc.rkt" provider?)
         (only-in "opaque.rkt" opaque/c)
         (only-in "signer.rkt" signer?))

(provide errors/c
         from/c
         provider/c
         receipt/c
         sender?
         sender/c
         to/c)

(define (sender? v)
  (or (abi-value? 'address v) (signer? v)))

(define from/c
  (opaque/c 'from/c "#f, an address or a signer" (lambda (v) (or (not v) (sender? v)))))
(define sender/c (opaque/c 'sender/c "an address or a signer" sender?))
(define to/c (or/c #f (abi-value/c 'address)))
(define errors/c (listof (or/c string? abi-signature?)))
(define provider/c (or/c #f provider?))
(define receipt/c (and/c hash? jsexpr?))
