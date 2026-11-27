#lang racket/base

(require racket/contract/base
         "private/exn.rkt")

(define halts
  '(out-of-gas stack-overflow stack-underflow invalid-opcode undefined-opcode invalid-jump
               return-data-out-of-bounds write-protection call-depth code-size initcode-size
               invalid-code address-collision))

(define halt/c (flat-named-contract 'halt/c (apply or/c halts)))

(provide halt/c
         (contract-out
          [halts (listof halt/c)]
          [message->halt (-> string? (or/c #f halt/c))]
          [exn->halt (-> exn? (or/c #f halt/c))]))

(define (message->halt message)
  (error 'unimplemented))

(define (exn->halt e)
  (error 'unimplemented))
