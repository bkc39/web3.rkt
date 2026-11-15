#lang racket/base

(require racket/contract/base
         (only-in keccak keccak256)
         "abi/type.rkt")

(provide (contract-out
          [address->checksum-string (-> (abi-value/c 'address) string?)]
          [checksum-address? (-> any/c boolean?)]))

(define (address->checksum-string address)
  (error 'unimplemented))

(define (checksum-address? v)
  (error 'unimplemented))
