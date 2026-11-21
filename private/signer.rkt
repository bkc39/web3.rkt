#lang racket/base

(require racket/generic)

(provide gen:signer
         signer?
         signer-implements?
         (prefix-out generic-
                     (combine-out signer-address
                                  signer-sign-digest
                                  signer-sign-message
                                  signer-sign-transaction)))

(define-generics signer
  (signer-address signer)
  (signer-sign-digest signer digest)
  (signer-sign-message signer message)
  (signer-sign-transaction signer tx)
  #:requires [signer-address]
  #:defined-predicate signer-implements?)
