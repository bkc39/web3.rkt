#lang racket/base

(require racket/contract/base
         racket/generic
         "account.rkt"
         (only-in "address.rkt" checksum-address?)
         "private/exn.rkt"
         (only-in "private/opaque.rkt"
                  address/c digest/c message/c opaque-> opaque/c secret-key/c)
         (only-in "rpc.rkt" provider?)
         "transaction.rkt"
         (only-in (submod "transaction.rkt" internal) transaction/c))

(define-generics signer
  (signer-address signer)
  (signer-sign-digest signer digest)
  (signer-sign-message signer message)
  (signer-sign-transaction signer tx)
  #:requires [signer-address]
  #:defined-predicate signer-implements?)

(define any-signer/c (opaque/c 'signer? "a signer" signer?))
(define provider/c (or/c #f provider?))

(provide gen:signer
         (struct-out exn:fail:web3:signer)
         (contract-out
          [signer? (opaque-> any/c boolean?)]
          [rename checked-address signer-address (opaque-> any-signer/c checksum-address?)]
          [rename checked-sign-digest signer-sign-digest
                  (opaque-> any-signer/c digest/c signature?)]
          [rename checked-sign-message signer-sign-message
                  (opaque-> any-signer/c message/c signature?)]
          [rename checked-sign-transaction signer-sign-transaction
                  (opaque-> any-signer/c transaction/c signed-transaction?)]
          [local-signer (opaque-> secret-key/c local-signer?)]
          [local-signer? (opaque-> any/c boolean?)]
          [node-signer (opaque-> address/c [#:provider provider/c] node-signer?)]
          [node-signer? (opaque-> any/c boolean?)]))

(module* internal #f
  (provide (rename-out [any-signer/c signer/c])
           signer-reason/c
           signer-address/who
           signer-sign-transaction/who))

(define signer-phrases
  (hasheq 'unsupported "not supported"
          'malformed "malformed result"
          'wrong-signer "wrong signer"
          'wrong-transaction "wrong transaction"
          'high-s "high s"))

(define signer-reason/c
  (flat-named-contract 'signer-reason/c (apply or/c (hash-keys signer-phrases #t))))

(struct local-signer (key address)
  #:constructor-name new-local-signer
  #:omit-define-syntaxes
  #:methods gen:signer
  [(define (signer-address signer) (error 'unimplemented))
   (define (signer-sign-digest signer digest) (error 'unimplemented))])

(struct node-signer (address provider)
  #:constructor-name new-node-signer
  #:omit-define-syntaxes
  #:methods gen:signer
  [(define (signer-address signer) (error 'unimplemented))
   (define (signer-sign-message signer message) (error 'unimplemented))
   (define (signer-sign-transaction signer tx) (error 'unimplemented))])

(define (local-signer key)
  (error 'unimplemented))

(define (node-signer address #:provider [provider #f])
  (error 'unimplemented))

(define (signer-address/who who signer)
  (error 'unimplemented))

(define (signer-sign-transaction/who who signer tx)
  (error 'unimplemented))

(define (checked-address signer)
  (error 'unimplemented))

(define (checked-sign-digest signer digest)
  (error 'unimplemented))

(define (checked-sign-message signer message)
  (error 'unimplemented))

(define (checked-sign-transaction signer tx)
  (error 'unimplemented))
