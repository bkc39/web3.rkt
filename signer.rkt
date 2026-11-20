#lang racket/base

(require racket/contract/base
         racket/generic
         (only-in racket/match match match*)
         (only-in "abi/type.rkt" abi-value?)
         "account.rkt"
         (only-in (submod "account.rkt" private) bytes->signature/who sign-digest/who)
         (only-in "address.rkt" address->checksum-string checksum-address?)
         (only-in "eth.rkt" eth-sign eth-sign-transaction)
         "private/exn.rkt"
         (only-in "private/hex.rkt" bytes->hex hex->bytes hex-data-string?)
         (only-in "private/opaque.rkt"
                  address/c describe-kind digest/c message/c opaque-> opaque/c secret-key/c)
         (only-in "rpc.rkt" current-provider provider?)
         "transaction.rkt"
         (only-in (submod "transaction.rkt" internal)
                  bytes->signed-transaction/who transaction/c transaction-difference))

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

(define (raise-signer who reason fmt . args)
  (raise (exn:fail:web3:signer
          (format "~a: ~a: ~a" who (hash-ref signer-phrases reason) (apply format fmt args))
          (current-continuation-marks)
          reason)))

(define (write-signer kind address port)
  (write-string (format "#<~a ~a>" kind address) port))

(struct local-signer (key address)
  #:constructor-name new-local-signer
  #:omit-define-syntaxes
  #:property prop:custom-write
  (lambda (signer port _mode) (write-signer 'local-signer (local-signer-address signer) port))
  #:methods gen:signer
  [(define (signer-address signer) (local-signer-address signer))
   (define (signer-sign-digest signer digest)
     (local-sign-digest 'signer-sign-digest signer digest))])

(struct node-signer (address provider)
  #:constructor-name new-node-signer
  #:omit-define-syntaxes
  #:property prop:custom-write
  (lambda (signer port _mode) (write-signer 'node-signer (node-signer-address signer) port))
  #:methods gen:signer
  [(define (signer-address signer) (node-signer-address signer))
   (define (signer-sign-message signer message)
     (node-sign-message 'signer-sign-message signer message))
   (define (signer-sign-transaction signer tx)
     (node-sign-transaction 'signer-sign-transaction signer tx))])

(define (local-signer key)
  (new-local-signer key (secret-key->address key)))

(define (node-signer address #:provider [provider #f])
  (new-node-signer (address->checksum-string address) provider))

(define (local-sign-digest who signer digest)
  (sign-digest/who who (local-signer-key signer) digest))

(define (node-provider signer)
  (or (node-signer-provider signer) (current-provider)))

(define (node-sign-message who signer message)
  (define answer
    (eth-sign (node-signer-address signer) (bytes->hex message) #:provider (node-provider signer)))
  (if (and (hex-data-string? answer) (= (string-length answer) 132))
      (bytes->signature/who who (hex->bytes answer))
      (raise-signer who 'malformed "the node returned ~a, not a 65-byte signature"
                    (describe-kind answer))))

(define (node-sign-transaction who signer tx)
  (define answer
    (eth-sign-transaction (transaction->jsexpr tx #:from (node-signer-address signer))
                          #:provider (node-provider signer)))
  (if (hex-data-string? answer)
      (bytes->signed-transaction/who who (hex->bytes answer))
      (raise-signer who 'malformed "the node returned ~a, not a raw transaction"
                    (describe-kind answer))))

(define generic-methods
  (hasheq 'signer-sign-digest signer-sign-digest
          'signer-sign-message signer-sign-message
          'signer-sign-transaction signer-sign-transaction))

(define (call-method who method signer argument)
  (match* (signer method)
    [((? local-signer?) 'signer-sign-digest) (local-sign-digest who signer argument)]
    [((? node-signer?) 'signer-sign-message) (node-sign-message who signer argument)]
    [((? node-signer?) 'signer-sign-transaction) (node-sign-transaction who signer argument)]
    [(_ _) ((hash-ref generic-methods method) signer argument)]))

(define (signer-address/who who signer)
  (define address (signer-address signer))
  (if (abi-value? 'address address)
      (address->checksum-string address)
      (raise-signer who 'malformed "the signer's address is ~a, not an address"
                    (describe-kind address))))

(define (check-signature who sig digest address)
  (define recovered (and (signature? sig) (signature-low-s? sig) (recover-address sig digest)))
  (cond
    [(not (signature? sig))
     (raise-signer who 'malformed "the signer returned ~a, not a signature" (describe-kind sig))]
    [(not (signature-low-s? sig))
     (raise-signer who 'high-s "s is more than half the curve order (EIP-2)")]
    [(not recovered) (raise-signer who 'wrong-signer "the signature recovers no address")]
    [(not (equal? recovered address))
     (raise-signer who 'wrong-signer "the signature recovers ~a, not the signer's address ~a"
                   recovered address)]
    [else sig]))

(define (check-signed-transaction who stx tx address)
  (define difference
    (and (signed-transaction? stx)
         (transaction-difference tx (signed-transaction-transaction stx))))
  (cond
    [(not (signed-transaction? stx))
     (raise-signer who 'malformed "the signer returned ~a, not a signed transaction"
                   (describe-kind stx))]
    [difference
     (raise-signer who 'wrong-transaction "the signed transaction differs in ~a" difference)]
    [(not (equal? (signed-transaction-sender stx) address))
     (raise-signer who 'wrong-signer "the transaction's sender is ~a, not the signer's address ~a"
                   (signed-transaction-sender stx) address)]
    [else stx]))

(define (first-method signer methods)
  (for/first ([method (in-list methods)] #:when (signer-implements? signer method))
    method))

(define (signed-digest who signer method argument digest address)
  (check-signature who (call-method who method signer argument) digest address))

(define (signer-sign-digest/who who signer digest)
  (define method (first-method signer '(signer-sign-digest)))
  (define address (and method (signer-address/who who signer)))
  (match method
    [#f (raise-signer who 'unsupported "the signer signs no digests")]
    [_ (signed-digest who signer method digest digest address)]))

(define (signer-sign-message/who who signer message)
  (define body (if (bytes? message) message (string->bytes/utf-8 message)))
  (define digest (hash-message body))
  (define method (first-method signer '(signer-sign-message signer-sign-digest)))
  (define address (and method (signer-address/who who signer)))
  (match method
    [#f (raise-signer who 'unsupported "the signer signs neither messages nor digests")]
    ['signer-sign-message (signed-digest who signer method body digest address)]
    ['signer-sign-digest (signed-digest who signer method digest digest address)]))

(define (signer-sign-transaction/who who signer tx)
  (define digest (transaction-signing-hash tx))
  (define method (first-method signer '(signer-sign-transaction signer-sign-digest)))
  (define address (and method (signer-address/who who signer)))
  (match method
    [#f (raise-signer who 'unsupported "the signer signs neither transactions nor digests")]
    ['signer-sign-transaction
     (check-signed-transaction who (call-method who method signer tx) tx address)]
    ['signer-sign-digest
     (signed-transaction tx (signed-digest who signer method digest digest address))]))

(define (checked-address signer)
  (signer-address/who 'signer-address signer))

(define (checked-sign-digest signer digest)
  (signer-sign-digest/who 'signer-sign-digest signer digest))

(define (checked-sign-message signer message)
  (signer-sign-message/who 'signer-sign-message signer message))

(define (checked-sign-transaction signer tx)
  (signer-sign-transaction/who 'signer-sign-transaction signer tx))
