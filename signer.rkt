#lang racket/base

(require racket/contract/base
         (only-in racket/match match-define)
         (only-in "abi/type.rkt" abi-value?)
         "account.rkt"
         (only-in (submod "account.rkt" private)
                  bytes->signature/who
                  message->bytes
                  sign-digest/who)
         (only-in "address.rkt" address->checksum-string checksum-address?)
         (only-in "eth.rkt" eth-sign eth-sign-transaction)
         "private/exn.rkt"
         (only-in "private/hex.rkt" bytes->hex hex->bytes hex-data-string?)
         (only-in "private/opaque.rkt"
                  address/c describe-kind digest/c message/c opaque-> opaque/c secret-key/c)
         "private/signer.rkt"
         "private/who.rkt"
         (only-in "rpc.rkt" current-provider provider?)
         "transaction.rkt"
         (only-in (submod "transaction.rkt" internal)
                  bytes->signed-transaction/who transaction/c transaction-difference))

(define signer/c (opaque/c 'signer? "a signer" signer?))
(define provider/c (or/c #f provider?))

(provide gen:signer
         (struct-out exn:fail:web3:signer)
         (contract-out
          [signer? (opaque-> any/c boolean?)]
          [signer-address (opaque-> signer/c checksum-address?)]
          [signer-sign-digest (opaque-> signer/c digest/c signature?)]
          [signer-sign-message (opaque-> signer/c message/c signature?)]
          [signer-sign-transaction (opaque-> signer/c transaction/c signed-transaction?)]
          [local-signer (opaque-> secret-key/c local-signer?)]
          [local-signer? (opaque-> any/c boolean?)]
          [node-signer (opaque-> address/c [#:provider provider/c] node-signer?)]
          [node-signer? (opaque-> any/c boolean?)]))

(module* internal #f
  (provide signer/c
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
  [(define (signer-address signer) (local-signer-address signer))])

(struct node-signer (address provider)
  #:constructor-name new-node-signer
  #:omit-define-syntaxes
  #:property prop:custom-write
  (lambda (signer port _mode) (write-signer 'node-signer (node-signer-address signer) port))
  #:methods gen:signer
  [(define (signer-address signer) (node-signer-address signer))])

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

(define methods
  `((signer-sign-digest ,generic-signer-sign-digest ,local-signer? ,local-sign-digest)
    (signer-sign-message ,generic-signer-sign-message ,node-signer? ,node-sign-message)
    (signer-sign-transaction ,generic-signer-sign-transaction ,node-signer?
                             ,node-sign-transaction)))

(define (find-method who signer name)
  (match-define (list _ generic built-in? built-in) (assq name methods))
  (cond
    [(built-in? signer) (lambda (target value) (built-in who target value))]
    [(signer-implements? signer name) generic]
    [else #f]))

(define/who (signer-address who signer)
  (define address (generic-signer-address signer))
  (if (abi-value? 'address address)
      (address->checksum-string address)
      (raise-signer who 'malformed "the signer's address is ~a, not an address"
                    (describe-kind address))))

(define (check-signature who sig digest address)
  (cond
    [(not (signature? sig))
     (raise-signer who 'malformed "the signer returned ~a, not a signature" (describe-kind sig))]
    [(not (signature-low-s? sig))
     (raise-signer who 'high-s "s is more than half the curve order (EIP-2)")]
    [(equal? (recover-address sig digest) address) sig]
    [(recover-address sig digest)
     => (lambda (recovered)
          (raise-signer who 'wrong-signer "the signature recovers ~a, not the signer's address ~a"
                        recovered address))]
    [else (raise-signer who 'wrong-signer "the signature recovers no address")]))

(define (check-signed-transaction who stx tx address)
  (cond
    [(not (signed-transaction? stx))
     (raise-signer who 'malformed "the signer returned ~a, not a signed transaction"
                   (describe-kind stx))]
    [(transaction-difference tx (signed-transaction-transaction stx))
     => (lambda (difference)
          (raise-signer who 'wrong-transaction "the signed transaction differs in ~a" difference))]
    [(not (equal? (signed-transaction-sender stx) address))
     (raise-signer who 'wrong-signer "the transaction's sender is ~a, not the signer's address ~a"
                   (signed-transaction-sender stx) address)]
    [else stx]))

(define (signed-digest who signer sign argument digest address)
  (check-signature who (sign signer (bytes-copy argument)) digest address))

(define/who (signer-sign-digest who signer digest)
  (define expected (bytes->immutable-bytes digest))
  (define by-digest (find-method who signer 'signer-sign-digest))
  (if by-digest
      (signed-digest who signer by-digest expected expected (signer-address/who who signer))
      (raise-signer who 'unsupported "the signer signs no digests")))

(define/who (signer-sign-message who signer message)
  (define body (bytes->immutable-bytes (message->bytes message)))
  (define digest (bytes->immutable-bytes (hash-message body)))
  (define by-message (find-method who signer 'signer-sign-message))
  (define by-digest (find-method who signer 'signer-sign-digest))
  (cond
    [by-message (signed-digest who signer by-message body digest (signer-address/who who signer))]
    [by-digest (signed-digest who signer by-digest digest digest (signer-address/who who signer))]
    [else (raise-signer who 'unsupported "the signer signs neither messages nor digests")]))

(define (signed-transaction-by-digest who signer by-digest tx)
  (define digest (bytes->immutable-bytes (transaction-signing-hash tx)))
  (define address (signer-address/who who signer))
  (define sig (signed-digest who signer by-digest digest digest address))
  (check-signed-transaction who (signed-transaction tx sig) tx address))

(define/who (signer-sign-transaction who signer tx)
  (define by-transaction (find-method who signer 'signer-sign-transaction))
  (define by-digest (find-method who signer 'signer-sign-digest))
  (cond
    [by-transaction
     (define address (signer-address/who who signer))
     (check-signed-transaction who (by-transaction signer tx) tx address)]
    [by-digest (signed-transaction-by-digest who signer by-digest tx)]
    [else (raise-signer who 'unsupported "the signer signs neither transactions nor digests")]))
