#lang racket/base

(require racket/match
         web3
         (only-in "../util.rkt" anvil-bob)
         (only-in "accounts.rkt" anvil-keys)
         (only-in "secrets.rkt" marker))

(provide key0
         liar
         message-only
         signer0
         watched
         watched-calls)

(define key0
  (match anvil-keys
    [(cons key-hex _) (hex-string->secret-key key-hex)]))

(define signer0 (local-signer key0))

(define (log! calls name)
  (set-box! calls (cons name (unbox calls))))

(struct watcher (secret key calls)
  #:transparent
  #:methods gen:signer
  [(define (signer-address signer)
     (log! (watcher-calls signer) 'address)
     (secret-key->address (watcher-key signer)))
   (define (signer-sign-digest signer digest)
     (log! (watcher-calls signer) 'digest)
     (sign-digest (watcher-key signer) digest))])

(define (watched [key key0])
  (watcher marker key (box '())))

(define (watched-calls signer)
  (reverse (unbox (watcher-calls signer))))

(struct liar (secret key)
  #:transparent
  #:methods gen:signer
  [(define (signer-address _signer) anvil-bob)
   (define (signer-sign-digest signer digest) (sign-digest (liar-key signer) digest))])

(struct message-signer (secret key)
  #:transparent
  #:methods gen:signer
  [(define (signer-address signer) (secret-key->address (message-signer-key signer)))
   (define (signer-sign-message signer message) (sign-message (message-signer-key signer) message))])

(define (message-only)
  (message-signer marker key0))
