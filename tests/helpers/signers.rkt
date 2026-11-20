#lang racket/base

(require racket/match
         web3
         (only-in "../util.rkt" anvil-bob)
         (only-in "accounts.rkt" anvil-keys)
         (only-in "secrets.rkt" marker))

(provide flipper
         key0
         key1
         liar
         message-only
         signer0
         watched
         watched-calls)

(define-values (key0 key1)
  (match anvil-keys
    [(list* key0-hex key1-hex _) (values (hex-string->secret-key key0-hex)
                                         (hex-string->secret-key key1-hex))]))

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

(struct flipping (secret first key answers)
  #:transparent
  #:methods gen:signer
  [(define (signer-address signer)
     (define answers (flipping-answers signer))
     (set-box! answers (add1 (unbox answers)))
     (if (= (unbox answers) 1)
         (secret-key->address (flipping-first signer))
         (secret-key->address (flipping-key signer))))
   (define (signer-sign-digest signer digest) (sign-digest (flipping-key signer) digest))])

(define (flipper)
  (flipping marker key0 key1 (box 0)))
