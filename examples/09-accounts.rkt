#lang racket/base

(require (only-in keccak keccak256)
         web3)

(provide message-digest
         run
         signature->hex
         signer-of)

(define message-digest (keccak256 #"hello"))

(define (signature->hex sig)
  (bytes->hex-string (signature->bytes sig)))

(define (signer-of hex digest)
  (recover-address (bytes->signature (hex-string->bytes hex)) digest))

(define (run key)
  (define address (secret-key->address key))
  (define hex (signature->hex (sign-digest key message-digest)))
  (printf "address:   ~a\n" address)
  (printf "signature: ~a\n" hex)
  (printf "recovered: ~a\n" (signer-of hex message-digest)))

(module+ main
  (run (generate-secret-key)))
