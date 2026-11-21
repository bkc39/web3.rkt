#lang racket/base

(require (only-in keccak keccak256)
         web3)

(define message-digest (keccak256 #"hello"))

(module+ main
  (define key (generate-secret-key))
  (define sig (sign-digest key message-digest))
  (printf "~a\n~a\n" (secret-key->address key) (recover-address sig message-digest)))
