#lang racket/base

(require web3)

(define gas-limit 21000)

(define (print-refusal text)
  (with-handlers ([exn:fail:web3:units?
                   (lambda (e) (printf "~s: ~a\n" text (exn-message e)))])
    (ether->wei text)))

(module+ main
  (define value (ether->wei "1.5"))
  (define fee (* gas-limit (gwei->wei 30)))
  (printf "value ~a wei\n" value)
  (printf "fee   ~a ether\n" (wei->string fee))
  (printf "total ~a ether\n" (wei->string (+ value fee)))
  (printf "token ~a\n" (wei->string (units->wei "1234.5" 6) 6 #:min-fraction-digits 2))
  (for ([text (in-list '("1,5" "0.0000000000000000001" ""))])
    (print-refusal text)))
