#lang racket/base

(require (only-in racket/match match))

(provide floor-gas
         intrinsic-gas)

(define transaction-gas 21000)
(define creation-gas 32000)
(define zero-byte-gas 4)
(define nonzero-byte-gas 16)
(define initcode-word-gas 2)
(define word-size 32)
(define access-list-address-gas 2400)
(define access-list-key-gas 1900)
(define floor-token-gas 10)
(define tokens-per-zero-byte 1)
(define tokens-per-nonzero-byte 4)

(define (byte-sum data #:zero zero #:nonzero nonzero)
  (for/sum ([byte (in-bytes data)])
    (if (zero? byte) zero nonzero)))

(define (initcode-gas data)
  (+ creation-gas
     (* initcode-word-gas (quotient (+ (bytes-length data) (sub1 word-size)) word-size))))

(define (access-list-gas access-list)
  (for/sum ([entry (in-list access-list)])
    (match entry
      [(cons _address keys) (+ access-list-address-gas (* access-list-key-gas (length keys)))])))

(define (intrinsic-gas data #:creation? [creation? #f] #:access-list [access-list '()])
  (+ transaction-gas
     (byte-sum data #:zero zero-byte-gas #:nonzero nonzero-byte-gas)
     (if creation? (initcode-gas data) 0)
     (access-list-gas access-list)))

(define (floor-gas data)
  (+ transaction-gas
     (* floor-token-gas
        (byte-sum data #:zero tokens-per-zero-byte #:nonzero tokens-per-nonzero-byte))))
