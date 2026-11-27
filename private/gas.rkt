#lang racket/base

(provide floor-gas
         intrinsic-gas)

(define (byte-sum data zero other)
  (for/sum ([byte (in-bytes data)])
    (if (zero? byte) zero other)))

(define (creation-gas data)
  (+ 32000 (* 2 (quotient (+ (bytes-length data) 31) 32))))

(define (access-list-gas access-list)
  (for/sum ([entry (in-list access-list)])
    (+ 2400 (* 1900 (length (cdr entry))))))

(define (intrinsic-gas data #:creation? [creation? #f] #:access-list [access-list '()])
  (+ 21000
     (byte-sum data 4 16)
     (if creation? (creation-gas data) 0)
     (access-list-gas access-list)))

(define (floor-gas data)
  (+ 21000 (* 10 (byte-sum data 1 4))))
