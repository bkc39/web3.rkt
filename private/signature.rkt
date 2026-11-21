#lang racket/base

(require (only-in racket/match match-define)
         (only-in "hex.rkt" bytes->hex natural->bytes))

(provide (struct-out signature)
         signature->bytes
         signature-v)

(struct signature (r s y-parity)
  #:transparent
  #:property prop:custom-write
  (lambda (sig port mode)
    (write-string (format "#<signature ~a>" (bytes->hex (signature->bytes sig))) port)))

(define (signature-v sig)
  (+ 27 (signature-y-parity sig)))

(define (signature->bytes sig)
  (match-define (signature r s _) sig)
  (bytes-append (natural->bytes r 32) (natural->bytes s 32) (bytes (signature-v sig))))
