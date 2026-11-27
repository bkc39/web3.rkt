#lang racket/base

(require (only-in keccak keccak256)
         (only-in "hex.rkt" address-hex-string? bytes->hex natural->bytes))

(provide address->checksum-string
         address->integer
         address-modulus
         address?
         bytes->address
         checksum-address?
         digest->address
         integer->address)

(define address-modulus (expt 2 160))

(define (address->checksum-string address)
  (define lower (string-downcase (substring address 2)))
  (define hash (keccak256 (string->bytes/latin-1 lower)))
  (define (nibble i)
    (define b (bytes-ref hash (quotient i 2)))
    (if (even? i) (arithmetic-shift b -4) (bitwise-and b 15)))
  (string-append
   "0x"
   (build-string 40 (lambda (i)
                      (define c (string-ref lower i))
                      (if (and (char-alphabetic? c) (>= (nibble i) 8)) (char-upcase c) c)))))

(define (one-case? v)
  (define digits (substring v 2))
  (or (string=? digits (string-downcase digits)) (string=? digits (string-upcase digits))))

(define (address? v)
  (and (address-hex-string? v)
       (or (one-case? v) (string=? v (address->checksum-string v)))))

(define (checksum-address? v)
  (and (address-hex-string? v) (string=? v (address->checksum-string v))))

(define (bytes->address address-bytes)
  (address->checksum-string (bytes->hex address-bytes)))

(define (address->integer address)
  (string->number (substring address 2) 16))

(define (integer->address number)
  (bytes->address (natural->bytes number 20)))

(define (digest->address digest)
  (bytes->address (subbytes digest 12)))
