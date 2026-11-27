#lang racket/base

(require racket/contract/base
         (only-in keccak keccak256)
         (only-in "private/hex.rkt" address-hex-string? bytes->hex hex->bytes natural->bytes))

(provide (contract-out
          [address? (-> any/c boolean?)]
          [address->bytes (-> address? bytes?)]
          [address->checksum-string (-> address? string?)]
          [address->integer (-> address? exact-nonnegative-integer?)]
          [bytes->address (-> address-bytes/c checksum-address?)]
          [checksum-address? (-> any/c boolean?)]
          [integer->address (-> address-integer/c checksum-address?)]))

(define address-bytes/c
  (flat-named-contract 'address-bytes/c (lambda (v) (and (bytes? v) (= (bytes-length v) 20)))))
(define address-integer/c
  (flat-named-contract 'address-integer/c (integer-in 0 (sub1 (expt 2 160)))))

(module* private #f
  (provide address->checksum-string
           digest->address)
  (define (digest->address digest)
    (bytes->address (subbytes digest 12))))

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

(define (address->bytes address)
  (hex->bytes address))

(define (bytes->address bs)
  (address->checksum-string (bytes->hex bs)))

(define (address->integer address)
  (string->number (substring address 2) 16))

(define (integer->address n)
  (bytes->address (natural->bytes n 20)))
