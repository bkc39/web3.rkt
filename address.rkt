#lang racket/base

(require racket/contract/base
         (only-in keccak keccak256)
         "abi/type.rkt")

(provide (contract-out
          [address->checksum-string (-> (abi-value/c 'address) string?)]
          [checksum-address? (-> any/c boolean?)]))

(module* private #f
  (require (only-in "private/hex.rkt" bytes->hex))
  (provide digest->address)
  (define (digest->address digest)
    (address->checksum-string (bytes->hex (subbytes digest 12)))))

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

(define (checksum-address? v)
  (and (string? v)
       (regexp-match? #px"^0x[0-9a-fA-F]{40}$" v)
       (string=? v (address->checksum-string v))))
