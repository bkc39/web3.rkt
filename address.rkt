#lang racket/base

(require racket/contract/base
         (only-in "private/address.rkt"
                  address->checksum-string
                  address->integer
                  address-modulus
                  address?
                  bytes->address
                  checksum-address?
                  integer->address)
         (only-in "private/argument.rkt" address/c argument/c)
         (only-in "private/hex.rkt" hex->bytes))

(provide (contract-out
          [address? (-> any/c boolean?)]
          [rename hex->bytes address->bytes (-> address/c bytes?)]
          [address->checksum-string (-> address/c string?)]
          [address->integer (-> address/c exact-nonnegative-integer?)]
          [bytes->address (-> address-bytes/c checksum-address?)]
          [checksum-address? (-> any/c boolean?)]
          [integer->address (-> address-number/c checksum-address?)]))

(define address-bytes/c
  (argument/c '(and/c bytes? (property/c bytes-length (=/c 20)))
              "an address's bytes"
              (lambda (value) (and (bytes? value) (= (bytes-length value) 20)))
              (lambda (value) (if (bytes? value) "not 20 bytes" "not a byte string"))))

(define address-number/c
  (argument/c '(integer-in 0 (sub1 (expt 2 160)))
              "an address's number"
              (lambda (value) (and (exact-nonnegative-integer? value) (< value address-modulus)))
              (lambda (value)
                (cond
                  [(not (exact-integer? value)) "not an exact integer"]
                  [(negative? value) "negative"]
                  [else "2^160 or more"]))))
