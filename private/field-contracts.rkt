#lang racket/base

(require racket/contract/base
         (only-in "../address.rkt" address?))

(provide access-list/c
         bytes32/c
         hash-argument/c
         hash-string/c
         to/c
         uint256/c
         uint64/c)

(define uint64/c (flat-named-contract 'uint64/c (integer-in 0 (sub1 (expt 2 64)))))
(define uint256/c (flat-named-contract 'uint256/c (integer-in 0 (sub1 (expt 2 256)))))
(define bytes32/c
  (flat-named-contract 'bytes32/c (lambda (v) (and (bytes? v) (= (bytes-length v) 32)))))
(define to/c (or/c #f address?))
(define access-list/c
  (flat-named-contract 'access-list/c (listof (cons/c address? (listof bytes32/c)))))
(define hash-string/c
  (flat-named-contract '(and/c string? #px"^0x[0-9a-f]{64}$")
                       (lambda (v) (and (string? v) (regexp-match? #px"^0x[0-9a-f]{64}$" v)))))
(define hash-argument/c
  (flat-named-contract '(and/c string? #px"^0x[0-9a-fA-F]{64}$")
                       (lambda (v) (and (string? v) (regexp-match? #px"^0x[0-9a-fA-F]{64}$" v)))))
