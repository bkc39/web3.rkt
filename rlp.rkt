#lang racket/base

(require racket/contract/base
         "private/exn.rkt"
         (only-in "private/hex.rkt" bytes->natural natural->bytes)
         "private/who.rkt")

(provide (struct-out exn:fail:web3:rlp)
         (contract-out
          [rlp-item? (-> any/c boolean?)]
          [rlp-encode (-> rlp-item? bytes?)]
          [rlp-decode (-> bytes? rlp-item?)]
          [natural->rlp-scalar (-> exact-nonnegative-integer? bytes?)]
          [rlp-scalar->natural (-> rlp-item? exact-nonnegative-integer?)]))

(module* internal #f
  (provide rlp-decode/who
           rlp-scalar->natural/who
           rlp-reason/c))

(define rlp-reason/c (or/c 'short 'single-byte 'size 'leading-zero 'trailing 'list))

(define (rlp-item? v)
  (error 'unimplemented))

(define (rlp-encode item)
  (error 'unimplemented))

(define/who (rlp-decode who bs)
  (error 'unimplemented))

(define (natural->rlp-scalar n)
  (error 'unimplemented))

(define/who (rlp-scalar->natural who item)
  (error 'unimplemented))
