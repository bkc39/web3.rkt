#lang racket/base

(require (only-in "../account.rkt" secp256k1-order))

(provide curve-scalar?
         fee-caps-ordered?
         legacy-v?
         scalar-rules
         v-chain-id
         v-y-parity
         y-parity?)

(define (v-chain-id legacy-v)
  (and (>= legacy-v 35) (quotient (- legacy-v 35) 2)))

(define (v-y-parity legacy-v)
  (if (>= legacy-v 35) (remainder (- legacy-v 35) 2) (- legacy-v 27)))

(define (legacy-v? legacy-v)
  (or (= legacy-v 27)
      (= legacy-v 28)
      (and (>= legacy-v 35) (< (v-chain-id legacy-v) (expt 2 64)))))

(define (curve-scalar? scalar)
  (< 0 scalar secp256k1-order))

(define (y-parity? parity)
  (<= parity 1))

(define (fee-caps-ordered? priority max-fee)
  (<= priority max-fee))

(define scalar-rules
  (hasheq 'u64 (list 8 exact-nonnegative-integer? "~a is 2^64 or more")
          'u256 (list 32 exact-nonnegative-integer? "~a is 2^256 or more")
          'v (list 9 legacy-v?
                   "~a is not 27, 28, or 35 + 2 * chain-id + y-parity with chain-id below 2^64")
          'y-parity (list 1 y-parity? "~a is not 0 or 1")
          'curve-scalar (list 32 curve-scalar? "~a is zero or not less than the curve order")))
