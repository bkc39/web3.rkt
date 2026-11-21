#lang racket/base

(module+ test
  (require rackunit
           (only-in "../helpers/examples.rkt" example-lines example-tests))

  (define expected
    '("alice nonce 0, bob 10000 ether"
      "block 1, gas used 21000, succeeded #t"
      "alice nonce 1, bob 10001 ether"))

  (example-tests "01-chain-state" #:anvil
    (test-case "X1: one ether moves to bob, and alice's nonce advances"
      (check-equal? (example-lines "01-chain-state") expected))))
