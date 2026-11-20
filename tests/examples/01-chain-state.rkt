#lang racket/base

(module+ test
  (require rackunit
           (only-in web3 current-anvil-path)
           (only-in "../helpers/examples.rkt" example-lines))

  (define expected
    '("alice 10000 ether, nonce 0"
      "bob 10000 ether, nonce 0"
      "block 1, gas used 21000, succeeded #t"
      "alice 9998.999958 ether, nonce 1"
      "bob 10001 ether, nonce 0"))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping example 01: anvil not found")]
    [else
     (test-case "X1: one ether moves, the fee is paid and the sender's nonce advances"
       (check-equal? (example-lines "01-chain-state") expected))]))
