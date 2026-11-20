#lang racket/base

(module+ test
  (require rackunit
           (only-in web3 current-anvil-path)
           (only-in "../helpers/examples.rkt" example-lines))

  (define expected
    '("balance (1000)"
      "reverted InsufficientBalance(0, 1)"))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping example 05: anvil not found")]
    [else
     (test-case "X1: a call's result decodes, and its custom error decodes by name"
       (check-equal? (example-lines "05-abi-decode") expected))]))
