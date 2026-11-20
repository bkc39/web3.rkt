#lang racket/base

(module+ test
  (require rackunit
           (only-in web3 current-anvil-path)
           (only-in "../helpers/examples.rkt" example-lines))

  (define expected
    '("GLD: alice 750, bob 250"
      "refused InsufficientBalance (250 1000)"
      "transfer to bob 250"))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping example 08: anvil not found")]
    [else
     (test-case "X1: bound procedures deploy, call, send, decode an error and read events"
       (check-equal? (example-lines "08-contract-bindings") expected))]))
