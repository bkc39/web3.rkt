#lang racket/base

(module+ test
  (require rackunit
           (only-in web3 current-anvil-path)
           (only-in "../helpers/examples.rkt" example-lines))

  (define expected
    '("Token at 0x5FbDB2315678afecb367f032d93F642f64180aa3"
      "transfer succeeded #t"
      "balances (750 250)"
      "refused: InsufficientBalance(250, 1000)"))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping example 06: anvil not found")]
    [else
     (test-case "X1: a deploy, a call, a send, and a send refused with its custom error"
       (check-equal? (example-lines "06-deploy-and-call") expected))]))
