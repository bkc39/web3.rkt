#lang racket/base

(module+ test
  (require rackunit
           (only-in "../helpers/examples.rkt" example-lines example-tests))

  (define expected
    '("Token at 0x5FbDB2315678afecb367f032d93F642f64180aa3"
      "transfer succeeded #t"
      "balances (750 250)"
      "refused: InsufficientBalance(250, 1000)"))

  (example-tests "06-deploy-and-call" #:anvil
    (test-case "X1: a deploy, a call, a send, and a send refused with its custom error"
      (check-equal? (example-lines "06-deploy-and-call") expected))))
