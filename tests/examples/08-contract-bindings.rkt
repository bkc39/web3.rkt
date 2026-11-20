#lang racket/base

(module+ test
  (require rackunit
           (only-in "../helpers/examples.rkt" example-lines example-tests))

  (define expected
    '("GLD: alice 750, bob 250"
      "refused InsufficientBalance (250 1000)"
      "transfer to bob 250"))

  (example-tests "08-contract-bindings" #:anvil
    (test-case "X1: bound procedures deploy, call, send, decode an error and read events"
      (check-equal? (example-lines "08-contract-bindings") expected))))
