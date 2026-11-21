#lang racket/base

(module+ test
  (require rackunit
           (only-in "../helpers/examples.rkt" example-lines example-tests))

  (define expected
    '("sent from 0x000000000000000000000000000000000000beef, stored 7"
      "reverted, stored 0"))

  (example-tests "02-anvil" #:anvil
    (test-case "X1: an impersonated send stores a value, and reverting the snapshot undoes it"
      (check-equal? (example-lines "02-anvil") expected))))
