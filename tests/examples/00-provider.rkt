#lang racket/base

(module+ test
  (require rackunit
           (only-in "../helpers/examples.rkt" example-lines example-tests))

  (define expected
    '("chain id 31337"
      "block 0"
      "eth_chainId \"0x7a69\""
      "uncles in block 0: 0"
      "refused -32601: Method not found"))

  (example-tests "00-provider" #:anvil
    (test-case "X1: endpoints, a raw call, a defined endpoint and a node's error"
      (check-equal? (example-lines "00-provider") expected))))
