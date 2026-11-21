#lang racket/base

(module+ test
  (require rackunit
           (only-in "../helpers/examples.rkt" example-lines))

  (define expected
    '("request eth_chainId"
      "chain id 31337"
      "request eth_getBalance"
      "balance 1 ether"
      "request eth_call"
      "refused: eth_call: execution reverted"
      "the mock answered 3 requests"))

  (test-case "X1: a logging provider passes each request to a mock, which answers or refuses"
    (check-equal? (example-lines "03-custom-providers") expected)))
