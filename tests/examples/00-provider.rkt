#lang racket/base

(module+ test
  (require rackunit
           (only-in web3 current-anvil-path)
           (only-in "../helpers/examples.rkt" example-lines))

  (define expected
    '("chain id 31337"
      "block 0"
      "eth_chainId \"0x7a69\""
      "uncles in block 0: 0"
      "eth_noSuchMethod error -32601"))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping example 00: anvil not found")]
    [else
     (test-case "X1: endpoints, a raw call, a defined endpoint and a node's error code"
       (check-equal? (example-lines "00-provider") expected))]))
