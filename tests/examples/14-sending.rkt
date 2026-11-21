#lang racket/base

(module+ test
  (require (only-in racket/string string-split)
           rackunit
           (only-in web3/rpc hex-string->bytes)
           (only-in "../helpers/examples.rkt" example-output example-tests)
           (only-in "../helpers/secrets.rkt" check-no-leak))

  (define key
    (hex-string->bytes "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))

  (define expected
    '("counter 0x5FbDB2315678afecb367f032d93F642f64180aa3 count 1"
      "transfer type 2 nonce 2 gas 21000"
      "  sender 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"
      "  balance 1.5 ether"))

  (example-tests "14-sending" #:anvil
    (define output (example-output "14-sending"))

    (test-case "X1: a key the node does not hold deploys, sends and transfers"
      (check-equal? (string-split output "\n") expected))

    (test-case "X2: the output shows no part of the key"
      (check-no-leak output #:secret key))))
