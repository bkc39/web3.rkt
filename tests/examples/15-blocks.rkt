#lang racket/base

(module+ test
  (require rackunit
           (only-in "../helpers/examples.rkt" example-lines example-tests))

  (define expected
    '("block 1, gas used 63000, base fee 1 gwei, 3 transactions"
      "  type 0 nonce 0, 1 ether from 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"
      "  type 1 nonce 1, 2 ether from 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"
      "  type 2 nonce 2, 3 ether from 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"
      "legacy transaction rebuilt, signed by 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"
      "block 0, base fee 1 gwei, rewards (0 0) gwei"
      "block 1, base fee 1 gwei, rewards (1 2) gwei"))

  (example-tests "15-blocks" #:anvil
    (test-case "X1: one block holds three kinds of transfer, each read back and its fees summed up"
      (check-equal? (example-lines "15-blocks") expected))))
