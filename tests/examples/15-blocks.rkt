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
      "fee history, block 0: priority fees 0 and 0 gwei"
      "fee history, block 1: priority fees 1 and 2 gwei"
      "next base fee 0.875525 gwei"))

  (example-tests "15-blocks" #:anvil
    (test-case "X1: one block holds three kinds of transfer, one is rebuilt, a fee history reads back"
      (check-equal? (example-lines "15-blocks") expected))))
