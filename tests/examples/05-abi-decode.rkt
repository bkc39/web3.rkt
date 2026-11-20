#lang racket/base

(module+ test
  (require rackunit
           (only-in "../helpers/examples.rkt" example-lines))

  (define expected
    '("result (\"Token\" 1000)"
      "call (\"0x70997970C51812dc3A010C7d01b50e0d17dc79C8\" 250)"
      "revert Error(\"not the owner\")"
      "revert Panic(0x11): arithmetic underflow or overflow"
      "revert InsufficientBalance(250, 1000)"
      "refused: decode-call: data too short: the head needs 64 bytes, 32 remain"
      "  type: (address,uint256)"
      "  offset: 4"))

  (test-case "X1: a result, call data and three reverts decode, and truncated data is refused"
    (check-equal? (example-lines "05-abi-decode") expected)))
