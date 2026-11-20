#lang racket/base

(module+ test
  (require rackunit
           (only-in web3 current-anvil-path)
           (only-in "../helpers/examples.rkt" example-lines))

  (define zero "0x0000000000000000000000000000000000000000")
  (define alice "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")
  (define bob "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")
  (define carol "0x3C44CdDdB6a900fa2b585dd299e03d12FA4293BC")

  (define expected
    `("receipt"
      ,(format "  ~a -> ~a 250" alice bob)
      "every transfer"
      ,(format "  ~a -> ~a 1000" zero alice)
      ,(format "  ~a -> ~a 250" alice bob)
      ,(format "  ~a -> ~a 100" alice carol)
      "to bob"
      ,(format "  ~a -> ~a 250" alice bob)))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping example 07: anvil not found")]
    [else
     (test-case "X1: a receipt's transfer decodes, and get-logs finds every transfer and bob's"
       (check-equal? (example-lines "07-events") expected))]))
