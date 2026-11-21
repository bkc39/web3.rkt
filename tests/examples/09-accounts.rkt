#lang racket/base

(module+ test
  (require racket/match
           rackunit
           (only-in web3/address checksum-address?)
           (only-in "../helpers/examples.rkt" example-lines))

  (test-case "main prints a fresh key's address and the address its signature recovers"
    (match (example-lines "09-accounts")
      [(list address recovered)
       (check-pred checksum-address? address)
       (check-equal? recovered address)]
      [lines (fail (format "not two lines: ~s" lines))])))
