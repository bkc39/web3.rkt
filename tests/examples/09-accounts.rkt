#lang racket/base

(module+ test
  (require racket/match
           racket/port
           racket/string
           rackunit
           (only-in web3/address checksum-address?))

  (test-case "main prints a fresh key's address and the address its signature recovers"
    (define output
      (with-output-to-string
        (lambda () (dynamic-require '(submod web3/examples/09-accounts main) #f))))
    (match (string-split output "\n" #:trim? #f)
      [(list address recovered "")
       (check-pred checksum-address? address)
       (check-equal? recovered address)]
      [lines (fail (format "not two lines: ~s" lines))])
    (check-false (regexp-match? #px"[0-9a-fA-F]{41}" output) output)))
