#lang racket/base

(module+ test
  (require racket/port
           (only-in racket/string string-join)
           rackunit)

  (define expected
    '("value 1500000000000000000 wei"
      "fee   0.00063 ether"
      "total 1.50063 ether"
      "token 1234.50"
      "\"1,5\": ether->wei: not a decimal number: unexpected \",\" at index 1"
      "\"0.0000000000000000001\": ether->wei: too many decimals: ether has 18, but decimal 19 is 1"
      "\"\": ether->wei: not a decimal number: no digits"))

  (test-case "X1: main prints amounts in wei and ether, a token amount, and three refusals"
    (check-equal? (with-output-to-string
                    (lambda () (dynamic-require '(submod web3/examples/11-units main) #f)))
                  (string-append (string-join expected "\n") "\n"))))
