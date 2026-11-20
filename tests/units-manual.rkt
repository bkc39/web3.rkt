#lang racket/base

(module+ test
  (require rackunit
           "helpers/manual.rkt")

  (define blocks
    '(([(ether->wei "1.5") (value 1500000000000000000)]
       [(gwei->wei 30) (value 30000000000)]
       [(wei->string (* 21000 (gwei->wei 30))) (value "0.00063")])
      ([(ether->wei 3/2) (value 1500000000000000000)]
       [(ether->wei "0.0000000000000000001")
        (raises "ether->wei: too many decimals: ether has 18, but decimal 19 is 1")]
       [(ether->wei 1.5) (blames ether->wei)])
      ([(units->wei "1234.5" 6) (value 1234500000)]
       [(wei->string 1234500000 6 #:min-fraction-digits 2) (value "1234.50")])
      ([(units->wei "0.25" 'gwei) (value 250000000)]
       [(units->wei "-.5" 6) (value -500000)]
       [(units->wei "1.500000000" 6) (value 1500000)]
       [(units->wei "1.0000005" 6)
        (raises "units->wei: too many decimals: the unit has 6, but decimal 7 is 5")]
       [(units->wei "1,5" 6) (raises "units->wei: not a decimal number: unexpected \",\" at index 1")])
      ([(gwei->wei "1.5") (value 1500000000)]
       [(ether->wei 1/1000) (value 1000000000000000)])
      ([(wei->string 1500000000000000000) (value "1.5")]
       [(wei->string 30000000000 'gwei) (value "30")]
       [(wei->string -1 'gwei) (value "-0.000000001")]
       [(wei->string 1500000 6 #:min-fraction-digits 2) (value "1.50")]
       [(wei->string (ether->wei 1) #:min-fraction-digits 18) (value "1.000000000000000000")])
      ([(hex-string->number "0x10") (value 16)]
       [(hex-string->number "10") (blames hex-string->number)])))

  (test-case "M1: the manual's units and hex-quantity examples give what the manual shows"
    (check-manual-blocks blocks)))
