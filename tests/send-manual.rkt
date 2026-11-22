#lang racket/base

(module+ test
  (require (only-in racket/file file->string)
           (only-in racket/match match-define)
           (only-in racket/string string-split)
           rackunit
           (only-in web3/private/send fill-reasons)
           "helpers/manual.rkt")

  (define sent-hash "0x353911be23e52e6b62f32b2323be63d06541c4ab17382cde9ff87b3174c3c9f6")

  (define expectations
    `(((void)
       (void)
       (void)
       (void)
       (void)
       (void)
       (prints "#<eip1559-transaction chain-id 31337 nonce 0>")
       (value 3000000000))
      ((value ,sent-hash))
      ((raises "fill-transaction: wrong chain: #:chain-id is 1, but the node's chain is 31337")
       (raises "fill-transaction: #:type 0 takes no #:access-list")
       (void)
       (raises ,(string-append "fill-transaction: no base fee: the latest block has no base fee, "
                               "so a type 2 transaction needs #:max-fee-per-gas")))
      ((void)
       (value ,sent-hash)
       (value (("eth_estimateGas"
                #hasheq((from . "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")
                        (to . "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")
                        (data . "0x")
                        (value . "0x14d1120d7b160000")))
               ("eth_sendTransaction"
                #hasheq((from . "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")
                        (to . "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")
                        (data . "0x")
                        (value . "0x14d1120d7b160000")
                        (gas . "0x5208"))))))))

  (test-case "M1: the manual's sending examples give what the manual shows"
    (check-manual-examples "send-eval" expectations))

  (test-case "M2: the manual's exn:fail:web3:fill reasons are the ones raised"
    (define manual
      (file->string (collection-file-path "scribblings/web3.scrbl" "web3")))
    (match-define (list _ documented)
      (regexp-match #px"exn:fail:web3:fill exn:fail:web3\\) \\(\\[reason \\(or/c ([^)]*)\\)" manual))
    (check-equal? (sort (map (lambda (s) (string->symbol (substring s 1)))
                             (string-split documented))
                        symbol<?)
                  (sort fill-reasons symbol<?))))
