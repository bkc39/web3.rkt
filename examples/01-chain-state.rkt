#lang racket/base

(require racket/match
         web3)

(define (print-accounts alice bob)
  (printf "alice nonce ~a, bob ~a ether\n"
          (eth-get-transaction-count alice "latest")
          (wei->string (eth-get-balance bob "latest"))))

(module+ main
  (with-anvil ()
    (match-define (list alice bob _ ...) (eth-accounts))
    (print-accounts alice bob)
    (define receipt (send-transaction/receipt alice bob #:value (ether->wei 1)))
    (printf "block ~a, gas used ~a, succeeded ~a\n"
            (receipt-block-number receipt)
            (receipt-gas-used receipt)
            (receipt-succeeded? receipt))
    (print-accounts alice bob)))
