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
    (define tx-hash
      (eth-send-transaction
       (hasheq 'from alice 'to bob 'value (number->hex-string (ether->wei 1)))))
    (define receipt (wait-for-receipt tx-hash))
    (printf "block ~a, gas used ~a, succeeded ~a\n"
            (hex-string->number (hash-ref receipt 'blockNumber))
            (hex-string->number (hash-ref receipt 'gasUsed))
            (receipt-succeeded? receipt))
    (print-accounts alice bob)))
