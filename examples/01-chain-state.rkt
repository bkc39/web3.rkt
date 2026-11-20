#lang racket/base

(require racket/match
         web3)

(define (print-account name address)
  (printf "~a ~a ether, nonce ~a\n"
          name
          (wei->string (eth-get-balance address "latest"))
          (eth-get-transaction-count address "latest")))

(module+ main
  (with-anvil ()
    (match-define (list alice bob _ ...) (eth-accounts))
    (print-account "alice" alice)
    (print-account "bob" bob)
    (define tx-hash
      (eth-send-transaction
       (hasheq 'from alice 'to bob 'value (number->hex-string (ether->wei 1)))))
    (define receipt (wait-for-receipt tx-hash))
    (printf "block ~a, gas used ~a, succeeded ~a\n"
            (hex-string->number (hash-ref receipt 'blockNumber))
            (hex-string->number (hash-ref receipt 'gasUsed))
            (receipt-succeeded? receipt))
    (print-account "alice" alice)
    (print-account "bob" bob)))
