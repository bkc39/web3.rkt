#lang racket/base

;; A mock node for the contract tests: the handlers a send meets, and a
;; runner that returns what a thunk gave or raised with the calls it made.

(require web3
         (only-in "../util.rkt" filled-hash insufficient-balance word))

(provide failed
         hex-word
         insufficient-payload
         mined
         reverting
         run-mock
         sending
         token
         tx-hash)

(define token "0x5FbDB2315678afecb367f032d93F642f64180aa3")
(define tx-hash (filled-hash #\a))
(define insufficient-payload (encode-call insufficient-balance 0 5000))
(define mined (hasheq 'status "0x1" 'blockNumber "0x2" 'transactionHash tx-hash 'logs '()))
(define failed (hasheq 'status "0x0" 'blockNumber "0x5" 'transactionHash tx-hash 'logs '()))

(define (hex-word n)
  (bytes->hex-string (word n)))

(define ((reverting method [data (bytes->hex-string insufficient-payload)]) _params)
  (raise-rpc-error method 3 "execution reverted: custom error" data))

(define (run-mock handler thunk)
  (define node (mock-provider handler))
  (define result
    (parameterize ([current-provider node])
      (with-handlers ([(lambda (_) #t) values])
        (call-with-values thunk list))))
  (values result (mock-provider-calls node)))

(define (sending #:estimate [estimate "0x5208"]
                 #:receipt [receipt mined]
                 #:replay [replay "0x"])
  (hash "eth_estimateGas" estimate
        "eth_sendTransaction" tx-hash
        "eth_getTransactionReceipt" receipt
        "eth_call" replay))
