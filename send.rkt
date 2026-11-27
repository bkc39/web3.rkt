#lang racket/base

(require racket/contract/base
         (only-in "eth/receipt.rkt" receipt?)
         (only-in "private/contracts.rkt" from/c sender/c)
         (only-in "private/eth-params.rkt" block-param/c block-param->string)
         "private/exn.rkt"
         (only-in "private/send.rkt"
                  call-order
                  call-transaction
                  define/filling
                  fill-transaction
                  filling->
                  node-gas-estimate
                  send-signed-transaction
                  send-signed-transaction/receipt
                  send-transaction
                  send-transaction/receipt)
         (only-in (submod "signer.rkt" internal) signer/c)
         (only-in "rpc.rkt" rpc-call)
         (only-in "transaction.rkt" transaction?)
         (only-in (submod "transaction.rkt" internal) transaction-hash/c))

(provide (struct-out exn:fail:web3:fill)
         (contract-out
          [call-transaction (filling-> from/c [#:block block-param/c] bytes?)]
          [estimate-transaction
           (filling-> from/c [#:block block-param/c] exact-nonnegative-integer?)]
          [fill-transaction (filling-> sender/c transaction?)]
          [send-signed-transaction (filling-> signer/c transaction-hash/c)]
          [send-signed-transaction/receipt (filling-> signer/c receipt?)]
          [send-transaction (filling-> sender/c transaction-hash/c)]
          [send-transaction/receipt (filling-> sender/c receipt?)]))

(define/filling (estimate-transaction who from to order #:block [block "latest"])
  (define (request tx provider)
    (rpc-call "eth_estimateGas" (list tx (block-param->string block)) #:provider provider))
  (node-gas-estimate who (call-order who from to order request)))
