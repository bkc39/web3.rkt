#lang racket/base

(require racket/contract/base
         (only-in "eth/receipt.rkt" receipt?)
         (only-in "private/contracts.rkt" sender/c)
         "private/exn.rkt"
         (only-in "private/send.rkt"
                  fill-transaction
                  filling->
                  send-signed-transaction
                  send-signed-transaction/receipt)
         (only-in (submod "signer.rkt" internal) signer/c)
         (only-in "transaction.rkt" transaction?)
         (only-in (submod "transaction.rkt" internal) transaction-hash/c))

(provide (struct-out exn:fail:web3:fill)
         (contract-out
          [fill-transaction (filling-> sender/c transaction?)]
          [send-signed-transaction (filling-> signer/c transaction-hash/c)]
          [send-signed-transaction/receipt (filling-> signer/c receipt?)]))
