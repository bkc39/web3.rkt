#lang racket/base

(require (only-in json jsexpr?)
         racket/contract/base
         syntax/parse/define
         (only-in "abi/type.rkt" abi-signature? abi-value/c)
         "private/exn.rkt"
         (only-in "private/opaque.rkt" opaque->)
         (only-in "private/send.rkt"
                  fill-transaction
                  send-signed-transaction
                  send-signed-transaction/receipt
                  sender/c)
         (only-in "rpc.rkt" provider?)
         (only-in (submod "signer.rkt" internal) signer/c)
         (only-in "transaction.rkt" access-list/c transaction? uint256/c uint64/c)
         (only-in (submod "transaction.rkt" internal) transaction-hash/c))

(define to/c (or/c #f (abi-value/c 'address)))
(define errors/c (listof (or/c string? abi-signature?)))
(define provider/c (or/c #f provider?))
(define receipt/c (and/c hash? jsexpr?))

(define-syntax-parse-rule (filling-> first/c:expr result/c:expr)
  (opaque-> first/c to/c
            [#:value uint256/c]
            [#:data bytes?]
            [#:nonce (or/c #f uint64/c)]
            [#:gas-limit (or/c #f uint64/c)]
            [#:chain-id (or/c #f uint64/c)]
            [#:type (or/c #f 0 1 2)]
            [#:gas-price (or/c #f uint256/c)]
            [#:max-priority-fee-per-gas (or/c #f uint256/c)]
            [#:max-fee-per-gas (or/c #f uint256/c)]
            [#:access-list (or/c #f access-list/c)]
            [#:errors errors/c]
            [#:provider provider/c]
            result/c))

(provide (struct-out exn:fail:web3:fill)
         (contract-out
          [fill-transaction (filling-> sender/c transaction?)]
          [send-signed-transaction (filling-> signer/c transaction-hash/c)]
          [send-signed-transaction/receipt (filling-> signer/c receipt/c)]))
