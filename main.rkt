#lang racket/base

;; web3: an Ethereum JSON-RPC client for Racket.
;;
;;   web3/rpc             providers, rpc-call, errors, hex helpers, define-jsonrpc-endpoint
;;   web3/eth             web3_* / net_* / eth_* endpoints, receipts
;;   web3/abi             standard and packed encodings, decoders, selectors, call data, reverts
;;   web3/abi/type        ABI types as structs, strings and s-expressions; signatures
;;   web3/abi/event-type  events as structs, strings and s-expressions
;;   web3/abi/description ABI items and contract descriptions from strings, s-expressions and JSON
;;   web3/address         EIP-55 checksums
;;   web3/account         secret keys, addresses, signing digests, recovering signers
;;   web3/contract        deploying, calling and sending; contract instances; define-contract;
;;                        CREATE and CREATE2 addresses
;;   web3/event           event logs: filters, get-logs, receipt-logs
;;   web3/abi/event       event topics and log decoding
;;   web3/eth/log         logs as structs
;;   web3/anvil           anvil_* / evm_* / debug_* endpoints, managing anvil processes
;;
;; `(require web3)` provides all of them.

(require "abi.rkt"
         "account.rkt"
         "address.rkt"
         "anvil.rkt"
         "contract.rkt"
         "eth.rkt"
         "event.rkt"
         "rpc.rkt")

(provide (all-from-out "abi.rkt"
                       "account.rkt"
                       "address.rkt"
                       "anvil.rkt"
                       "contract.rkt"
                       "eth.rkt"
                       "event.rkt"
                       "rpc.rkt"))
