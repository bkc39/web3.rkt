#lang racket/base

;; web3: an Ethereum JSON-RPC client for Racket.
;;
;;   web3/rpc     providers, rpc-call, errors, hex helpers, define-jsonrpc-endpoint
;;   web3/eth     web3_* / net_* / eth_* endpoints, receipts
;;   web3/abi     selectors, words, call data, revert reasons
;;   web3/anvil   anvil_* / evm_* / debug_* endpoints, managing anvil processes
;;
;; `(require web3)` provides all of them.

(require "abi.rkt"
         "anvil.rkt"
         "eth.rkt"
         "rpc.rkt")

(provide (all-from-out "abi.rkt"
                       "anvil.rkt"
                       "eth.rkt"
                       "rpc.rkt"))
