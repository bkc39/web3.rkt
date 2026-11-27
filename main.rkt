#lang racket/base

;; web3: an Ethereum JSON-RPC client for Racket.
;;
;;   web3/rpc             providers, rpc-call, errors, hex helpers, define-jsonrpc-endpoint
;;   web3/eth             web3_* / net_* / eth_* endpoints, receipts, blocks, transactions
;;   web3/abi             standard and packed encodings, decoders, selectors, call data, reverts
;;   web3/abi/type        ABI types as structs, strings and s-expressions; signatures
;;   web3/abi/event-type  events as structs, strings and s-expressions
;;   web3/abi/description ABI items and contract descriptions from strings, s-expressions and JSON
;;   web3/halt            exceptional halts as symbols, from a node's or a tracer's words
;;   web3/address         EIP-55 checksums; addresses as bytes and integers
;;   web3/account         secret keys, addresses, signing digests and EIP-191 messages,
;;                        recovering signers
;;   web3/rlp             Recursive Length Prefix: encoding, strict decoding, scalars
;;   web3/units           amounts in wei, gwei, ether or a token's decimals, from and to
;;                        decimal strings
;;   web3/transaction     legacy, EIP-2930 and EIP-1559 transactions: build, sign, raw bytes, parse;
;;                        intrinsic and floor gas
;;   web3/signer          signers: local keys, node accounts, gen:signer
;;   web3/send            filling transactions from a node, signing and sending them; sends
;;                        the node signs
;;   web3/trace           call traces as structs; debug_* and trace_* endpoints
;;   web3/contract        deploying, calling and sending; contract instances; define-contract;
;;                        CREATE and CREATE2 addresses
;;   web3/event           event logs: filters, get-logs, receipt-logs
;;   web3/abi/event       event topics and log decoding
;;   web3/eth/log         logs as structs
;;   web3/eth/receipt     receipts as structs
;;   web3/eth/block       blocks, withdrawals and fee histories as structs
;;   web3/eth/transaction-info  transactions as a node describes them
;;   web3/anvil           anvil_* / evm_* endpoints, debug_* from web3/trace, managing anvil
;;                        processes
;;
;; `(require web3)` provides all of them, and keccak's keccak256.

(require (only-in keccak keccak256)
         "abi.rkt"
         "account.rkt"
         "address.rkt"
         "anvil.rkt"
         "contract.rkt"
         "eth.rkt"
         "event.rkt"
         "halt.rkt"
         "rlp.rkt"
         "rpc.rkt"
         "send.rkt"
         "signer.rkt"
         "trace.rkt"
         "transaction.rkt"
         "units.rkt")

(provide keccak256
         (all-from-out "abi.rkt"
                       "account.rkt"
                       "address.rkt"
                       "anvil.rkt"
                       "contract.rkt"
                       "eth.rkt"
                       "event.rkt"
                       "halt.rkt"
                       "rlp.rkt"
                       "rpc.rkt"
                       "send.rkt"
                       "signer.rkt"
                       "trace.rkt"
                       "transaction.rkt"
                       "units.rkt"))
