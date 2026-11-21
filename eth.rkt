#lang racket/base

;; Standard Ethereum JSON-RPC endpoints (web3_*, net_*, eth_*), returning
;; their results directly.
;;
;; Quantities come back as exact integers; data and addresses as "0x..."
;; strings; transactions, receipts and blocks as jsexprs (hasheq).

(require json
         racket/contract
         (only-in racket/match match)
         "eth/log.rkt"
         (only-in "private/hex.rkt" bytes->hex hex-data-string? natural->bytes)
         (only-in "private/node.rkt" node-quantity->number receipt-status->boolean)
         "rpc.rkt")

(provide (all-from-out "eth/log.rkt")
         block-param/c
         block-param->string
         web3-client-version
         web3-sha3
         net-version
         net-listening
         net-peer-count
         eth-protocol-version
         eth-syncing
         eth-coinbase
         eth-chain-id
         eth-mining
         eth-hashrate
         eth-gas-price
         eth-max-priority-fee-per-gas
         eth-blob-base-fee
         eth-fee-history
         eth-accounts
         eth-block-number
         eth-get-balance
         eth-get-storage-at
         eth-get-transaction-count
         eth-get-proof
         eth-get-block-transaction-count-by-hash
         eth-get-block-transaction-count-by-number
         eth-get-code
         eth-get-block-by-number
         eth-get-block-by-hash
         eth-get-block-receipts
         eth-get-transaction-by-block-hash-and-index
         eth-get-transaction-by-block-number-and-index
         eth-call
         eth-estimate-gas
         eth-create-access-list
         eth-send-transaction
         eth-send-raw-transaction
         eth-sign
         eth-sign-transaction
         eth-get-transaction-by-hash
         eth-get-transaction-receipt
         eth-get-raw-transaction-by-hash
         eth-get-logs
         eth-new-filter
         eth-new-block-filter
         eth-new-pending-transaction-filter
         eth-get-filter-changes
         eth-get-filter-logs
         eth-uninstall-filter
         (contract-out
          [wait-for-receipt
           (->* (hex-string?)
                (#:timeout real? #:interval real? #:provider (or/c #f provider?))
                hash?)]
          [send-transaction/receipt (->* (tx/c) (#:provider (or/c #f provider?)) hash?)]
          [receipt-succeeded? (-> hash? boolean?)]))

(define block-param/c
  (flat-named-contract
   'block-param/c
   (or/c exact-nonnegative-integer? "latest" "earliest" "pending" "safe" "finalized")))

(define (block-param->string p)
  (if (exact-nonnegative-integer? p) (number->hex-string p) p))

(define (non-decreasing? xs)
  (or (null? xs) (apply <= xs)))

(define reward-percentiles/c
  (flat-named-contract 'reward-percentiles/c (and/c (listof (real-in 0 100)) non-decreasing?)))

(define storage-slot/c (flat-named-contract 'storage-slot/c (integer-in 0 (sub1 (expt 2 256)))))

(define logs/c (listof hash?))

(define receipts/c (listof hash?))

(define (percentiles->jsexpr percentiles)
  (error 'percentiles->jsexpr "unimplemented"))

(define (storage-slots->jsexpr slots)
  (error 'storage-slots->jsexpr "unimplemented"))

(define (empty-data->null v)
  (error 'empty-data->null "unimplemented"))

(define (quantity->number v)
  (error 'quantity->number "unimplemented"))

(define (count-or-null->number r)
  (if (eq? r 'null) 0 (hex-string->number r)))

(define tx/c (and/c hash? jsexpr?))

(define filter/c (and/c hash? jsexpr?))

(define (signed-transaction-result result)
  (match result
    [(? string?) result]
    [(hash-table ('raw (? string? raw)) _ ...) raw]))

(define-jsonrpc-endpoint web3_clientVersion (-> string?))
(define-jsonrpc-endpoint web3_sha3 (-> hex-string? hex-string?))
(define-jsonrpc-endpoint net_version (-> string?))
(define-jsonrpc-endpoint net_listening (-> boolean?))
(define-jsonrpc-endpoint net_peerCount (-> exact-nonnegative-integer?)
  #:result-transform hex-string->number)
(define-jsonrpc-endpoint eth_protocolVersion (-> string?))
(define-jsonrpc-endpoint eth_syncing (-> (or/c boolean? hash?)))
(define-jsonrpc-endpoint eth_coinbase (-> string?))
(define-jsonrpc-endpoint eth_chainId (-> exact-nonnegative-integer?)
  #:result-transform hex-string->number)
(define-jsonrpc-endpoint eth_mining (-> boolean?))
(define-jsonrpc-endpoint eth_hashrate (-> exact-nonnegative-integer?)
  #:result-transform hex-string->number)
;; wei
(define-jsonrpc-endpoint eth_gasPrice (-> exact-nonnegative-integer?)
  #:result-transform hex-string->number)
(define-jsonrpc-endpoint eth_maxPriorityFeePerGas (-> exact-nonnegative-integer?)
  #:result-transform hex-string->number)
(define-jsonrpc-endpoint eth_blobBaseFee (-> exact-nonnegative-integer?)
  #:result-transform quantity->number)
(define-jsonrpc-endpoint eth_feeHistory
  (-> [exact-positive-integer? number->hex-string] [block-param/c block-param->string]
      [reward-percentiles/c percentiles->jsexpr] hash?))
(define-jsonrpc-endpoint eth_accounts (-> (listof string?)))
(define-jsonrpc-endpoint eth_blockNumber (-> exact-nonnegative-integer?)
  #:result-transform hex-string->number)
;; wei
(define-jsonrpc-endpoint eth_getBalance
  (-> string? [block-param/c block-param->string] exact-nonnegative-integer?)
  #:result-transform hex-string->number)
(define-jsonrpc-endpoint eth_getStorageAt
  (-> string? [exact-nonnegative-integer? number->hex-string] [block-param/c block-param->string]
      hex-string?))
(define-jsonrpc-endpoint eth_getTransactionCount
  (-> string? [block-param/c block-param->string] exact-nonnegative-integer?)
  #:result-transform hex-string->number)
(define-jsonrpc-endpoint eth_getProof
  (-> string? [(listof storage-slot/c) storage-slots->jsexpr] [block-param/c block-param->string]
      hash?))
(define-jsonrpc-endpoint eth_getBlockTransactionCountByHash
  (-> hex-string? exact-nonnegative-integer?)
  #:result-transform count-or-null->number)
(define-jsonrpc-endpoint eth_getBlockTransactionCountByNumber
  (-> [block-param/c block-param->string] exact-nonnegative-integer?)
  #:result-transform count-or-null->number)
(define-jsonrpc-endpoint eth_getCode
  (-> string? [block-param/c block-param->string] hex-string?))
(define-jsonrpc-endpoint eth_getBlockByNumber
  (-> [block-param/c block-param->string] boolean? (or/c 'null hash?)))
(define-jsonrpc-endpoint eth_getBlockByHash (-> hex-string? boolean? (or/c 'null hash?)))
(define-jsonrpc-endpoint eth_getBlockReceipts
  (-> [block-param/c block-param->string] (or/c 'null receipts/c)))
(define-jsonrpc-endpoint eth_getTransactionByBlockHashAndIndex
  (-> hex-string? [exact-nonnegative-integer? number->hex-string] (or/c 'null hash?)))
(define-jsonrpc-endpoint eth_getTransactionByBlockNumberAndIndex
  (-> [block-param/c block-param->string] [exact-nonnegative-integer? number->hex-string]
      (or/c 'null hash?)))

;; Calls and transactions take a transaction object, e.g.
;;   (hasheq 'from a 'to b 'data "0x...")
(define-jsonrpc-endpoint eth_call
  (-> tx/c [block-param/c block-param->string] hex-string?))
(define-jsonrpc-endpoint eth_estimateGas
  (-> tx/c exact-nonnegative-integer?)
  #:result-transform hex-string->number)
(define-jsonrpc-endpoint eth_createAccessList (-> tx/c [block-param/c block-param->string] hash?))
(define-jsonrpc-endpoint eth_sendTransaction (-> tx/c hex-string?))
(define-jsonrpc-endpoint eth_sendRawTransaction (-> hex-string? hex-string?))
(define-jsonrpc-endpoint eth_sign (-> string? hex-string? hex-string?))
(define-jsonrpc-endpoint eth_signTransaction (-> tx/c hex-string?)
  #:result-transform signed-transaction-result)
(define-jsonrpc-endpoint eth_getTransactionByHash (-> hex-string? (or/c 'null hash?)))
(define-jsonrpc-endpoint eth_getTransactionReceipt (-> hex-string? (or/c 'null hash?)))
(define-jsonrpc-endpoint eth_getRawTransactionByHash
  (-> hex-string? (or/c 'null hex-data-string?))
  #:result-transform empty-data->null)
(define-jsonrpc-endpoint eth_getLogs (-> filter/c logs/c))

(define-jsonrpc-endpoint eth_newFilter (-> filter/c hex-string?))
(define-jsonrpc-endpoint eth_newBlockFilter (-> hex-string?))
(define-jsonrpc-endpoint eth_newPendingTransactionFilter (-> hex-string?))
(define-jsonrpc-endpoint eth_getFilterChanges (-> hex-string? (listof (or/c hex-string? hash?))))
(define-jsonrpc-endpoint eth_getFilterLogs (-> hex-string? logs/c))
(define-jsonrpc-endpoint eth_uninstallFilter (-> hex-string? boolean?))

;; Poll until the receipt exists.  Nodes (anvil included) may return
;; eth_sendTransaction before the receipt is queryable.
(define (wait-for-receipt tx-hash
                          #:timeout [timeout 30.0]
                          #:interval [interval 0.002]
                          #:provider [provider (current-provider)])
  (define deadline (+ (current-inexact-milliseconds) (* 1000 timeout)))
  (let loop ([interval interval])
    (define r (eth-get-transaction-receipt tx-hash #:provider provider))
    (cond
      [(hash? r) r]
      [(> (current-inexact-milliseconds) deadline)
       (raise (exn:fail:web3 (format "wait-for-receipt: no receipt for ~a after ~as" tx-hash timeout)
                             (current-continuation-marks)))]
      [else
       (sleep interval)
       (loop (min 0.25 (* 2 interval)))])))

(define (send-transaction/receipt tx #:provider [provider (current-provider)])
  (wait-for-receipt (eth-send-transaction tx #:provider provider) #:provider provider))

(define (receipt-succeeded? receipt)
  (receipt-status->boolean 'receipt-succeeded? (hash-ref receipt 'status #f)))
