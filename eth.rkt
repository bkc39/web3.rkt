#lang racket/base

;; Standard Ethereum JSON-RPC endpoints (web3_*, net_*, eth_*), returning
;; their results directly, plus transaction helpers.
;;
;; Quantities come back as exact integers; data and addresses as "0x..."
;; strings; transactions, receipts and blocks as jsexprs (hasheq).

(require json
         racket/contract
         "rpc.rkt")

(provide block-param/c
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
         eth-accounts
         eth-block-number
         eth-get-balance
         eth-get-storage-at
         eth-get-transaction-count
         eth-get-block-transaction-count-by-hash
         eth-get-block-transaction-count-by-number
         eth-get-code
         eth-get-block-by-number
         eth-call
         eth-estimate-gas
         eth-send-transaction
         eth-send-raw-transaction
         eth-get-transaction-by-hash
         eth-get-transaction-receipt
         eth-get-logs
         wait-for-receipt
         send-transaction/receipt
         receipt-succeeded?)

(define block-param/c
  (or/c exact-nonnegative-integer? "latest" "earliest" "pending" "safe" "finalized"))

(define (block-param->string p)
  (if (exact-nonnegative-integer? p) (number->hex-string p) p))

(define (count-or-null->number r)
  (if (eq? r 'null) 0 (hex-string->number r)))

(define tx/c (and/c hash? jsexpr?))

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
(define-jsonrpc-endpoint eth_getBlockTransactionCountByHash
  (-> string? exact-nonnegative-integer?)
  #:result-transform count-or-null->number)
(define-jsonrpc-endpoint eth_getBlockTransactionCountByNumber
  (-> [block-param/c block-param->string] exact-nonnegative-integer?)
  #:result-transform count-or-null->number)
(define-jsonrpc-endpoint eth_getCode
  (-> string? [block-param/c block-param->string] hex-string?))
(define-jsonrpc-endpoint eth_getBlockByNumber
  (-> [block-param/c block-param->string] boolean? (or/c 'null hash?)))

;; Calls and transactions take a transaction object, e.g.
;;   (hasheq 'from a 'to b 'data "0x...")
(define-jsonrpc-endpoint eth_call
  (-> tx/c [block-param/c block-param->string] hex-string?))
(define-jsonrpc-endpoint eth_estimateGas
  (-> tx/c exact-nonnegative-integer?)
  #:result-transform hex-string->number)
(define-jsonrpc-endpoint eth_sendTransaction (-> tx/c hex-string?))
(define-jsonrpc-endpoint eth_sendRawTransaction (-> hex-string? hex-string?))
(define-jsonrpc-endpoint eth_getTransactionByHash (-> hex-string? (or/c 'null hash?)))
(define-jsonrpc-endpoint eth_getTransactionReceipt (-> hex-string? (or/c 'null hash?)))
(define-jsonrpc-endpoint eth_getLogs (-> (and/c hash? jsexpr?) list?))

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
  (equal? (hash-ref receipt 'status #f) "0x1"))
