#lang racket/base

(require racket/contract
         (only-in racket/match match)
         (only-in "address.rkt" checksum-address?)
         "eth/block.rkt"
         (only-in (submod "eth/block.rkt" internal) jsexpr->block-of jsexpr->fee-history-of)
         "eth/log.rkt"
         "eth/receipt.rkt"
         "eth/transaction-info.rkt"
         (only-in (submod "eth/receipt.rkt" internal) jsexpr->receipt-of jsexpr->receipt/who)
         (only-in (submod "eth/transaction-info.rkt" internal)
                  jsexpr->transaction-info-in
                  jsexpr->transaction-info-of)
         (only-in "private/argument.rkt" address/c hash-argument/c)
         (only-in "private/eth-params.rkt"
                  block-param/c
                  block-param->string
                  filter-object/c
                  objects/c
                  percentiles->jsexpr
                  reward-percentiles/c
                  storage-slot/c
                  storage-slots->jsexpr
                  transaction-object/c)
         (only-in "private/field-contracts.rkt" hash-string/c)
         (only-in "private/hex.rkt" hex-data-string?)
         (only-in "private/node.rkt" node-hash->string node-quantity->number node-quantity?)
         (only-in "private/record.rkt" address-field list-field read-field record-field)
         "private/who.rkt"
         "rpc.rkt")

(provide (all-from-out "eth/block.rkt" "eth/log.rkt" "eth/receipt.rkt" "eth/transaction-info.rkt")
         block-param/c
         block-param->string
         reward-percentiles/c
         storage-slot/c
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
           (->* (hash-argument/c)
                (#:timeout real? #:interval real? #:provider (or/c #f provider?))
                receipt?)]))

(module* internal #f
  (provide wait-for-receipt/who))

(define (empty-data->null v)
  (if (equal? v "0x") 'null v))

(define (quantity->number v)
  (and (node-quantity? v) (node-quantity->number 'quantity->number "quantity" v)))

(define ((nullable reader shape?) who v . arguments)
  (cond
    [(eq? v 'null) #f]
    [(shape? v) (apply reader who v arguments)]
    [else v]))

(define receipts-field (list-field (record-field jsexpr->receipt/who receipt?)))

(define (read-receipts who v _block)
  (read-field receipts-field who "receipts" v))

(define (count-or-null->number v)
  (if (eq? v 'null) 0 (quantity->number v)))

(define ((field-reader kind what) method raw)
  (read-field kind method what raw))

(define (read-transaction-hash method raw _tx)
  (node-hash->string method "transaction hash" raw))

(define (signed-transaction-result result)
  (match result
    [(? string?) result]
    [(hash-table ('raw (? string? raw)) _ ...) raw]))

(define-jsonrpc-endpoint web3_clientVersion (-> string?))
(define-jsonrpc-endpoint web3_sha3 (-> hex-string? hex-string?))
(define-jsonrpc-endpoint net_version (-> string?))
(define-jsonrpc-endpoint net_listening (-> boolean?))
(define-jsonrpc-endpoint net_peerCount (-> exact-nonnegative-integer?)
  #:result-transform quantity->number)
(define-jsonrpc-endpoint eth_protocolVersion (-> string?))
(define-jsonrpc-endpoint eth_syncing (-> (or/c boolean? hash?)))
(define-jsonrpc-endpoint eth_coinbase (-> checksum-address?)
  #:result-reader (field-reader address-field "coinbase"))
(define-jsonrpc-endpoint eth_chainId (-> exact-nonnegative-integer?)
  #:result-transform quantity->number)
(define-jsonrpc-endpoint eth_mining (-> boolean?))
(define-jsonrpc-endpoint eth_hashrate (-> exact-nonnegative-integer?)
  #:result-transform quantity->number)
;; wei
(define-jsonrpc-endpoint eth_gasPrice (-> exact-nonnegative-integer?)
  #:result-transform quantity->number)
(define-jsonrpc-endpoint eth_maxPriorityFeePerGas (-> exact-nonnegative-integer?)
  #:result-transform quantity->number)
(define-jsonrpc-endpoint eth_blobBaseFee (-> exact-nonnegative-integer?)
  #:result-transform quantity->number)
(define-jsonrpc-endpoint eth_feeHistory
  (-> [exact-positive-integer? number->hex-string] [block-param/c block-param->string]
      [reward-percentiles/c percentiles->jsexpr] fee-history?)
  #:result-reader (nullable jsexpr->fee-history-of hash?))
(define-jsonrpc-endpoint eth_accounts (-> (listof checksum-address?))
  #:result-reader (field-reader (list-field address-field) "account"))
(define-jsonrpc-endpoint eth_blockNumber (-> exact-nonnegative-integer?)
  #:result-transform quantity->number)
;; wei
(define-jsonrpc-endpoint eth_getBalance
  (-> address/c [block-param/c block-param->string] exact-nonnegative-integer?)
  #:result-transform quantity->number)
(define-jsonrpc-endpoint eth_getStorageAt
  (-> address/c [storage-slot/c number->hex-string] [block-param/c block-param->string]
      hex-string?))
(define-jsonrpc-endpoint eth_getTransactionCount
  (-> address/c [block-param/c block-param->string] exact-nonnegative-integer?)
  #:result-transform quantity->number)
(define-jsonrpc-endpoint eth_getProof
  (-> address/c [(listof storage-slot/c) storage-slots->jsexpr] [block-param/c block-param->string]
      hash?))
(define-jsonrpc-endpoint eth_getBlockTransactionCountByHash
  (-> hash-argument/c exact-nonnegative-integer?)
  #:result-transform count-or-null->number)
(define-jsonrpc-endpoint eth_getBlockTransactionCountByNumber
  (-> [block-param/c block-param->string] exact-nonnegative-integer?)
  #:result-transform count-or-null->number)
(define-jsonrpc-endpoint eth_getCode
  (-> address/c [block-param/c block-param->string] hex-string?))
(define-jsonrpc-endpoint eth_getBlockByNumber
  (-> [block-param/c block-param->string] #:full-transactions? [boolean? #:default #f]
      (or/c #f block?))
  #:result-reader (nullable jsexpr->block-of hash?))
(define-jsonrpc-endpoint eth_getBlockByHash
  (-> hash-argument/c #:full-transactions? [boolean? #:default #f] (or/c #f block?))
  #:result-reader (nullable jsexpr->block-of hash?))
(define-jsonrpc-endpoint eth_getBlockReceipts
  (-> [block-param/c block-param->string] (or/c #f (listof receipt?)))
  #:result-reader (nullable read-receipts list?))
(define-jsonrpc-endpoint eth_getTransactionByBlockHashAndIndex
  (-> hash-argument/c [exact-nonnegative-integer? number->hex-string] (or/c #f transaction-info?))
  #:result-reader (nullable jsexpr->transaction-info-in hash?))
(define-jsonrpc-endpoint eth_getTransactionByBlockNumberAndIndex
  (-> [block-param/c block-param->string] [exact-nonnegative-integer? number->hex-string]
      (or/c #f transaction-info?))
  #:result-reader (nullable jsexpr->transaction-info-in hash?))

;; Calls and transactions take a transaction object, e.g.
;;   (hasheq 'from a 'to b 'data "0x...")
(define-jsonrpc-endpoint eth_call
  (-> transaction-object/c [block-param/c block-param->string] hex-string?))
(define-jsonrpc-endpoint eth_estimateGas
  (-> transaction-object/c exact-nonnegative-integer?)
  #:result-transform quantity->number)
(define-jsonrpc-endpoint eth_createAccessList
  (-> transaction-object/c [block-param/c block-param->string] hash?))
(define-jsonrpc-endpoint eth_sendTransaction (-> transaction-object/c hash-string/c)
  #:result-reader read-transaction-hash)
(define-jsonrpc-endpoint eth_sendRawTransaction (-> hex-string? hex-string?))
(define-jsonrpc-endpoint eth_sign (-> address/c hex-string? hex-string?))
(define-jsonrpc-endpoint eth_signTransaction (-> transaction-object/c hex-string?)
  #:result-transform signed-transaction-result)
(define-jsonrpc-endpoint eth_getTransactionByHash (-> hash-argument/c (or/c #f transaction-info?))
  #:result-reader (nullable jsexpr->transaction-info-of hash?))
(define-jsonrpc-endpoint eth_getTransactionReceipt (-> hash-argument/c (or/c #f receipt?))
  #:result-reader (nullable jsexpr->receipt-of hash?))
(define-jsonrpc-endpoint eth_getRawTransactionByHash
  (-> hash-argument/c (or/c 'null hex-data-string?))
  #:result-transform empty-data->null)
(define-jsonrpc-endpoint eth_getLogs (-> filter-object/c objects/c))

(define-jsonrpc-endpoint eth_newFilter (-> filter-object/c hex-string?))
(define-jsonrpc-endpoint eth_newBlockFilter (-> hex-string?))
(define-jsonrpc-endpoint eth_newPendingTransactionFilter (-> hex-string?))
(define-jsonrpc-endpoint eth_getFilterChanges (-> hex-string? (listof (or/c hex-string? hash?))))
(define-jsonrpc-endpoint eth_getFilterLogs (-> hex-string? objects/c))
(define-jsonrpc-endpoint eth_uninstallFilter (-> hex-string? boolean?))

(define receipt-poll-interval 0.002)

;; A node may answer eth_sendTransaction before the receipt is queryable.
(define (poll-receipt who tx-hash timeout interval provider)
  (define deadline (+ (current-inexact-milliseconds) (* 1000 timeout)))
  (let loop ([interval interval])
    (define raw (rpc-call "eth_getTransactionReceipt" (list tx-hash) #:provider provider))
    (cond
      [(not (eq? raw 'null)) raw]
      [(> (current-inexact-milliseconds) deadline)
       (raise (exn:fail:web3
               (format "~a: no receipt for ~a after ~as" who tx-hash timeout)
               (current-continuation-marks)))]
      [else
       (sleep interval)
       (loop (min 0.25 (* 2 interval)))])))

(define/who (wait-for-receipt who
                              tx-hash
                              [read-receipt #:public jsexpr->receipt-of]
                              #:timeout [timeout 30.0]
                              #:interval [interval receipt-poll-interval]
                              #:provider [provider (current-provider)])
  (read-receipt who (poll-receipt who tx-hash timeout interval provider) tx-hash))
