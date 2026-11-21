#lang racket/base

(require racket/contract/base
         (only-in racket/match match)
         "../private/exn.rkt"
         (only-in "../private/node.rkt" raise-node-missing)
         "../private/record.rkt"
         (only-in "log.rkt" log?)
         (only-in (submod "log.rkt" internal) jsexpr->log/who))

(provide (contract-out [receipt-succeeded? (-> receipt? boolean?)]))

(define (receipt-outcome receipt)
  (match (receipt-status receipt)
    [#f (string-append "root " (hash-abbreviation (receipt-root receipt)))]
    [status (format "status ~a" status)]))

(define (write-receipt receipt port _mode)
  (fprintf port "#<receipt ~a block ~a index ~a ~a gas used ~a>"
           (receipt-transaction-hash receipt)
           (receipt-block-number receipt)
           (receipt-transaction-index receipt)
           (receipt-outcome receipt)
           (receipt-gas-used receipt)))

(define (check-receipt who detail receipt)
  (unless (or (receipt-status receipt) (receipt-root receipt))
    (raise-node-missing who (string-append "receipt status" detail))))

(define-node-record receipt #:noun "receipt" #:printer write-receipt #:check check-receipt
  [transaction-hash hash-field #:key transactionHash]
  [transaction-index uint64-field #:key transactionIndex]
  [block-hash hash-field #:key blockHash]
  [block-number uint64-field #:key blockNumber]
  [from address-field]
  [to address-field #:optional]
  [type byte-field #:optional]
  [status bit-field #:optional]
  [root hash-field #:optional]
  [cumulative-gas-used uint64-field #:key cumulativeGasUsed]
  [gas-used uint64-field #:key gasUsed]
  [effective-gas-price quantity-field #:key effectiveGasPrice #:optional]
  [contract-address address-field #:key contractAddress #:optional]
  [logs (list-field (record-field jsexpr->log/who log?)) #:unprovided]
  [logs-bloom (fixed-data-field 256) #:key logsBloom]
  [blob-gas-used uint64-field #:key blobGasUsed #:optional]
  [blob-gas-price quantity-field #:key blobGasPrice #:optional])

(define (receipt-succeeded? receipt)
  (match (receipt-status receipt)
    [#f (raise (exn:fail:web3 "receipt-succeeded?: the receipt has a root, not a status"
                              (current-continuation-marks)))]
    [status (eqv? status 1)]))

(define jsexpr->receipt-of
  (asked-reader jsexpr->receipt/who receipt-transaction-hash "receipt transaction hash"))

(module* internal #f
  (provide jsexpr->receipt-of
           jsexpr->receipt/who
           receipt-fields
           receipt-noun
           (rename-out [receipt-logs receipt-log-list])))
