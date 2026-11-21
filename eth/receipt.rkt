#lang racket/base

(require racket/contract/base
         (only-in racket/match match)
         "../private/exn.rkt"
         (only-in "../private/node.rkt" raise-node-malformed)
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
    (raise-node-malformed who (string-append "receipt status" detail) 'null)))

(define-node-record receipt #:noun "receipt" #:printer write-receipt #:check check-receipt
  [transaction-hash transactionHash hash-field]
  [transaction-index transactionIndex uint64-field]
  [block-hash blockHash hash-field]
  [block-number blockNumber uint64-field]
  [from from address-field]
  [to to address-field #:optional]
  [type type byte-field #:optional]
  [status status bit-field #:optional]
  [root root hash-field #:optional]
  [cumulative-gas-used cumulativeGasUsed uint64-field]
  [gas-used gasUsed uint64-field]
  [effective-gas-price effectiveGasPrice quantity-field]
  [contract-address contractAddress address-field #:optional]
  [logs logs (list-field (record-field jsexpr->log/who log?)) #:unprovided]
  [logs-bloom logsBloom (fixed-data-field 256)]
  [blob-gas-used blobGasUsed uint64-field #:optional]
  [blob-gas-price blobGasPrice quantity-field #:optional])

(define (receipt-succeeded? receipt)
  (match (receipt-status receipt)
    [#f (raise (exn:fail:web3 "receipt-succeeded?: the receipt has a root, not a status"
                              (current-continuation-marks)))]
    [status (eqv? status 1)]))

(define (jsexpr->receipt-of who tx-hash v [detail ""])
  (define receipt (jsexpr->receipt/who who v detail))
  (define received (receipt-transaction-hash receipt))
  (if (equal? received (string-downcase tx-hash))
      receipt
      (raise-node-malformed who (string-append "receipt transaction hash" detail) received)))

(module* internal #f
  (provide jsexpr->receipt-of
           jsexpr->receipt/who
           receipt-fields
           (rename-out [receipt-logs receipt-log-list])))
