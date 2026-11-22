#lang racket/base

(require racket/contract/base
         (only-in racket/string string-join)
         "../private/record.rkt")

(define (log-tail entry [detail ""])
  (format "block ~a index ~a~a~a"
          (or (log-block-number entry) "pending")
          (or (log-index entry) "pending")
          detail
          (if (log-removed? entry) " removed" "")))

(define (write-log entry port _mode)
  (define size (bytes-length (log-data entry)))
  (fprintf port "#<log ~a ~a>"
           (log-address entry)
           (log-tail entry (format " topics [~a] data ~a byte~a"
                                   (string-join (map hash-abbreviation (log-topics entry)) " ")
                                   size
                                   (if (= size 1) "" "s")))))

(define-node-record log #:noun "log" #:printer write-log
  [address address-field]
  [topics (list-field hash-field #:max-length 4)]
  [data data-field]
  [block-number uint64-field #:key blockNumber #:optional]
  [block-hash hash-field #:key blockHash #:optional]
  [block-timestamp lenient-uint64-field #:key blockTimestamp #:optional]
  [transaction-hash hash-field #:key transactionHash #:optional]
  [transaction-index uint64-field #:key transactionIndex #:optional]
  [index uint64-field #:key logIndex #:optional]
  [removed? boolean-field #:key removed #:default #f])

(module* internal #f
  (provide jsexpr->log/who
           log->list
           log-fields
           log-noun
           log-struct
           log-tail))
