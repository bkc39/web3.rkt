#lang racket/base

(require racket/contract/base
         (only-in racket/string string-join)
         (only-in "../private/node.rkt" raise-node-malformed)
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

(define hashes-field (list-field hash-field))

(define (at-most-four? topics)
  (<= (length topics) 4))

(define topics-field
  (field-kind 'topics
              (lambda (who what v)
                (if (and (list? v) (at-most-four? v))
                    ((field-kind-reader hashes-field) who what v)
                    (raise-node-malformed who what v)))
              (and/c (field-kind-contract hashes-field) at-most-four?)))

(define-node-record log #:noun "log" #:printer write-log
  [address address address-field]
  [topics topics topics-field]
  [data data data-field]
  [block-number blockNumber uint64-field #:optional]
  [block-hash blockHash hash-field #:optional]
  [block-timestamp blockTimestamp uint64-field #:optional]
  [transaction-hash transactionHash hash-field #:optional]
  [transaction-index transactionIndex uint64-field #:optional]
  [index logIndex uint64-field #:optional]
  [removed? removed boolean-field #:default #f])

(module* internal #f
  (provide jsexpr->log/who
           log->list
           log-fields
           log-struct
           log-tail))
