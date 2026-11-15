#lang racket/base

(require racket/contract/base
         (only-in racket/string string-join)
         "../private/node.rkt"
         "../private/who.rkt")

(provide
 (contract-out
  [log? (-> any/c boolean?)]
  [log-address (-> log? string?)]
  [log-topics (-> log? (listof string?))]
  [log-data (-> log? bytes?)]
  [log-block-number (-> log? (or/c #f exact-nonnegative-integer?))]
  [log-block-hash (-> log? (or/c #f string?))]
  [log-block-timestamp (-> log? (or/c #f exact-nonnegative-integer?))]
  [log-transaction-hash (-> log? (or/c #f string?))]
  [log-transaction-index (-> log? (or/c #f exact-nonnegative-integer?))]
  [log-index (-> log? (or/c #f exact-nonnegative-integer?))]
  [log-removed? (-> log? boolean?)]
  [jsexpr->log (-> any/c log?)]
  [log->jsexpr (-> log? hash?)]))

(define (topic-abbreviation topic)
  (string-append (substring topic 0 10) "…"))

(define (log-tail l [detail ""])
  (format "block ~a index ~a~a~a"
          (or (log-block-number l) "pending")
          (or (log-index l) "pending")
          detail
          (if (log-removed? l) " removed" "")))

(define (write-log l port _mode)
  (define n (bytes-length (log-data l)))
  (fprintf port "#<log ~a ~a>"
           (log-address l)
           (log-tail l (format " topics [~a] data ~a byte~a"
                               (string-join (map topic-abbreviation (log-topics l)) " ")
                               n
                               (if (= n 1) "" "s")))))

;; raw is the node's object, so fields the struct does not name stay
;; reachable through log->jsexpr.
(struct log (address topics data block-number block-hash block-timestamp
             transaction-hash transaction-index index removed? raw)
  #:transparent
  #:property prop:custom-write write-log
  #:property prop:custom-print-quotable 'never)

(define (log->jsexpr l)
  (log-raw l))

(define (read-topics who v)
  (cond
    [(and (list? v) (<= (length v) 4))
     (map (lambda (topic) (node-hash->string who "log topic" topic)) v)]
    [else (raise-node-malformed who "log topics" v)]))

(define (read-removed who v)
  (cond
    [(boolean? v) v]
    [else (raise-node-malformed who "log removed flag" v)]))

(define/who (jsexpr->log who v)
  (cond
    [(hash? v) (read-log who v)]
    [else (raise-node-malformed who "log" v)]))

(define (read-log who v)
  (define (optional reader what key)
    (node-optional reader who what (hash-ref v key 'null)))
  (log (node-address->string who "log address" (hash-ref v 'address #f))
       (read-topics who (hash-ref v 'topics #f))
       (node-data->bytes who "log data" (hash-ref v 'data #f))
       (optional node-quantity->number "log block number" 'blockNumber)
       (optional node-hash->string "log block hash" 'blockHash)
       (optional node-quantity->number "log block timestamp" 'blockTimestamp)
       (optional node-hash->string "log transaction hash" 'transactionHash)
       (optional node-quantity->number "log transaction index" 'transactionIndex)
       (optional node-quantity->number "log index" 'logIndex)
       (read-removed who (hash-ref v 'removed #f))
       v))

(module* internal #f
  (provide jsexpr->log/who
           log
           log-tail))
