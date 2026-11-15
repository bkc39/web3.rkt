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

(define (log-place->string l)
  (error 'unimplemented))

(define (log-removed-suffix l)
  (error 'unimplemented))

(define (write-log l port _mode)
  (error 'unimplemented))

;; raw is the node's object, so fields the struct does not name stay
;; reachable through log->jsexpr.
(struct log (address topics data block-number block-hash block-timestamp
             transaction-hash transaction-index index removed? raw)
  #:transparent
  #:property prop:custom-write write-log
  #:property prop:custom-print-quotable 'never)

(define (log->jsexpr l)
  (error 'unimplemented))

(define/who (jsexpr->log who v)
  (error 'unimplemented))

(module* internal #f
  (provide jsexpr->log/who
           log-place->string
           log-removed-suffix
           log))
