#lang racket/base

(require (only-in json jsexpr?)
         racket/contract/base
         (only-in "argument.rkt" hash-argument/c)
         (only-in "eth-params.rkt" block-param->string block-param/c transaction-object/c)
         (only-in "../rpc.rkt" define-jsonrpc-endpoint))

(provide debug-trace-call
         debug-trace-transaction
         trace-call
         trace-transaction)

(define trace-types/c (non-empty-listof (or/c "trace" "vmTrace" "stateDiff")))

(define-jsonrpc-endpoint debug_traceCall
  (-> transaction-object/c [block-param/c block-param->string] (and/c hash? jsexpr?) hash?))
(define-jsonrpc-endpoint debug_traceTransaction
  (-> hash-argument/c (and/c hash? jsexpr?) hash?))
(define-jsonrpc-endpoint trace_transaction (-> hash-argument/c (listof hash?)))
(define-jsonrpc-endpoint trace_call
  (-> transaction-object/c trace-types/c [block-param/c block-param->string] hash?))
