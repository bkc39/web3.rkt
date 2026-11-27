#lang racket/base

(require (only-in json jsexpr?)
         racket/contract/base
         "eth/call-trace.rkt"
         (only-in (submod "eth/call-trace.rkt" internal) jsexpr->call-trace/who)
         (only-in "private/argument.rkt" hash-argument/c)
         (only-in "private/contracts.rkt" from/c provider/c)
         (only-in "private/eth-params.rkt" block-param->string block-param/c transaction-object/c)
         (only-in "private/opaque.rkt" opaque->)
         (only-in "private/send.rkt" call-order define/filling filling->)
         (only-in "rpc.rkt" current-provider define-jsonrpc-endpoint rpc-call))

(provide (all-from-out "eth/call-trace.rkt")
         debug-trace-call
         debug-trace-transaction
         trace-call
         trace-transaction
         (contract-out
          [transaction-call-trace (opaque-> hash-argument/c [#:provider provider/c] call-trace?)]
          [call-transaction/trace (filling-> from/c [#:block block-param/c] call-trace?)]))

(define-jsonrpc-endpoint debug_traceCall
  (-> transaction-object/c [block-param/c block-param->string] (and/c hash? jsexpr?) hash?))
(define-jsonrpc-endpoint debug_traceTransaction
  (-> hash-argument/c (and/c hash? jsexpr?) hash?))
(define-jsonrpc-endpoint trace_transaction (-> hash-argument/c (listof hash?)))
(define trace-types/c (non-empty-listof (or/c "trace" "vmTrace" "stateDiff")))

(define-jsonrpc-endpoint trace_call
  (-> transaction-object/c trace-types/c [block-param/c block-param->string] hash?))

(define call-tracer (hasheq 'tracer "callTracer"))

(define (transaction-call-trace tx-hash #:provider [provider (current-provider)])
  (error 'unimplemented))

(define/filling (call-transaction/trace who from to order #:block [block "latest"])
  (error 'unimplemented))
