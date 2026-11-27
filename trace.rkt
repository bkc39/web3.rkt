#lang racket/base

(require racket/contract/base
         "eth/call-trace.rkt"
         (only-in (submod "eth/call-trace.rkt" internal) jsexpr->call-trace/who)
         (only-in "private/argument.rkt" hash-argument/c)
         (only-in "private/contracts.rkt" from/c provider/c)
         (only-in "private/eth-params.rkt" block-param/c)
         (only-in "private/opaque.rkt" opaque->)
         (only-in "private/send.rkt" call-order define/filling filling->)
         "private/trace-endpoints.rkt"
         (only-in "rpc.rkt" current-provider))

(provide (all-from-out "eth/call-trace.rkt")
         (all-from-out "private/trace-endpoints.rkt")
         (contract-out
          [transaction-call-trace (opaque-> hash-argument/c [#:provider provider/c] call-trace?)]
          [call-transaction/trace (filling-> from/c [#:block block-param/c] call-trace?)]))

(define call-tracer-options (hasheq 'tracer "callTracer"))

(define (transaction-call-trace tx-hash #:provider [provider (current-provider)])
  (jsexpr->call-trace/who 'transaction-call-trace
                          (debug-trace-transaction tx-hash call-tracer-options
                                                   #:provider provider)))

(define/filling (call-transaction/trace who from to order #:block [block "latest"])
  (define (request tx provider)
    (debug-trace-call tx block call-tracer-options #:provider provider))
  (jsexpr->call-trace/who who (call-order who from to order request)))

(module* internal #f
  (provide call-tracer-options))
