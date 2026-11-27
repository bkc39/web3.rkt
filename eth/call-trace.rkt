#lang racket/base

(require racket/contract/base
         (only-in racket/match match)
         (only-in "../halt.rkt" halt/c message->halt)
         "../private/record.rkt")

(provide (contract-out [call-trace-halt (-> call-trace? (or/c #f halt/c))]))

;; The EVM runs calls 1024 deep, and a tracer records the call refused beyond that as a frame.
(define deepest-frame-depth 1025)

(define (calls-tail count)
  (match count
    [0 ""]
    [1 " 1 call"]
    [_ (format " ~a calls" count)]))

(define (write-call-trace trace port _mode)
  (define words (call-trace-error trace))
  (fprintf port "#<call-trace ~a ~a to ~a gas used ~a~a~a>"
           (call-trace-type trace)
           (call-trace-from trace)
           (or (call-trace-to trace) "nothing")
           (call-trace-gas-used trace)
           (calls-tail (length (call-trace-calls trace)))
           (if words (format " error ~s" words) "")))

(define-node-record call-trace #:noun "call trace" #:printer write-call-trace
  [type string-field]
  [from address-field]
  [to address-field #:optional]
  [value quantity-field #:optional]
  [gas uint64-field]
  [gas-used uint64-field #:key gasUsed]
  [input data-field]
  [output data-field #:default #""]
  [error string-field #:optional]
  [revert-reason string-field #:key revertReason #:optional]
  [calls (list-field (record-field jsexpr->call-trace/who call-trace?
                                   #:max-depth deepest-frame-depth))
         #:default '()])

(define (call-trace-halt trace)
  (define words (call-trace-error trace))
  (and words (message->halt words)))

(module* internal #f
  (provide call-trace-fields
           call-trace-noun
           jsexpr->call-trace/who))
