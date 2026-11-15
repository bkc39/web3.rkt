#lang racket/base

(require racket/contract/base
         (only-in json jsexpr?)
         (only-in racket/list append*)
         (only-in racket/match match match-define struct*)
         "abi/event.rkt"
         (submod "abi/event.rkt" internal)
         "abi/type.rkt"
         (only-in (submod "abi/type.rkt" private) spec->abi-event/who)
         "eth.rkt"
         "eth/log.rkt"
         (only-in (submod "eth/log.rkt" internal) jsexpr->log/who)
         (only-in "private/node.rkt" node-quantity->number raise-node-malformed)
         "rpc.rkt")

(provide
 (all-from-out "abi/event.rkt" "eth/log.rkt")
 (contract-out
  [log-filter
   (->i ()
        (#:address [address (or/c #f (abi-value/c 'address) (non-empty-listof (abi-value/c 'address)))]
         #:from-block [from block-param/c]
         #:to-block [to block-param/c]
         #:block-hash [block-hash (or/c #f topic/c)]
         #:topics [topics topic-positions/c]
         #:event [event (or/c #f abi-event-spec/c)]
         #:where [where (event) (if (and (not (unsupplied-arg? event)) event)
                                    (event-where/c 'log-filter event)
                                    no-where/c)])
        #:pre/name (block-hash from to) "a block hash excludes #:from-block and #:to-block"
        (not (and (supplied? block-hash) block-hash (or (supplied? from) (supplied? to))))
        #:pre/name (topics event) "#:topics excludes #:event"
        (not (and (supplied? topics) (pair? topics) (supplied? event) event))
        #:pre/name (from to) "#:from-block is after #:to-block"
        (not (and (exact-integer? from) (exact-integer? to) (> from to)))
        [_ log-filter?])]
  [log-filter? (-> any/c boolean?)]
  [log-filter->jsexpr (-> log-filter? (and/c hash? jsexpr?))]
  [get-logs (->* (log-filter?)
                 (#:events (listof abi-event-spec/c)
                  #:chunk (or/c #f exact-positive-integer?)
                  #:provider (or/c #f provider?))
                 (listof log?))]
  [receipt-logs (->* ((and/c hash? jsexpr?)) (#:events (listof abi-event-spec/c)) (listof log?))]))

(define no-where/c (flat-named-contract '(and/c hash? hash-empty?) (lambda (v) (and (hash? v) (hash-empty? v)))))

(define (supplied? v)
  (not (unsupplied-arg? v)))

(define (write-log-filter f port _mode)
  (error 'unimplemented))

;; topics are the positions as sent, computed from event and #:where when
;; there is an event.
(struct log-filter (address from to block-hash topics event)
  #:name filter-info
  #:constructor-name new-log-filter
  #:property prop:custom-write write-log-filter
  #:property prop:custom-print-quotable 'never)

(define (log-filter #:address [address #f]
                    #:from-block [from "earliest"]
                    #:to-block [to "latest"]
                    #:block-hash [block-hash #f]
                    #:topics [topics '()]
                    #:event [event-spec #f]
                    #:where [where (hasheq)])
  (error 'unimplemented))

(define (log-filter->jsexpr f)
  (error 'unimplemented))

(define (log-limit-error? e)
  (error 'unimplemented))

;; Logs from blocks from to to, requested span blocks at a time.  A
;; request the node refuses as too large is retried at half its size, and
;; the span is not grown back.
(define (fetch-logs who f from to #:chunk span #:provider provider)
  (error 'unimplemented))

(define (filter-events f)
  (match (log-filter-event f)
    [#f '()]
    [event (list event)]))

(define (get-logs f
                  #:events [events (filter-events f)]
                  #:chunk [chunk #f]
                  #:provider [provider (current-provider)])
  (error 'unimplemented))

(define (receipt-logs receipt #:events [events '()])
  (error 'unimplemented))

(module* internal #f
  (provide event-where/c
           fetch-logs
           log-limit-error?))
