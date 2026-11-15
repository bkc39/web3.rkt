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
  (write-string (format "#<log-filter ~s>" (log-filter->jsexpr f)) port))

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
  (define event (and event-spec (spec->abi-event/who 'log-filter event-spec)))
  (new-log-filter address from to block-hash
                  (if event (event-topics/who 'log-filter event where) topics)
                  event))

(define (log-filter->jsexpr f)
  (match-define (struct* filter-info ([address address] [from from] [to to]
                                      [block-hash block-hash] [topics topics]))
    f)
  (define range
    (if block-hash
        (hasheq 'blockHash block-hash)
        (hasheq 'fromBlock (block-param->string from) 'toBlock (block-param->string to))))
  (define addressed (if address (hash-set range 'address address) range))
  (if (null? topics)
      addressed
      (hash-set addressed 'topics (map (lambda (p) (or p 'null)) topics))))

(define never-retried #px"(?i:rate|invalid|out ?of ?range|unknown block)")

(define limit-exceeded
  (pregexp (string-append "(?i:block range|range (?:is )?too|ranges? over|too many"
                          "|more than [0-9,]+ (?:results|logs|blocks)|response size"
                          "|limited to|exceeds? (?:the )?max)")))

;; The errors nodes give for a range or result too large, recognised
;; conservatively: anything else is raised as it came.
(define (log-limit-error? e)
  (match e
    [(struct* exn:fail:web3:rpc ([method "eth_getLogs"] [code code] [rpc-message (? string? message)]))
     (and (not (regexp-match? never-retried message))
          (or (eqv? code -32005) (regexp-match? limit-exceeded message)))]
    [_ #f]))

(define (read-logs who jsexprs)
  (map (lambda (v) (jsexpr->log/who who v)) jsexprs))

;; Logs from blocks from to to, requested span blocks at a time.  A
;; request the node refuses as too large is retried at half its size, and
;; the span is not grown back.
(define (fetch-logs who f from to #:chunk span #:provider provider)
  (define base (log-filter->jsexpr f))
  (let loop ([lo from] [span span] [chunks '()])
    (define hi (min to (+ lo span -1)))
    (define (request)
      (eth-get-logs (hash-set* base 'fromBlock (number->hex-string lo) 'toBlock (number->hex-string hi))
                    #:provider provider))
    (define result (and (<= lo to) (with-handlers ([log-limit-error? values]) (request))))
    (cond
      [(> lo to) (append* (reverse chunks))]
      [(list? result) (loop (add1 hi) span (cons (read-logs who result) chunks))]
      [(> hi lo) (loop lo (max 1 (quotient (+ (- hi lo) 1) 2)) chunks)]
      [else (raise result)])))

(define (block-number who tag provider)
  (match tag
    [(? exact-nonnegative-integer?) tag]
    ["earliest" 0]
    ["latest" (eth-block-number #:provider provider)]
    [_
     (match (eth-get-block-by-number tag #f #:provider provider)
       [(? hash? block) (node-quantity->number who "block number" (hash-ref block 'number #f))]
       [block (raise-node-malformed who (format "~a block" tag) block)])]))

(define (fetch-resolved who f span provider)
  (define from (block-number who (log-filter-from f) provider))
  (define to
    (if (equal? (log-filter-to f) (log-filter-from f))
        from
        (block-number who (log-filter-to f) provider)))
  (cond
    [(> from to) '()]
    [else (fetch-logs who f from to #:chunk (span (+ (- to from) 1)) #:provider provider)]))

(define (fetch-filter who f chunk provider)
  (define (whole)
    (read-logs who (eth-get-logs (log-filter->jsexpr f) #:provider provider)))
  (cond
    [(log-filter-block-hash f) (whole)]
    [chunk (fetch-resolved who f (lambda (_) chunk) provider)]
    [else
     (with-handlers ([log-limit-error?
                      (lambda (_) (fetch-resolved who f (lambda (n) (max 1 (quotient n 2))) provider))])
       (whole))]))

(define (filter-events f)
  (match (log-filter-event f)
    [#f '()]
    [event (list event)]))

(define (get-logs f
                  #:events [events (filter-events f)]
                  #:chunk [chunk #f]
                  #:provider [provider (current-provider)])
  (define decode (logs-decoder 'get-logs events))
  (decode (fetch-filter 'get-logs f chunk provider)))

(define (receipt-logs receipt #:events [events '()])
  (define decode (logs-decoder 'receipt-logs events))
  (match (hash-ref receipt 'logs #f)
    [(? list? logs) (decode (read-logs 'receipt-logs logs))]
    [logs (raise-node-malformed 'receipt-logs "logs" logs)]))

(module* internal #f
  (provide event-where/c
           fetch-logs
           log-limit-error?))
