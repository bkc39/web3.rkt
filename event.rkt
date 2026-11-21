#lang racket/base

(require racket/contract/base
         (only-in json jsexpr?)
         (only-in racket/list append*)
         (only-in racket/match match struct*)
         "abi/event.rkt"
         (submod "abi/event.rkt" internal)
         "abi/event-type.rkt"
         (only-in (submod "abi/event-type.rkt" private) spec->abi-event/who)
         "abi/type.rkt"
         "eth.rkt"
         "eth/log.rkt"
         (only-in (submod "eth/log.rkt" internal) jsexpr->log/who)
         (only-in (submod "eth/receipt.rkt" internal) receipt-log-list)
         (only-in "private/node.rkt" node-quantity->number raise-node-malformed)
         "rpc.rkt")

(provide
 (all-from-out "abi/event.rkt" "eth/log.rkt")
 (contract-out
  [log-filter
   (->i ()
        (#:address [address address-filter/c]
         #:from-block [from block-param/c]
         #:to-block [to block-param/c]
         #:block-hash [block-hash (or/c #f block-hash/c)]
         #:topics [topics topic-positions/c]
         #:event [event (or/c #f abi-event-spec/c)]
         #:where [where (event) (where/c event)])
        #:pre/name (block-hash from to) "a block hash excludes #:from-block and #:to-block"
        (hash-or-range? block-hash from to)
        #:pre/name (topics event) "#:topics excludes #:event"
        (topics-or-event? topics event)
        #:pre/name (from to) "#:from-block is after #:to-block"
        (ordered-range? from to)
        [_ log-filter?])]
  [log-filter? (-> any/c boolean?)]
  [log-filter->jsexpr (-> log-filter? (and/c hash? jsexpr?))]
  [get-logs (->* (log-filter?)
                 (#:events (listof abi-event-spec/c)
                  #:chunk (or/c #f exact-positive-integer?)
                  #:provider (or/c #f provider?))
                 (listof log?))]
  [receipt-logs (->* (receipt?) (#:events (listof abi-event-spec/c)) (listof log?))]))

(define address-filter/c
  (flat-named-contract 'address-filter/c
                       (or/c #f (abi-value/c 'address) (non-empty-listof (abi-value/c 'address)))))

(define block-hash/c (flat-named-contract 'block-hash/c topic/c))

(define no-where/c (flat-named-contract 'no-where/c (lambda (v) (and (hash? v) (hash-empty? v)))))

(define (supplied? v)
  (not (unsupplied-arg? v)))

(define (where/c event)
  (if (and (supplied? event) event)
      (event-where/c 'log-filter event)
      no-where/c))

(define (hash-or-range? block-hash from to)
  (not (and (supplied? block-hash) block-hash (or (supplied? from) (supplied? to)))))

(define (topics-or-event? topics event)
  (not (and (supplied? topics) (pair? topics) (supplied? event) event)))

(define (ordered-range? from to)
  (not (and (exact-integer? from) (exact-integer? to) (> from to))))

(define (write-log-filter f port _mode)
  (fprintf port "#<log-filter ~s>" (log-filter->jsexpr f)))

;; topics are the positions as sent, computed from event and #:where when
;; there is an event.
(struct log-filter (address from to block-hash topics event)
  #:omit-define-syntaxes
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
  (define address (log-filter-address f))
  (define block-hash (log-filter-block-hash f))
  (define topics (log-filter-topics f))
  (define range
    (if block-hash
        (hasheq 'blockHash block-hash)
        (hasheq 'fromBlock (block-param->string (log-filter-from f))
                'toBlock (block-param->string (log-filter-to f)))))
  (define addressed (if address (hash-set range 'address address) range))
  (if (null? topics)
      addressed
      (hash-set addressed 'topics (map (lambda (p) (or p 'null)) topics))))

(define never-retried
  (pregexp (string-append "(?i:rate|invalid|out ?of ?range|unknown block|too many requests"
                          "|concurrent|exceed max (?:topics|addresses)|beyond)")))

(define limit-exceeded
  (pregexp (string-append "(?i:block range|range (?:is )?too|ranges? over"
                          "|too many (?:results|logs|blocks)"
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
  (define (request lo hi)
    (with-handlers ([log-limit-error? values])
      (eth-get-logs (hash-set* base 'fromBlock (number->hex-string lo) 'toBlock (number->hex-string hi))
                    #:provider provider)))
  (let loop ([lo from] [span span] [chunks '()])
    (define hi (min to (+ lo span -1)))
    (cond
      [(> lo to) (append* (reverse chunks))]
      [else
       (match (request lo hi)
         [(? list? logs) (loop (add1 hi) span (cons (read-logs who logs) chunks))]
         [_ #:when (> hi lo) (loop lo (max 1 (quotient (+ (- hi lo) 1) 2)) chunks)]
         [e (raise e)])])))

;; "pending" is read as the latest block: nodes refuse a range past it.
(define (tag->block-number who tag provider)
  (match tag
    [(? exact-nonnegative-integer?) tag]
    ["earliest" 0]
    [(or "latest" "pending") (eth-block-number #:provider provider)]
    [_
     (match (rpc-call "eth_getBlockByNumber" (list tag #f) #:provider provider)
       [(? hash? block) (node-quantity->number who "block number" (hash-ref block 'number #f))]
       [block (raise-node-malformed who (format "~a block" tag) block)])]))

(define (resolved-range who f provider)
  (define from (tag->block-number who (log-filter-from f) provider))
  (define to
    (if (equal? (log-filter-to f) (log-filter-from f))
        from
        (tag->block-number who (log-filter-to f) provider)))
  (values from to))

(define (fetch-range who from to span provider f)
  (if (> from to)
      '()
      (fetch-logs who f from to #:chunk span #:provider provider)))

(define (fetch-filter who f chunk provider)
  (define (whole)
    (read-logs who (eth-get-logs (log-filter->jsexpr f) #:provider provider)))
  (define (halved e)
    (define-values (from to) (resolved-range who f provider))
    (if (>= from to)
        (raise e)
        (fetch-range who from to (max 1 (quotient (+ (- to from) 1) 2)) provider f)))
  (cond
    [(log-filter-block-hash f) (whole)]
    [chunk
     (define-values (from to) (resolved-range who f provider))
     (fetch-range who from to chunk provider f)]
    [else (with-handlers ([log-limit-error? halved]) (whole))]))

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
  ((logs-decoder 'receipt-logs events) (receipt-log-list receipt)))

(module* internal #f
  (provide event-where/c
           fetch-logs
           log-limit-error?))
