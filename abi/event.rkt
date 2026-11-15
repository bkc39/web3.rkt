#lang racket/base

(require racket/contract/base
         (only-in keccak keccak256)
         (only-in racket/list check-duplicates count dropf-right filter-not remove-duplicates)
         (only-in racket/match == match match* match-define struct*)
         (only-in racket/string string-join)
         "../abi.rkt"
         (submod "../abi.rkt" internal)
         "type.rkt"
         (submod "type.rkt" private)
         "../eth/log.rkt"
         (submod "../eth/log.rkt" internal)
         (only-in "../private/abi-decode.rkt" raise-abi-decode-error)
         "../private/exn.rkt"
         (only-in "../private/hex.rkt" bytes->hex hex->bytes)
         "../private/who.rkt")

(provide
 (contract-out
  [event-topic (-> abi-event-spec/c (or/c #f topic/c))]
  [abi-value->topic (-> abi-type-spec/c any/c topic/c)]
  [event-topics (->i ([event abi-event-spec/c])
                     ([where (event) (event-where/c 'event-topics event)])
                     [_ topic-positions/c])]
  [any-of (->* () #:rest (and/c pair? list?) any-of?)]
  [any-of? (-> any/c boolean?)]
  [any-of-values (-> any-of? (non-empty-listof any/c))]
  (struct indexed-hash ([topic topic/c]))
  [decode-log (-> abi-event-spec/c log? event-log?)]
  [decode-logs (-> (listof abi-event-spec/c) (listof log?) (listof log?))]
  [event-log? (-> any/c boolean?)]
  [event-log-event (-> event-log? abi-event?)]
  [event-log-name (-> event-log? string?)]
  [event-log-arguments (-> event-log? list?)]
  [event-log-ref (->i ([el event-log?] [key (el) (event-log-key/c el)]) [_ any/c])]))

(define (topic? v)
  (and (string? v) (regexp-match? #px"^0x[0-9a-fA-F]{64}$" v)))

(define topic/c (flat-named-contract '(and/c string? #px"^0x[0-9a-fA-F]{64}$") topic?))

(define topic-positions/c
  (and/c (listof (or/c #f topic/c (non-empty-listof topic/c)))
         (property/c length (<=/c 4))))

;; The index of the parameter key names, by name or position, or #f.
(define (parameter-index event key)
  (define ps (abi-event-parameters event))
  (cond
    [(exact-nonnegative-integer? key) (and (< key (length ps)) key)]
    [(or (symbol? key) (string? key))
     (define name (if (symbol? key) (symbol->string key) key))
     (for/first ([p (in-list ps)]
                 [i (in-naturals)]
                 #:when (equal? (abi-event-parameter-name p) name))
       i)]
    [else #f]))

(define (where-indices event where)
  (for/list ([key (in-hash-keys where)])
    (parameter-index event key)))

(define (indexed-indices? event indices)
  (define ps (abi-event-parameters event))
  (and (andmap (lambda (i) (and i (abi-event-parameter-indexed? (list-ref ps i)))) indices)
       (not (check-duplicates indices))))

(define (event-where/c who spec)
  (define event (spec->abi-event/who who spec))
  (flat-named-contract
   `(event-where/c ,(string->symbol (abi-event-name event)))
   (lambda (where)
     (and (hash? where) (indexed-indices? event (where-indices event where))))))

(define (event-log-key/c el)
  (define event (event-log-event el))
  (flat-named-contract `(event-log-key/c ,(string->symbol (abi-event-name event)))
                       (lambda (key) (and (parameter-index event key) #t))))

(define/who (event-topic who spec)
  (error 'unimplemented))

(define/who (abi-value->topic who type v)
  (error 'unimplemented))

(struct any-of (values)
  #:constructor-name new-any-of
  #:omit-define-syntaxes)

(define (any-of . vs)
  (error 'unimplemented))

(struct indexed-hash (topic) #:transparent
  #:guard (lambda (topic _who) (string-downcase topic)))

(define/who (event-topics who spec [where (hasheq)])
  (error 'unimplemented))

;;;; decoding
(define (write-event-log el port _mode)
  (error 'unimplemented))

(struct event-log log (event arguments)
  #:transparent
  #:property prop:custom-write write-event-log)

(define (event-log-name el)
  (error 'unimplemented))

(define (event-log-ref el key)
  (error 'unimplemented))

(define/who (decode-log who spec l)
  (error 'unimplemented))

(define (logs-decoder who specs)
  (error 'unimplemented))

(define/who (decode-logs who specs logs)
  (error 'unimplemented))

(module* internal #f
  (provide event-topics/who
           event-where/c
           logs-decoder
           topic-positions/c
           topic/c))
