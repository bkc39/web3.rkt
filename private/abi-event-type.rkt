#lang racket/base

(require racket/contract/base
         (only-in racket/list count)
         (only-in racket/match match match-define struct*)
         (only-in racket/string string-join)
         "abi-type.rkt"
         (submod "abi-type.rkt" private))

(provide
 abi-event-spec/c
 (contract-out
  (struct abi-event-parameter ([name (or/c #f abi-parameter-name/c)]
                               [type abi-type-spec/c]
                               [indexed? boolean?]))
  (struct abi-event ([name abi-identifier/c]
                     [parameters (listof abi-event-parameter?)]
                     [anonymous? boolean?]))
  [string->abi-event (-> string? abi-event?)]
  [sexp->abi-event (-> pair? abi-event?)]
  [abi-event->string (-> abi-event-spec/c string?)]
  [abi-event->sexp (-> abi-event-spec/c pair?)]
  [abi-event->signature (-> abi-event-spec/c abi-signature?)]))

(define reserved-words
  '("indexed" "anonymous" "event" "tuple" "payable" "memory" "storage" "calldata" "function"
    "returns"))

;; Solidity cannot name a parameter after a keyword or a type, and
;; refusing them keeps every event printable as a string that reads back.
(define (parameter-name? v)
  (and (string? v)
       (eqv? (identifier-end v 0) (string-length v))
       (not (member v reserved-words))
       (not (elementary v))))

(define abi-parameter-name/c (flat-named-contract 'abi-parameter-name/c parameter-name?))

(define abi-event-spec/c
  (flat-named-contract 'abi-event-spec/c
                       (lambda (v) (or (abi-event? v) (string? v) (pair? v)))))

(struct abi-event-parameter (name type indexed?) #:transparent
  #:guard (lambda (name type indexed? who) (values name (abi-type/who who type) indexed?)))

;; #f, or (cons reason i) for the first parameter i that breaks a rule.
(define (parameters-failure parameters anonymous?)
  (define limit (if anonymous? 4 3))
  (define total (count abi-event-parameter-indexed? parameters))
  (let loop ([ps parameters] [i 0] [names '()] [indexed 0])
    (match ps
      ['() #f]
      [(cons (struct* abi-event-parameter ([name name] [indexed? indexed?])) rest)
       (define seen (if indexed? (add1 indexed) indexed))
       (cond
         [(> seen limit) (cons (format "too many indexed parameters: ~a (at most ~a)" total limit) i)]
         [(and name (member name names)) (cons (format "duplicate parameter name: ~a" name) i)]
         [else (loop rest (add1 i) (if name (cons name names) names) seen)])])))

(struct abi-event (name parameters anonymous?) #:transparent
  #:guard (lambda (name parameters anonymous? who)
            (match (parameters-failure parameters anonymous?)
              [#f (values name parameters anonymous?)]
              [(cons reason _) (raise-abi-type-error who reason name #f)])))

;;;; reading
(define (string->abi-event/who who s)
  (error 'unimplemented))

(define (sexp->abi-event/who who input)
  (error 'unimplemented))

(define (spec->abi-event/who who spec)
  (error 'unimplemented))

;;;; the public conversions
(define (string->abi-event s)
  (error 'unimplemented))

(define (sexp->abi-event v)
  (error 'unimplemented))

(define (abi-event->string spec)
  (error 'unimplemented))

(define (abi-event->sexp spec)
  (error 'unimplemented))

(define (abi-event->signature spec)
  (error 'unimplemented))

(module* private #f
  (provide abi-parameter-name/c
           sexp->abi-event/who
           spec->abi-event/who
           string->abi-event/who))
