#lang racket/base

(require racket/contract/base
         (only-in racket/list count)
         (only-in racket/match match match-define struct*)
         (only-in racket/string string-join)
         "type.rkt"
         (submod "type.rkt" private)
         "../private/who.rkt")

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

(define/who (string->abi-event who s)
  (define n (string-length s))
  (define (fail reason position)
    (raise-abi-type-error who reason s position))
  (define (char-at i)
    (and (< i n) (string-ref s i)))
  (define (skip i)
    (skip-spaces s i))
  (define (word-end i word)
    (define end (identifier-end s i))
    (and end (string=? (substring s i end) word) end))
  (define (read-parameter i)
    (define-values (type type-end) (read-abi-type s i #:component-names? #t #:who who))
    (define flag-start (skip type-end))
    (define flag-end (and (> flag-start type-end) (word-end flag-start "indexed")))
    (define after-flag (or flag-end type-end))
    (define name-start (skip after-flag))
    (define name-end (and (> name-start after-flag) (identifier-end s name-start)))
    (define name (and name-end (substring s name-start name-end)))
    (when (and name (not (parameter-name? name)))
      (fail "expected a parameter name" name-start))
    (values (abi-event-parameter name type (and flag-end #t)) (or name-end after-flag)))
  ;; The parameters from just after the "(" at i, their positions, and the
  ;; index after the ")".
  (define (read-parameters i)
    (define start (skip (add1 i)))
    (cond
      [(eqv? (char-at start) #\)) (values '() '() (add1 start))]
      [else
       (let loop ([at start] [ps '()] [positions '()])
         (define-values (p end) (read-parameter at))
         (define k (skip end))
         (match (char-at k)
           [#\, (loop (skip (add1 k)) (cons p ps) (cons at positions))]
           [#\) (values (reverse (cons p ps)) (reverse (cons at positions)) (add1 k))]
           [#f (fail "expected , or )" n)]
           [_ (fail "expected , or )" k)]))]))
  (define start (skip 0))
  (define keyword-end (word-end start "event"))
  (define name-start (if (and keyword-end (eqv? (char-at keyword-end) #\space)) (skip keyword-end) start))
  (define name-end (identifier-end s name-start))
  (define open (and name-end (skip name-end)))
  (cond
    [(not name-end) (fail "expected an event name" name-start)]
    [(not (eqv? (char-at open) #\()) (fail "expected (" open)]
    [else
     (define-values (parameters positions close) (read-parameters open))
     (define anonymous-start (skip close))
     (define anonymous-end (and (> anonymous-start close) (word-end anonymous-start "anonymous")))
     (define end (skip (or anonymous-end close)))
     (define anonymous? (and anonymous-end #t))
     (cond
       [(< end n) (fail "unexpected text" end)]
       [(parameters-failure parameters anonymous?)
        => (lambda (failure) (fail (car failure) (list-ref positions (cdr failure))))]
       [else (abi-event (substring s name-start name-end) parameters anonymous?)])]))

(define identifier? (flat-contract-predicate abi-identifier/c))

(define (identifier-symbol? v)
  (and (symbol? v) (identifier? (symbol->string v))))

(define (parameter-name-datum? v)
  (or (not v) (and (symbol? v) (parameter-name? (symbol->string v)))))

(define/who (sexp->abi-event who input)
  (define (fail reason form)
    (raise-abi-type-error who reason input #f form))
  (define (parameter name type indexed?)
    (abi-event-parameter (and name (symbol->string name)) (sexp->type who type input) indexed?))
  (define (read-parameter p)
    (match p
      [(list (? parameter-name-datum? name) type) (parameter name type #f)]
      [(list (? parameter-name-datum? name) type '#:indexed) (parameter name type #t)]
      [_ (fail "expected [name type] or [name type #:indexed]" p)]))
  (define (build name ps anonymous?)
    (define parameters (map read-parameter ps))
    (match (parameters-failure parameters anonymous?)
      [#f (abi-event (symbol->string name) parameters anonymous?)]
      [(cons reason i) (fail reason (list-ref ps i))]))
  (match input
    [(list 'event (? identifier-symbol? name) (? list? ps)) (build name ps #f)]
    [(list 'event (? identifier-symbol? name) (? list? ps) '#:anonymous) (build name ps #t)]
    [_ (fail "expected (event name (parameter ...)) with an optional #:anonymous" input)]))

(define (spec->abi-event/who who spec)
  (cond
    [(abi-event? spec) spec]
    [(string? spec) (string->abi-event/who who spec)]
    [else (sexp->abi-event/who who spec)]))

;;;; writing

(define (event->string e)
  (match-define (struct* abi-event ([name name] [parameters ps] [anonymous? anonymous?])) e)
  (define (parameter->string p)
    (match-define (struct* abi-event-parameter ([name pname] [type type] [indexed? indexed?])) p)
    (string-append (type->string type)
                   (if indexed? " indexed" "")
                   (if pname (string-append " " pname) "")))
  (format "event ~a(~a)~a"
          name
          (string-join (map parameter->string ps) ", ")
          (if anonymous? " anonymous" "")))

(define (event->sexp e)
  (match-define (struct* abi-event ([name name] [parameters ps] [anonymous? anonymous?])) e)
  (define (parameter->sexp p)
    (match-define (struct* abi-event-parameter ([name pname] [type type] [indexed? indexed?])) p)
    `(,(and pname (string->symbol pname)) ,(type->sexp type) ,@(if indexed? '(#:indexed) '())))
  `(event ,(string->symbol name) ,(map parameter->sexp ps) ,@(if anonymous? '(#:anonymous) '())))

(define (event->signature e)
  (abi-signature (abi-event-name e) (map abi-event-parameter-type (abi-event-parameters e)) #f))

;;;; the conversions of a spec

(define (abi-event->string spec)
  (event->string (spec->abi-event/who 'abi-event->string spec)))

(define (abi-event->sexp spec)
  (event->sexp (spec->abi-event/who 'abi-event->sexp spec)))

(define (abi-event->signature spec)
  (event->signature (spec->abi-event/who 'abi-event->signature spec)))

(module* private #f
  (provide abi-parameter-name/c
           parameters-failure
           sexp->abi-event/who
           spec->abi-event/who
           string->abi-event/who))
