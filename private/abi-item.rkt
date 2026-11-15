#lang racket/base

;; ABI items: the structs and their string and s-expression forms, under
;; the caller's who.  web3/abi/description puts contracts on them.

(require racket/contract/base
         (only-in racket/list check-duplicates)
         (only-in racket/match match match* struct*)
         (only-in racket/string string-join)
         "../abi/event-type.rkt"
         (submod "../abi/event-type.rkt" private)
         "../abi/type.rkt"
         (submod "../abi/type.rkt" private)
         "exn.rkt")

(provide (struct-out abi-parameter-struct)
         (struct-out abi-function-struct)
         (struct-out abi-error-struct)
         (struct-out abi-constructor-struct)
         (struct-out abi-fallback-struct)
         (struct-out abi-receive-struct)
         abi-constructor/who
         abi-error/who
         abi-function/who
         abi-item?
         abi-parameter/who
         identifier?
         item->sexp
         item->string
         item/who
         parameter-name?
         sexp->abi-item/who
         string->abi-item/who)

(define identifier? (flat-contract-predicate abi-identifier/c))
(define parameter-name? (flat-contract-predicate abi-parameter-name/c))

;;;; printing

(define ((constructor-writer name fields) v port _mode)
  (fprintf port "(~a" name)
  (for ([field (in-list (fields v))])
    (fprintf port " ~v" field))
  (fprintf port ")"))

;;;; structs

(struct abi-parameter (name type) #:transparent
  #:name abi-parameter-struct
  #:constructor-name raw-abi-parameter
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (constructor-writer 'abi-parameter (lambda (p) (list (abi-parameter-name p) (abi-parameter-type p)))))

(struct abi-function abi-signature (parameters returns state-mutability) #:transparent
  #:name abi-function-struct
  #:constructor-name raw-abi-function
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (constructor-writer 'abi-function
                      (lambda (f) (list (abi-signature-name f) (abi-function-parameters f)
                                        (abi-function-returns f) (abi-function-state-mutability f)))))

(struct abi-error abi-signature (parameters) #:transparent
  #:name abi-error-struct
  #:constructor-name raw-abi-error
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (constructor-writer 'abi-error (lambda (e) (list (abi-signature-name e) (abi-error-parameters e)))))

(struct abi-constructor abi-signature (parameters state-mutability) #:transparent
  #:name abi-constructor-struct
  #:constructor-name raw-abi-constructor
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (constructor-writer 'abi-constructor
                      (lambda (c) (list (abi-constructor-parameters c)
                                        (abi-constructor-state-mutability c)))))

(struct abi-fallback (state-mutability) #:transparent
  #:name abi-fallback-struct
  #:constructor-name raw-abi-fallback
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (constructor-writer 'abi-fallback (lambda (f) (list (abi-fallback-state-mutability f)))))

(struct abi-receive () #:transparent
  #:name abi-receive-struct
  #:constructor-name raw-abi-receive
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (constructor-writer 'abi-receive (lambda (_) '())))

(define (abi-item? v)
  (or (abi-function? v) (abi-error? v) (abi-constructor? v) (abi-fallback? v) (abi-receive? v)
      (abi-event? v)))

;;;; smart constructors

(define (abi-parameter/who who name type)
  (raw-abi-parameter name (abi-type/who who type)))

(define (parameter/who who spec)
  (if (abi-parameter? spec) spec (raw-abi-parameter #f (abi-type/who who spec))))

(define (check-parameter-names who input ps)
  (match (check-duplicates (filter values (map abi-parameter-name ps)))
    [#f (void)]
    [name (raise-abi-type-error who (format "duplicate parameter name: ~a" name) input #f)]))

(define (abi-function/who who name inputs outputs [mutability 'nonpayable])
  (define ps (map (lambda (spec) (parameter/who who spec)) inputs))
  (define rs (map (lambda (spec) (parameter/who who spec)) outputs))
  (check-parameter-names who name (append ps rs))
  (raw-abi-function name (map abi-parameter-type ps) (map abi-parameter-type rs) ps rs mutability))

(define (abi-error/who who name inputs)
  (define ps (map (lambda (spec) (parameter/who who spec)) inputs))
  (check-parameter-names who name ps)
  (raw-abi-error name (map abi-parameter-type ps) #f ps))

(define (abi-constructor/who who inputs [mutability 'nonpayable])
  (define ps (map (lambda (spec) (parameter/who who spec)) inputs))
  (check-parameter-names who "constructor" ps)
  (raw-abi-constructor "constructor" (map abi-parameter-type ps) #f ps mutability))

;;;; strings

(define (string->abi-item/who who s)
  (define n (string-length s))
  (define (fail reason position)
    (raise-abi-type-error who reason s position))
  (define (char-at i)
    (and (< i n) (string-ref s i)))
  (define (skip i)
    (skip-spaces s i))
  (define (word-at i)
    (define end (identifier-end s i))
    (and end (substring s i end)))
  (define (word-end i)
    (or (identifier-end s i) i))
  (define (read-parameter i)
    (define-values (type type-end) (read-abi-type s i #:component-names? #t #:who who))
    (define j (skip type-end))
    (define location (and (> j type-end) (member (word-at j) '("memory" "calldata" "storage")) j))
    (define after-location (if location (word-end location) type-end))
    (define k (skip after-location))
    (define name (and (> k after-location) (word-at k)))
    (cond
      [(not name) (values (raw-abi-parameter #f type) after-location)]
      [(parameter-name? name) (values (raw-abi-parameter name type) (word-end k))]
      [else (fail "expected a parameter name" k)]))
  ;; The parameters from the "(" at open, their positions, and the index
  ;; after the ")".
  (define (read-parameters open)
    (define start (skip (add1 open)))
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
  (define (check-names ps positions)
    (let loop ([ps ps] [positions positions] [seen '()])
      (match* (ps positions)
        [('() '()) (void)]
        [((cons (struct* abi-parameter-struct ([name name])) ps) (cons at positions))
         (cond
           [(and name (member name seen)) (fail (format "duplicate parameter name: ~a" name) at)]
           [else (loop ps positions (if name (cons name seen) seen))])])))
  (define (open-at i)
    (define open (skip i))
    (if (eqv? (char-at open) #\() open (fail "expected (" open)))
  (define (empty-parameters i)
    (define close (skip (add1 (open-at i))))
    (if (eqv? (char-at close) #\)) (add1 close) (fail "expected )" close)))
  ;; The modifiers from i to the end or to the word stop, at most one of
  ;; each group, and the index where they end.
  (define (read-modifiers i groups #:stop [stop #f])
    (define allowed (apply append groups))
    (let loop ([i i] [words '()])
      (define j (skip i))
      (define word (and (> j i) (word-at j)))
      (cond
        [(= j n) (values (reverse words) j)]
        [(and stop (equal? word stop)) (values (reverse words) j)]
        [(member word '("internal" "private")) (fail (format "~a is not part of an ABI" word) j)]
        [(and word (member word allowed))
         (define group (for/first ([g (in-list groups)] #:when (member word g)) g))
         (if (for/or ([w (in-list words)]) (member w group))
             (fail (format "at most one of ~a" (string-join group ", ")) j)
             (loop (word-end j) (cons word words)))]
        [word (fail (format "expected ~a" (string-join (if stop (append allowed (list stop)) allowed) ", ")) j)]
        [else (fail "unexpected text" j)])))
  (define (end-at i)
    (define j (skip i))
    (when (< j n)
      (fail "unexpected text" j)))
  (define (name-at i what)
    (define start (skip i))
    (define name (and (> start i) (word-at start)))
    (if (and name (identifier? name)) (values name (word-end start)) (fail (format "expected ~a" what) start)))
  (define (read-function i)
    (define-values (name name-end) (name-at i "a function name"))
    (define-values (inputs input-positions close) (read-parameters (open-at name-end)))
    (define-values (modifiers modifiers-end)
      (read-modifiers close '(("external" "public") ("view" "pure" "payable")) #:stop "returns"))
    (define mutability
      (for/fold ([m 'nonpayable]) ([w (in-list modifiers)])
        (if (member w '("view" "pure" "payable")) (string->symbol w) m)))
    (define-values (outputs output-positions)
      (cond
        [(= modifiers-end n) (values '() '())]
        [else
         (define open (open-at (word-end modifiers-end)))
         (define-values (ps positions end) (read-parameters open))
         (when (null? ps)
           (fail "returns needs at least one type" open))
         (define j (skip end))
         (when (and (< j n) (member (word-at j) '("external" "public" "view" "pure" "payable")))
           (fail "modifiers come before returns" j))
         (end-at end)
         (values ps positions)]))
    (check-names (append inputs outputs) (append input-positions output-positions))
    (raw-abi-function name (map abi-parameter-type inputs) (map abi-parameter-type outputs)
                      inputs outputs mutability))
  (define (read-error i)
    (define-values (name name-end) (name-at i "an error name"))
    (define-values (inputs positions close) (read-parameters (open-at name-end)))
    (end-at close)
    (check-names inputs positions)
    (raw-abi-error name (map abi-parameter-type inputs) #f inputs))
  (define (read-constructor i)
    (define-values (inputs positions close) (read-parameters (open-at i)))
    (define-values (modifiers _end) (read-modifiers close '(("public") ("payable"))))
    (check-names inputs positions)
    (raw-abi-constructor "constructor" (map abi-parameter-type inputs) #f inputs
                         (if (member "payable" modifiers) 'payable 'nonpayable)))
  (define (read-fallback i)
    (define-values (modifiers _end) (read-modifiers (empty-parameters i) '(("external") ("payable"))))
    (raw-abi-fallback (if (member "payable" modifiers) 'payable 'nonpayable)))
  (define (read-receive i)
    (define-values (modifiers end) (read-modifiers (empty-parameters i) '(("external") ("payable"))))
    (if (member "payable" modifiers) (raw-abi-receive) (fail "receive is payable" end)))
  (define start (skip 0))
  (define keyword (word-at start))
  (define after (word-end start))
  (match keyword
    ["function" (read-function after)]
    ["event" (string->abi-event/who who s)]
    ["error" (read-error after)]
    ["constructor" (read-constructor after)]
    ["fallback" (read-fallback after)]
    ["receive" (read-receive after)]
    [_ (fail "an item starts with function, event, error, constructor, fallback or receive" start)]))

(define (parameter->string p)
  (match p
    [(struct* abi-parameter-struct ([name #f] [type t])) (type->string t)]
    [(struct* abi-parameter-struct ([name name] [type t])) (format "~a ~a" (type->string t) name)]))

(define (parameters->string ps)
  (format "(~a)" (string-join (map parameter->string ps) ", ")))

(define (item->string i)
  (match i
    [(? abi-function?)
     (format "function ~a~a~a~a"
             (abi-signature-name i)
             (parameters->string (abi-function-parameters i))
             (match (abi-function-state-mutability i)
               ['nonpayable ""]
               [m (format " ~a" m)])
             (match (abi-function-returns i)
               ['() ""]
               [rs (format " returns ~a" (parameters->string rs))]))]
    [(? abi-error?) (format "error ~a~a" (abi-signature-name i) (parameters->string (abi-error-parameters i)))]
    [(? abi-constructor?)
     (format "constructor~a~a" (parameters->string (abi-constructor-parameters i))
             (if (eq? (abi-constructor-state-mutability i) 'payable) " payable" ""))]
    [(? abi-fallback?) (if (eq? (abi-fallback-state-mutability i) 'payable) "fallback() payable" "fallback()")]
    [(? abi-receive?) "receive() payable"]
    [(? abi-event?) (abi-event->string i)]))

;;;; s-expressions

(define (identifier-symbol? v)
  (and (symbol? v) (identifier? (symbol->string v))))

(define (parameter-name-datum? v)
  (or (not v) (and (symbol? v) (parameter-name? (symbol->string v)))))

(define (sexp->abi-item/who who input)
  (define (fail reason form)
    (raise-abi-type-error who reason input #f form))
  (define (name-of v)
    (if (identifier-symbol? v) (symbol->string v) (fail "expected a name" v)))
  (define (read-parameters ps)
    (define parameters
      (if (list? ps)
          (for/list ([p (in-list ps)])
            (match p
              [(list (? parameter-name-datum? name) type)
               (raw-abi-parameter (and name (symbol->string name)) (sexp->type who type input))]
              [_ (fail "expected [name type], with name a symbol or #f" p)]))
          (fail "expected a list of parameters" ps)))
    (values parameters ps))
  (define (check-names parameters forms)
    (for/fold ([seen '()] #:result (void))
              ([p (in-list parameters)]
               [form (in-list forms)])
      (define name (abi-parameter-name p))
      (cond
        [(and name (member name seen)) (fail (format "duplicate parameter name: ~a" name) form)]
        [name (cons name seen)]
        [else seen])))
  (define (read-function name ps clauses)
    (define-values (inputs input-forms) (read-parameters ps))
    (define-values (outputs output-forms mutability)
      (let loop ([cs clauses] [outputs #f] [output-forms '()] [mutability #f])
        (match cs
          ['() (values (or outputs '()) output-forms (or mutability 'nonpayable))]
          [(list* '#:returns rs rest)
           #:when (not outputs)
           (define-values (parameters forms) (read-parameters rs))
           (loop rest parameters forms mutability)]
          [(cons (and kw (or '#:view '#:pure '#:payable)) rest)
           #:when (not mutability)
           (loop rest outputs output-forms (string->symbol (keyword->string kw)))]
          [(cons c _) #:when (memq c '(#:returns #:view #:pure #:payable))
           (fail "at most one #:returns and one of #:view, #:pure and #:payable" c)]
          [(cons c _) (fail "expected #:returns (parameter ...), #:view, #:pure or #:payable" c)])))
    (check-names (append inputs outputs) (append input-forms output-forms))
    (raw-abi-function (name-of name) (map abi-parameter-type inputs) (map abi-parameter-type outputs)
                      inputs outputs mutability))
  (define (read-error name ps)
    (define-values (inputs forms) (read-parameters ps))
    (check-names inputs forms)
    (raw-abi-error (name-of name) (map abi-parameter-type inputs) #f inputs))
  (define (read-constructor ps mutability)
    (define-values (inputs forms) (read-parameters ps))
    (check-names inputs forms)
    (raw-abi-constructor "constructor" (map abi-parameter-type inputs) #f inputs mutability))
  (match input
    [(list 'function name ps clauses ...) (read-function name ps clauses)]
    [(list 'error name ps) (read-error name ps)]
    [(list 'constructor ps) (read-constructor ps 'nonpayable)]
    [(list 'constructor ps '#:payable) (read-constructor ps 'payable)]
    [(list 'fallback) (raw-abi-fallback 'nonpayable)]
    [(list 'fallback '#:payable) (raw-abi-fallback 'payable)]
    [(list 'receive) (raw-abi-receive)]
    [(cons 'event _) (sexp->abi-event/who who input)]
    [(cons (and head (or 'function 'error 'constructor 'fallback 'receive)) _)
     (fail (format "malformed ~a item" head) input)]
    [_ (fail "expected (function ...), (event ...), (error ...), (constructor ...), (fallback ...) or (receive)"
             input)]))

(define (parameters->sexp ps)
  (for/list ([p (in-list ps)])
    (list (and (abi-parameter-name p) (string->symbol (abi-parameter-name p)))
          (type->sexp (abi-parameter-type p)))))

(define (item->sexp i)
  (match i
    [(? abi-function?)
     `(function ,(string->symbol (abi-signature-name i))
                ,(parameters->sexp (abi-function-parameters i))
                ,@(match (abi-function-state-mutability i)
                    ['nonpayable '()]
                    [m (list (string->keyword (symbol->string m)))])
                ,@(match (abi-function-returns i)
                    ['() '()]
                    [rs (list '#:returns (parameters->sexp rs))]))]
    [(? abi-error?) `(error ,(string->symbol (abi-signature-name i)) ,(parameters->sexp (abi-error-parameters i)))]
    [(? abi-constructor?)
     `(constructor ,(parameters->sexp (abi-constructor-parameters i))
                   ,@(if (eq? (abi-constructor-state-mutability i) 'payable) '(#:payable) '()))]
    [(? abi-fallback?) (if (eq? (abi-fallback-state-mutability i) 'payable) '(fallback #:payable) '(fallback))]
    [(? abi-receive?) '(receive)]
    [(? abi-event?) (abi-event->sexp i)]))

(define (item/who who spec)
  (cond
    [(abi-item? spec) spec]
    [(string? spec) (string->abi-item/who who spec)]
    [else (sexp->abi-item/who who spec)]))
