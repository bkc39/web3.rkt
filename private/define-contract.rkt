#lang racket/base

;; define-contract's expansion-time half: the item and option syntax, the
;; descriptions they denote, the names they bind, and the transformers that
;; check applications of bound procedures.  Never loads keccak.

(require (for-template (only-in racket/base set!))
         (only-in compiler/cm-accomplice register-external-file)
         (only-in racket/list index-of remove-duplicates)
         (only-in racket/match match match-define struct*)
         (only-in racket/string string-contains? string-join string-prefix?)
         syntax/parse
         "../abi/description.rkt"
         (submod "../abi/description.rkt" private)
         "../abi/event-type.rkt"
         "../abi/type.rkt"
         (only-in (submod "../abi/type.rkt" private) identifier-end string->signature)
         (only-in "abi-item.rkt" identifier? parameter-name? sexp->abi-item/who string->abi-item/who)
         "abi-json.rkt"
         "binding-rules.rkt")

(provide (struct-out binding)
         (struct-out bound-procedure-info)
         (struct-out contract-binding-info)
         (struct-out overload)
         description-bindings
         file-description
         inline-description
         item-clause
         rename-clause
         second-of
         solidity-name->racket-name)

;;;; names

(define (solidity-name->racket-name name)
  (define words
    (for*/list ([chunk (in-list (regexp-split #rx"_+" name))]
                #:unless (string=? chunk "")
                [word (in-list (regexp-split #px"(?<=[a-z0-9])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])" chunk))])
      (string-downcase word)))
  (and (pair? words) (string-join words "-")))

;;;; item syntax

(define (type-datum? d)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (sexp->abi-type d)
    #t))

(define-syntax-class abi-type-sexp
  #:description "an ABI type"
  #:attributes ()
  (pattern t:id #:fail-unless (type-datum? (syntax-e #'t)) "not an ABI type")
  (pattern ((~datum array) _:abi-type-sexp (~optional _:exact-positive-integer)))
  (pattern ((~datum tuple) _:abi-type-sexp ...+)))

(define-syntax-class parameter-name
  #:description "a parameter name or #f"
  #:attributes ()
  (pattern #f)
  (pattern x:id #:fail-unless (parameter-name? (symbol->string (syntax-e #'x))) "not a parameter name"))

(define-syntax-class item-name
  #:description "a name"
  #:attributes ()
  (pattern x:id #:fail-unless (identifier? (symbol->string (syntax-e #'x))) "not a name"))

(define-syntax-class parameter
  #:description "[name type]"
  #:attributes ()
  (pattern [_:parameter-name _:abi-type-sexp]))

(define-syntax-class event-parameter
  #:description "[name type] or [name type #:indexed]"
  #:attributes ()
  (pattern [_:parameter-name _:abi-type-sexp (~optional #:indexed)]))

(define-syntax-class function-item
  #:attributes ()
  (pattern ((~datum function) _:item-name (_:parameter ...)
            (~alt (~seq (~and returns #:returns) (_:parameter ...))
                  (~and mutability (~or* #:view #:pure #:payable)))
            ...)
           #:fail-when (second-of #'(returns ...)) "at most one #:returns"
           #:fail-when (second-of #'(mutability ...)) "at most one of #:view, #:pure and #:payable"))

(define (second-of stx)
  (match (syntax->list stx)
    [(list* _ second _) second]
    [_ #f]))

(define-syntax-class other-item
  #:attributes ()
  (pattern ((~datum error) _:item-name (_:parameter ...)))
  (pattern ((~datum constructor) (_:parameter ...) (~optional #:payable)))
  (pattern ((~datum fallback) (~optional #:payable)))
  (pattern ((~datum receive)))
  (pattern ((~datum event) _:item-name (_:event-parameter ...) (~optional #:anonymous))))

(define item-heads '(function event error constructor fallback receive))

(define-syntax-class item-clause
  #:description "an item"
  #:attributes ()
  (pattern _:str)
  (pattern (head:id . _)
           #:when (memq (syntax-e #'head) item-heads)))

(define-syntax-class rename-clause
  #:description "[key identifier]"
  #:attributes (key new)
  (pattern [key:str new:id]))

;;;; descriptions

;; The syntax at index i of the string literal lit, spanning the token
;; there, or lit itself when its text is not the string's.
(define (string-position->syntax lit i)
  (define s (syntax-e lit))
  (define n (string-length s))
  (cond
    [(and (syntax-span lit) (= (syntax-span lit) (+ 2 n)) (syntax-position lit))
     (define span
       (cond
         [(>= i n) 1]
         [(identifier-end s i) => (lambda (end) (- end i))]
         [else 1]))
     (datum->syntax lit
                    (substring s (min i n) (min n (+ i span)))
                    (list (syntax-source lit)
                          (syntax-line lit)
                          (and (syntax-column lit) (+ (syntax-column lit) 1 i))
                          (+ (syntax-position lit) 1 i)
                          span))]
    [else lit]))

(define (reason-of e)
  (define message (exn-message e))
  (define prefix "define-contract: ")
  (if (string-prefix? message prefix) (substring message (string-length prefix)) message))

(define (fail form at reason)
  (raise-syntax-error 'define-contract reason form at))

;; The item a clause denotes, raising at its offending syntax.
(define (clause->item form clause)
  (define (fail-at at e)
    (fail form at (car (regexp-split #rx"\n" (reason-of e)))))
  (syntax-parse clause
    [s:str
     (with-handlers ([exn:fail:web3:abi:type?
                      (lambda (e)
                        (fail-at (match (exn:fail:web3:abi:type-position e)
                                   [#f clause]
                                   [i (string-position->syntax clause i)])
                                 e))])
       (string->abi-item/who 'define-contract (syntax-e #'s)))]
    [((~datum function) . _)
     (syntax-parse clause #:context form [_:function-item (void)])
     (with-handlers ([exn:fail:web3:abi:type? (lambda (e) (fail-at clause e))])
       (sexp->abi-item/who 'define-contract (syntax->datum clause)))]
    [_
     (syntax-parse clause #:context form [_:other-item (void)])
     (with-handlers ([exn:fail:web3:abi:type? (lambda (e) (fail-at clause e))])
       (sexp->abi-item/who 'define-contract (syntax->datum clause)))]))

;; The description of inline clauses.
(define (inline-description form clauses)
  (define items (for/list ([c (in-list clauses)]) (clause->item form c)))
  (match (contract-abi-failure items)
    [#f (raw-contract-abi items #f)]
    [(cons reason i) (fail form (list-ref clauses i) reason)]))

;; The directory a relative #:file in path-stx is read from.
(define (base-directory path-stx)
  (define source (syntax-source path-stx))
  (cond
    [(and (path? source) (complete-path? source))
     (let-values ([(dir _name _dir?) (split-path source)]) dir)]
    [(current-load-relative-directory)]
    [else (current-directory)]))

;; The description in the file path-stx names.
(define (file-description form path-stx)
  (define written (syntax-e path-stx))
  (define complete (simplify-path (path->complete-path written (base-directory path-stx))))
  (register-external-file complete)
  (with-handlers ([exn:fail:filesystem?
                   (lambda (e) (fail form path-stx (format "cannot read ~a: ~a" written (exn-message e))))]
                  [exn:fail:web3:abi:type? (lambda (e) (fail form path-stx (exn-message e)))])
    (file->contract-abi/who (string->symbol written) complete)))

;;;; bindings

;; An overload of a bound procedure: its input count and mutability.
(struct overload (arity mutability) #:prefab)

;; One name define-contract binds: id, kind 'function (indices of its
;; functions), 'event (the event's index) or 'deploy, and the syntax a
;; collision is reported at.
(struct binding (id kind indices at))

;; The bindings of description d for name; at lists the syntax of each
;; item, renames the #:rename clauses.
(define (description-bindings form name d at renames deploy?)
  (define items (contract-abi-items d))
  (define (index-of-item i)
    (index-of items i eq?))
  (define (named suffix)
    (datum->syntax name (string->symbol (format "~a-~a" (syntax-e name) suffix)) name))
  (define (racket-name item)
    (or (solidity-name->racket-name (if (abi-event? item) (abi-event-name item) (abi-signature-name item)))
        (fail form (list-ref at (index-of-item item))
              (format "no Racket name for ~a; use #:rename" (abi-signature-name item)))))
  (define functions (contract-abi-functions d))
  (define events (contract-abi-events d))
  (define (rename-targets key)
    (define text (syntax-e key))
    (define (functions-for match?)
      (filter match? functions))
    (cond
      [(string-prefix? text "event ")
       (define spec (substring text 6))
       (filter (lambda (e)
                 (if (string-contains? spec "(")
                     (equal? (abi-signature->string (abi-event->signature e))
                             (abi-signature->string (string->signature 'define-contract spec)))
                     (equal? (abi-event-name e) spec)))
               events)]
      [(string-contains? text "(")
       (define sig (abi-signature->string (string->signature 'define-contract text)))
       (functions-for (lambda (f) (equal? (abi-signature->string f) sig)))]
      [else (functions-for (lambda (f) (equal? (abi-signature-name f) text)))]))
  (define renamed
    (for/list ([r (in-list renames)])
      (syntax-parse r
        [c:rename-clause
         (match (with-handlers ([exn:fail:web3:abi:type? (lambda (e) (fail form #'c.key (reason-of e)))])
                  (rename-targets #'c.key))
           ['() (fail form #'c.key (format "#:rename: no function or event ~a" (syntax-e #'c.key)))]
           [targets (cons #'c.new targets)])])))
  (define taken (apply append (map cdr renamed)))
  (define (binding-of id targets)
    (match targets
      [(list (? abi-event? e)) (binding id 'event (index-of-item e) (list-ref at (index-of-item e)))]
      [fs (binding id 'function (map index-of-item fs) (list-ref at (index-of-item (car fs))))]))
  (define remaining (filter (lambda (f) (not (memq f taken))) functions))
  (define groups
    (for/list ([n (in-list (remove-duplicates (map abi-signature-name remaining)))])
      (filter (lambda (f) (equal? (abi-signature-name f) n)) remaining)))
  (for ([group (in-list (append groups (map cdr renamed)))]
        #:unless (abi-event? (car group)))
    (check-arities form group at index-of-item))
  (define all
    (append (for/list ([group (in-list groups)])
              (binding-of (named (racket-name (car group))) group))
            (for/list ([e (in-list events)]
                       #:unless (memq e taken))
              (binding-of (named (format "~a-event" (racket-name e))) (list e)))
            (for/list ([r (in-list renamed)])
              (binding-of (car r) (cdr r)))
            (if deploy? (list (binding (named "deploy") 'deploy '() form)) '())))
  (check-collisions form all items)
  all)

(define (check-arities form fs at index-of-item)
  (for/fold ([seen '()] #:result (void))
            ([f (in-list fs)])
    (define n (length (abi-signature-inputs f)))
    (match (assv n seen)
      [#f (cons (cons n f) seen)]
      [(cons _ other)
       (fail form (list-ref at (index-of-item f))
             (format "~a and ~a both take ~a argument~a; use #:rename"
                     (abi-signature->string other) (abi-signature->string f) n (if (= n 1) "" "s")))])))

(define (binding-label b items)
  (match b
    [(struct* binding ([kind 'deploy])) "the deployment"]
    [(struct* binding ([kind 'event] [indices i])) (format "event ~a" (abi-event-name (list-ref items i)))]
    [(struct* binding ([indices (cons i _)])) (abi-signature-name (list-ref items i))]))

(define (check-collisions form all items)
  (define (position b)
    (match (binding-indices b)
      [(cons i _) i]
      [(? exact-integer? i) i]
      [_ (length items)]))
  (define ordered (sort all < #:key position))
  (for/fold ([seen '()] #:result (void))
            ([b (in-list ordered)])
    (define key (syntax-e (binding-id b)))
    (match (assq key seen)
      [#f (cons (cons key b) seen)]
      [(cons _ other)
       (fail form (if (eq? (binding-kind b) 'deploy) (binding-at other) (binding-at b))
             (format "two bindings are named ~a: ~a and ~a; use #:rename"
                     key (binding-label other items) (binding-label b items)))])))

;;;; static information

;; What a contract binding knows at expansion: the reference to its
;; description and the names it introduced.
(struct contract-binding-info (reference names)
  #:property prop:procedure
  (lambda (self stx)
    (syntax-parse stx
      [_:id (contract-binding-info-reference self)]
      [(_ . args) (datum->syntax stx (cons (contract-binding-info-reference self) #'args) stx stx)])))

(define (check-application self stx)
  (match-define (struct* bound-procedure-info ([name name] [solidity what] [overloads overloads]
                                               [deploy? deploy?]))
    self)
  (define-values (positional keywords)
    (let loop ([args (cdr (syntax->list stx))] [positional 0] [keywords '()])
      (match args
        ['() (values positional (reverse keywords))]
        [(list* kw _ rest) #:when (keyword? (syntax-e kw)) (loop rest positional (cons kw keywords))]
        [(cons _ rest) (loop rest (add1 positional) keywords)])))
  (define given (if deploy? positional (max 0 (sub1 positional))))
  (define chosen (findf (lambda (o) (= (overload-arity o) given)) overloads))
  (define (refuse at message)
    (raise-syntax-error name message stx at))
  (cond
    [(or (not chosen) (and (not deploy?) (zero? positional)))
     (refuse #f (arity-message what (map overload-arity overloads) given #:deploy? deploy?))]
    [else
     (define mutability (overload-mutability chosen))
     (define allowed (if deploy? (deploy-keywords (eq? mutability 'payable)) (function-keywords mutability)))
     (for ([kw (in-list keywords)]
           #:unless (memq (syntax-e kw) allowed))
       (refuse kw (keyword-message (if deploy? "the constructor" what)
                                   (if deploy? "not payable" mutability)
                                   (syntax-e kw)
                                   allowed)))]))

;; What a bound procedure knows at expansion: the reference to its
;; contracted procedure, its name, the Solidity name, and its overloads.
(struct bound-procedure-info (reference name solidity overloads deploy?)
  #:property prop:set!-transformer
  (lambda (self stx)
    (define reference (bound-procedure-info-reference self))
    (syntax-parse stx
      #:literals (set!)
      [(set! _ _) (raise-syntax-error (bound-procedure-info-name self) "cannot mutate a bound procedure" stx)]
      [_:id reference]
      [(_ . args)
       (check-application self stx)
       (datum->syntax stx (cons reference #'args) stx stx)])))
