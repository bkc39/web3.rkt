#lang racket/base

(require (for-template (only-in racket/base set!))
         (only-in compiler/cm-accomplice register-external-file)
         (only-in racket/list append-map filter-not index-of remove-duplicates)
         (only-in racket/match match match-define struct*)
         (only-in racket/string string-contains? string-join string-prefix?)
         syntax/parse
         "../abi/description.rkt"
         (only-in (submod "../abi/description.rkt" private) described-items raw-contract-abi)
         "../abi/event-type.rkt"
         (only-in (submod "../abi/event-type.rkt" private) parameters-failure)
         "../abi/type.rkt"
         (only-in (submod "../abi/type.rkt" private)
                  array-length? elementary elementary-failure identifier-end string->signature)
         (only-in "abi-item.rkt"
                  constructor-item duplicate-index error-item function-item identifier?
                  parameter-name? raw-abi-fallback raw-abi-parameter raw-abi-receive
                  string->abi-item/who)
         "abi-json.rkt"
         "binding-rules.rkt")

(provide (struct-out binding)
         (struct-out bound-procedure-info)
         (struct-out contract-binding-info)
         (struct-out overload)
         description-bindings
         rename-clause
         solidity-name->racket-name
         source
         syntax->abi-item)

(define (solidity-name->racket-name name)
  (define words
    (for*/list ([chunk (in-list (regexp-split #rx"_+" name))]
                #:unless (string=? chunk "")
                [word (in-list (regexp-split #px"(?<=[a-z0-9])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])"
                                             chunk))])
      (string-downcase word)))
  (and (pair? words) (string-join words "-")))

(define (keyword->mutability kw)
  (string->symbol (keyword->string (syntax-e kw))))

(define (duplicate-parameter forms parameters)
  (match (duplicate-index (map abi-parameter-name parameters))
    [#f (values #f #f)]
    [i (values (list-ref forms i)
               (format "duplicate parameter name: ~a"
                       (abi-parameter-name (list-ref parameters i))))]))

(define-syntax-class abi-type-sexp
  #:description "an ABI type"
  #:attributes (type)
  (pattern t:id
           #:attr type (elementary (symbol->string (syntax-e #'t)))
           #:fail-unless (attribute type) (elementary-failure (symbol->string (syntax-e #'t))))
  (pattern ((~datum array) e:abi-type-sexp)
           #:attr type (abi-array (attribute e.type)))
  (pattern ((~datum array) e:abi-type-sexp k:exact-positive-integer)
           #:fail-unless (array-length? (syntax-e #'k)) "expected an array length from 1 to 2^64-1"
           #:attr type (abi-fixed-array (attribute e.type) (syntax-e #'k)))
  (pattern ((~datum tuple) c:abi-type-sexp ...+)
           #:attr type (abi-tuple (attribute c.type))))

(define-syntax-class parameter-name
  #:description "a parameter name or #f"
  #:attributes (name)
  (pattern #f #:attr name #f)
  (pattern x:id
           #:attr name (symbol->string (syntax-e #'x))
           #:fail-unless (parameter-name? (attribute name)) "not a parameter name"))

(define-syntax-class solidity-name
  #:description "a name"
  #:attributes (name)
  (pattern x:id
           #:attr name (symbol->string (syntax-e #'x))
           #:fail-unless (identifier? (attribute name)) "not a name"))

(define-syntax-class parameter
  #:description "[name type]"
  #:attributes (parameter)
  (pattern [n:parameter-name t:abi-type-sexp]
           #:attr parameter (raw-abi-parameter (attribute n.name) (attribute t.type))))

(define-syntax-class parameters
  #:description "(parameter ...)"
  #:attributes (parameters forms)
  (pattern (p:parameter ...)
           #:attr parameters (attribute p.parameter)
           #:attr forms (syntax->list #'(p ...))))

(define-syntax-class event-parameter
  #:description "[name type] or [name type #:indexed]"
  #:attributes (parameter)
  (pattern [n:parameter-name t:abi-type-sexp
            (~optional (~and #:indexed (~bind [indexed? #t])) #:defaults ([indexed? #f]))]
           #:attr parameter
           (abi-event-parameter (attribute n.name) (attribute t.type) (attribute indexed?))))

(define-syntax-class function-clause
  #:description "(function name (parameter ...) option ...)"
  #:attributes (item)
  (pattern ((~datum function) name:solidity-name inputs:parameters
            (~alt (~optional (~seq #:returns outputs:parameters)
                             #:name "#:returns"
                             #:too-many "at most one #:returns"
                             #:defaults ([outputs.parameters '()] [outputs.forms '()]))
                  (~optional (~and mutability (~or* #:view #:pure #:payable))
                             #:too-many "at most one of #:view, #:pure and #:payable"
                             #:defaults ([mutability #'#:nonpayable])))
            ...)
           #:do [(define-values (duplicate reason)
                   (duplicate-parameter (append (attribute inputs.forms) (attribute outputs.forms))
                                        (append (attribute inputs.parameters)
                                                (attribute outputs.parameters))))]
           #:fail-when duplicate reason
           #:attr item (function-item (attribute name.name)
                                      (attribute inputs.parameters)
                                      (attribute outputs.parameters)
                                      (keyword->mutability #'mutability))))

(define-syntax-class other-clause
  #:attributes (item)
  (pattern ((~datum error) name:solidity-name inputs:parameters)
           #:do [(define-values (duplicate reason)
                   (duplicate-parameter (attribute inputs.forms) (attribute inputs.parameters)))]
           #:fail-when duplicate reason
           #:attr item (error-item (attribute name.name) (attribute inputs.parameters)))
  (pattern ((~datum constructor) inputs:parameters
            (~optional (~and mutability #:payable) #:defaults ([mutability #'#:nonpayable])))
           #:do [(define-values (duplicate reason)
                   (duplicate-parameter (attribute inputs.forms) (attribute inputs.parameters)))]
           #:fail-when duplicate reason
           #:attr item (constructor-item (attribute inputs.parameters)
                                         (keyword->mutability #'mutability)))
  (pattern ((~datum fallback)
            (~optional (~and mutability #:payable) #:defaults ([mutability #'#:nonpayable])))
           #:attr item (raw-abi-fallback (keyword->mutability #'mutability)))
  (pattern ((~datum receive))
           #:attr item (raw-abi-receive))
  (pattern ((~datum event) name:solidity-name (p:event-parameter ...)
            (~optional (~and #:anonymous (~bind [anonymous? #t])) #:defaults ([anonymous? #f])))
           #:do [(define-values (at reason)
                   (match (parameters-failure (attribute p.parameter) (attribute anonymous?))
                     [#f (values #f #f)]
                     [(cons reason i) (values (list-ref (syntax->list #'(p ...)) i) reason)]))]
           #:fail-when at reason
           #:attr item
           (abi-event (attribute name.name) (attribute p.parameter) (attribute anonymous?))))

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
  (define message (car (regexp-split #rx"\n" (exn-message e))))
  (define prefix "define-contract: ")
  (if (string-prefix? message prefix) (substring message (string-length prefix)) message))

(define (string-clause lit)
  (with-handlers ([exn:fail:web3:abi:type?
                   (lambda (e)
                     (values #f
                             (match (exn:fail:web3:abi:type-position e)
                               [#f lit]
                               [i (string-position->syntax lit i)])
                             (reason-of e)))])
    (values (string->abi-item/who 'define-contract (syntax-e lit)) #f #f)))

(define-syntax-class item-clause
  #:description "an item"
  #:attributes (item)
  (pattern s:str
           #:do [(define-values (parsed at reason) (string-clause #'s))]
           #:fail-when at reason
           #:attr item parsed)
  (pattern i:function-clause #:attr item (attribute i.item))
  (pattern i:other-clause #:attr item (attribute i.item)))

(define (syntax->abi-item stx)
  (syntax-parse stx
    [i:item-clause (attribute i.item)]))

(define (base-directory path-stx)
  (define source (syntax-source path-stx))
  (cond
    [(and (path? source) (complete-path? source))
     (let-values ([(dir _name _dir?) (split-path source)]) dir)]
    [(current-load-relative-directory)]
    [else (current-directory)]))

(define (system-error e)
  (match (regexp-match #rx"system error: ([^;\n]*)" (exn-message e))
    [(list _ reason) (string-append ": " reason)]
    [#f ""]))

(define (file-description path-stx)
  (define written (syntax-e path-stx))
  (define complete (simplify-path (path->complete-path written (base-directory path-stx))))
  (register-external-file complete)
  (with-handlers ([exn:fail:filesystem?
                   (lambda (e) (values #f (format "cannot read ~a~a" complete (system-error e))))]
                  [exn:fail:web3:abi:type? (lambda (e) (values #f (reason-of e)))])
    (values (file->contract-abi/who (string->symbol written) complete) #f)))

(define (inline-description clauses items)
  (define-values (kept failure) (described-items items))
  (match failure
    [#f (values (raw-contract-abi kept #f)
                (for/hasheq ([item (in-list items)]
                             [clause (in-list clauses)])
                  (values item clause))
                #f
                #f)]
    [(cons reason i) (values #f #f (list-ref clauses i) reason)]))

(define-splicing-syntax-class source
  #:description "#:file path or items"
  #:attributes (start description where)
  (pattern (~seq (~and start #:file) path:str)
           #:fail-when (and (string=? (syntax-e #'path) "") #'path) "expected a path"
           #:do [(define-values (d reason) (file-description #'path))]
           #:fail-when (and reason #'path) reason
           #:attr description d
           #:attr where (for/hasheq ([item (in-list (contract-abi-items d))])
                          (values item #'path)))
  (pattern (~seq clause:item-clause ...+ (~peek-not _:item-clause))
           #:do [(define-values (d where at reason)
                   (inline-description (syntax->list #'(clause ...)) (attribute clause.item)))]
           #:fail-when at reason
           #:with (start _ ...) #'(clause ...)
           #:attr description d
           #:attr where where))

(define-syntax-class rename-clause
  #:description "[key identifier]"
  #:attributes (key new)
  (pattern [key:str new:id]))

(struct overload (arity mutability) #:prefab)

(struct binding (id kind indices at))

(define (fail form at reason)
  (raise-syntax-error 'define-contract reason form at))

(define (item-name item)
  (if (abi-event? item) (abi-event-name item) (abi-signature-name item)))

(define (event-signature e)
  (abi-signature->string (abi-event->signature e)))

(define (rename-targets form d key)
  (define text (syntax-e key))
  (define (signature-of spec)
    (with-handlers ([exn:fail:web3:abi:type? (lambda (e) (fail form key (reason-of e)))])
      (abi-signature->string (string->signature 'define-contract spec))))
  (define events (contract-abi-events d))
  (define functions (contract-abi-functions d))
  (cond
    [(string-prefix? text "event ")
     (define spec (substring text 6))
     (cond
       [(string-contains? spec "(")
        (define signature (signature-of spec))
        (filter (lambda (e) (equal? (event-signature e) signature)) events)]
       [else
        (match (filter (lambda (e) (equal? (abi-event-name e) spec)) events)
          [(list _ _ _ ...) (fail form key (format "event ~a is overloaded; use \"event ~a(…)\""
                                                  spec spec))]
          [found found])])]
    [(string-contains? text "(")
     (define signature (signature-of text))
     (filter (lambda (f) (equal? (abi-signature->string f) signature)) functions)]
    [else (filter (lambda (f) (equal? (abi-signature-name f) text)) functions)]))

(define (renamed-groups form d renames)
  (for/list ([r (in-list renames)])
    (syntax-parse r
      [c:rename-clause
       (match (rename-targets form d #'c.key)
         ['() (fail form #'c.key (format "#:rename: no function or event ~a" (syntax-e #'c.key)))]
         [targets (cons #'c.new targets)])])))

(define (group-overloads functions)
  (for/list ([name (in-list (remove-duplicates (map abi-signature-name functions)))])
    (filter (lambda (f) (equal? (abi-signature-name f) name)) functions)))

(define (check-arities form where group)
  (for/fold ([seen '()] #:result (void))
            ([f (in-list group)])
    (define n (length (abi-signature-inputs f)))
    (match (assv n seen)
      [#f (cons (cons n f) seen)]
      [(cons _ other)
       (fail form (hash-ref where f)
             (format "~a and ~a both take ~a; use #:rename"
                     (abi-signature->string other) (abi-signature->string f)
                     (count-of n "argument")))])))

(define (check-event-signatures form where events)
  (for/fold ([seen (hash)] #:result (void))
            ([e (in-list events)])
    (define signature (event-signature e))
    (match (hash-ref seen signature #f)
      [#f (hash-set seen signature e)]
      [other
       (fail form (hash-ref where e)
             (format "two events share the signature ~a, so they cannot be bound: ~a and ~a"
                     signature (abi-event->string other) (abi-event->string e)))])))

(define (binding-label b items)
  (match b
    [(struct* binding ([kind 'deploy])) "the deployment"]
    [(struct* binding ([kind 'event] [indices i]))
     (format "event ~a" (abi-event-name (list-ref items i)))]
    [(struct* binding ([indices (cons i _)])) (abi-signature-name (list-ref items i))]))

(define (check-collisions form bindings items)
  (define (position b)
    (match (binding-indices b)
      [(cons i _) i]
      [(? exact-integer? i) i]
      [_ (length items)]))
  (for/fold ([seen '()] #:result (void))
            ([b (in-list (sort bindings < #:key position))])
    (define key (syntax-e (binding-id b)))
    (match (assq key seen)
      [#f (cons (cons key b) seen)]
      [(cons _ other)
       (fail form (if (eq? (binding-kind b) 'deploy) (binding-at other) (binding-at b))
             (format "two bindings are named ~a: ~a and ~a; use #:rename"
                     key (binding-label other items) (binding-label b items)))])))

(define (description-bindings form name d where renames deploy?)
  (define items (contract-abi-items d))
  (define (index-of-item i)
    (index-of items i eq?))
  (define (named suffix)
    (datum->syntax name (string->symbol (format "~a-~a" (syntax-e name) suffix)) name))
  (define (racket-name item)
    (or (solidity-name->racket-name (item-name item))
        (fail form (hash-ref where item)
              (format "no Racket name for ~a; use #:rename" (item-name item)))))
  (define (binding-of id targets)
    (match targets
      [(list (? abi-event? e)) (binding id 'event (index-of-item e) (hash-ref where e))]
      [fs (binding id 'function (map index-of-item fs) (hash-ref where (car fs)))]))
  (check-event-signatures form where (contract-abi-events d))
  (define renamed (renamed-groups form d renames))
  (define taken (append-map cdr renamed))
  (define groups
    (group-overloads (filter-not (lambda (f) (memq f taken)) (contract-abi-functions d))))
  (for ([group (in-list (append groups (map cdr renamed)))]
        #:unless (abi-event? (car group)))
    (check-arities form where group))
  (define bindings
    (append (for/list ([group (in-list groups)])
              (binding-of (named (racket-name (car group))) group))
            (for/list ([e (in-list (contract-abi-events d))]
                       #:unless (memq e taken))
              (binding-of (named (format "~a-event" (racket-name e))) (list e)))
            (for/list ([r (in-list renamed)])
              (binding-of (car r) (cdr r)))
            (if deploy? (list (binding (named "deploy") 'deploy '() form)) '())))
  (check-collisions form bindings items)
  bindings)

(struct contract-binding-info (reference names)
  #:property prop:procedure
  (lambda (self stx)
    (syntax-parse stx
      [_:id (contract-binding-info-reference self)]
      [(_ . args)
       (datum->syntax stx (cons (contract-binding-info-reference self) #'args) stx stx)])))

(define-splicing-syntax-class argument
  #:description "an argument"
  #:attributes (keyword)
  (pattern (~seq kw:keyword (~describe "a value after the keyword" _:expr))
           #:attr keyword #'kw)
  (pattern _:expr #:attr keyword #f))

(define (check-application self stx)
  (match-define (struct* bound-procedure-info ([name name] [solidity what] [overloads overloads]
                                               [deploy? deploy?]))
    self)
  (define (refuse at message)
    (raise-syntax-error name message stx at))
  (when (syntax->list stx)
    (syntax-parse stx
      [(_ arg:argument ...)
       #:do [(define keywords (filter values (attribute arg.keyword)))
             (define positional (- (length (attribute arg.keyword)) (length keywords)))
             (define given (if deploy? positional (max 0 (sub1 positional))))
             (define chosen (findf (lambda (o) (= (overload-arity o) given)) overloads))]
       #:fail-when (and (or (not chosen) (and (not deploy?) (zero? positional))) stx)
       (arity-message what (map overload-arity overloads) given #:deploy? deploy?)
       #:do [(define mutability (overload-mutability chosen))
             (define allowed
               (if deploy?
                   (deploy-keywords (eq? mutability 'payable))
                   (function-keywords mutability)))]
       (for ([kw (in-list keywords)]
             #:unless (memq (syntax-e kw) allowed))
         (refuse kw (keyword-message (if deploy? "the constructor" what)
                                     (if deploy? "not payable" mutability)
                                     (syntax-e kw)
                                     allowed)))])))

(struct bound-procedure-info (reference name solidity overloads deploy?)
  #:property prop:set!-transformer
  (lambda (self stx)
    (define reference (bound-procedure-info-reference self))
    (syntax-parse stx
      #:literals (set!)
      [(set! _ _)
       (raise-syntax-error (bound-procedure-info-name self) "cannot mutate a bound procedure" stx)]
      [_:id reference]
      [(_ . args)
       (check-application self stx)
       (datum->syntax stx (cons reference #'args) stx stx)])))
