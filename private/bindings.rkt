#lang racket/base

;; define-contract, contract-bindings-out, and the procedures they bind.

(require (for-syntax racket/base
                     racket/provide-transform
                     syntax/parse
                     "../abi/description.rkt"
                     (only-in "../abi/type.rkt" abi-signature-name)
                     "binding-rules.rkt"
                     "define-contract.rkt")
         racket/contract/base
         (only-in racket/match match)
         syntax/parse/define
         "../abi.rkt"
         (only-in (submod "../abi/description.rkt" private) bytecode/c)
         (only-in "../eth.rkt" block-param/c)
         (only-in "../rpc.rkt" current-provider provider?)
         "binding-rules.rkt"
         "transaction.rkt")

(provide contract-bindings-out
         define-contract)

;;;; the procedures

(define from/c (or/c #f (abi-value/c 'address)))
(define value/c exact-nonnegative-integer?)
(define block/c (flat-named-contract 'block-param/c block-param/c))
(define gas/c (or/c #f exact-positive-integer?))
(define errors/c (listof (or/c string? abi-signature?)))
(define provider/c (or/c #f provider?))

;; A bound procedure, its contract, the module that defines it, and its
;; contracted forms by the module using them.
(struct bound (procedure contract positive name uses))

(define (bound-reference b negative)
  (define neg (or negative 'top-level))
  (hash-ref! (bound-uses b) neg
             (lambda () (contract (bound-contract b) (bound-procedure b) (bound-positive b) neg (bound-name b) #f))))

(define-syntax-parse-rule (here)
  (variable-reference->module-source (#%variable-reference)))

(define (raise-arity name message)
  (raise (exn:fail:contract:arity (format "~a: ~a" name message) (current-continuation-marks))))

(define (options name what how allowed kws kw-args)
  (for/hasheq ([kw (in-list kws)]
               [v (in-list kw-args)])
    (unless (memq kw allowed)
      (raise (exn:fail:contract (format "~a: ~a" name (keyword-message what how kw allowed))
                                (current-continuation-marks))))
    (values kw v)))

(define (function-procedure name d indices)
  (define items (contract-abi-items d))
  (define functions (for/list ([k (in-list indices)]) (list-ref items k)))
  (define errors (contract-abi-errors d))
  (define what (abi-signature-name (car functions)))
  (define (inputs f)
    (length (abi-signature-inputs f)))
  (define (run kws kw-args positional)
    (define given (max 0 (sub1 (length positional))))
    (define f (and (pair? positional) (findf (lambda (f) (= (inputs f) given)) functions)))
    (unless f
      (raise-arity name (arity-message what (map inputs functions) given)))
    (define mutability (abi-function-state-mutability f))
    (define opts (options name what mutability (function-keywords mutability) kws kw-args))
    (define (opt kw default)
      (hash-ref opts kw default))
    (define address (car positional))
    (define args (cdr positional))
    (define all-errors (append errors (opt '#:errors '())))
    (match mutability
      [(or 'view 'pure)
       (call-function name address f args
                      #:from (opt '#:from current-account)
                      #:value 0
                      #:gas (opt '#:gas #f)
                      #:block (opt '#:block "latest")
                      #:errors all-errors
                      #:provider (opt '#:provider current-provider))]
      [_
       (send-function name address f args
                      #:from (opt '#:from current-account)
                      #:value (opt '#:value 0)
                      #:gas (opt '#:gas #f)
                      #:errors all-errors
                      #:provider (opt '#:provider current-provider))]))
  (procedure-rename (make-keyword-procedure (lambda (kws kw-args . positional) (run kws kw-args positional)))
                    name))

(define (deploy-procedure name d)
  (define constructor (contract-abi-constructor d))
  (define arity (if constructor (length (abi-signature-inputs constructor)) 0))
  (define payable? (and constructor (eq? (abi-constructor-state-mutability constructor) 'payable)))
  (define errors (contract-abi-errors d))
  (define (run kws kw-args args)
    (unless (= (length args) arity)
      (raise-arity name (arity-message "the constructor" (list arity) (length args) #:deploy? #t)))
    (define opts (options name "the constructor" "not payable" (deploy-keywords payable?) kws kw-args))
    (define (opt kw default)
      (hash-ref opts kw default))
    (define-values (address _receipt)
      (deploy-code name (contract-abi-bytecode d) (or constructor "constructor()") args
                   #:from (opt '#:from current-account)
                   #:value (opt '#:value 0)
                   #:gas (opt '#:gas #f)
                   #:errors (append errors (opt '#:errors '()))
                   #:provider (opt '#:provider current-provider)))
    address)
  (procedure-rename (make-keyword-procedure (lambda (kws kw-args . args) (run kws kw-args args))) name))

;;;; the forms

(begin-for-syntax
  (define (keyword-contract kw)
    (case kw
      [(#:from) #'from/c]
      [(#:value) #'value/c]
      [(#:block) #'block/c]
      [(#:gas) #'gas/c]
      [(#:errors) #'errors/c]
      [(#:provider) #'provider/c]))

  (define (keyword-contracts keywords)
    (for*/list ([kw (in-list all-keywords)]
                #:when (memq kw keywords)
                [part (in-list (list (datum->syntax #f kw) (keyword-contract kw)))])
      part))

  ;; The definitions of a bound procedure named id: its runtime record p,
  ;; and its static information.
  (define (procedure-definitions id procedure keywords positional solidity overloads deploy?)
    (with-syntax ([(p) (generate-temporaries (list id))])
      (list #`(define p
                (bound #,procedure
                       (->* #,positional (#,@(keyword-contracts keywords)) #:rest list? any)
                       (here)
                       '#,id
                       (make-hash)))
            #`(define-syntax #,id
                (bound-procedure-info (quote-syntax (bound-reference p (here))) '#,id #,solidity
                                      '#,overloads #,deploy?)))))

  ;; The definitions of binding b of description d, whose runtime value
  ;; abi names.
  (define (binding-definitions b d abi)
    (define id (binding-id b))
    (define indices (binding-indices b))
    (case (binding-kind b)
      [(event) (list #`(define #,id (list-ref (contract-abi-items #,abi) #,indices)))]
      [(deploy)
       (define constructor (contract-abi-constructor d))
       (define arity (if constructor (length (abi-constructor-parameters constructor)) 0))
       (define mutability (if constructor (abi-constructor-state-mutability constructor) 'nonpayable))
       (procedure-definitions id #`(deploy-procedure '#,id #,abi) (deploy-keywords (eq? mutability 'payable))
                              #'() "the constructor" (list (overload arity mutability)) #t)]
      [else
       (define functions (for/list ([k (in-list indices)]) (list-ref (contract-abi-items d) k)))
       (define overloads
         (for/list ([f (in-list functions)])
           (overload (length (abi-function-parameters f)) (abi-function-state-mutability f))))
       (procedure-definitions id #`(function-procedure '#,id #,abi '#,indices)
                              (apply append (map (lambda (o) (function-keywords (overload-mutability o))) overloads))
                              #'((abi-value/c 'address)) (abi-signature-name (car functions)) overloads #f)])))

(define-syntax (define-contract stx)
  (syntax-parse stx
    [(_ name:id
        (~alt (~seq (~and file-kw #:file) path:str)
              (~seq (~and bytecode-kw #:bytecode)
                    (~var bytecode (expr/c #'bytecode/c #:name "the #:bytecode expression")))
              (~seq (~and rename-kw #:rename) (rename:rename-clause ...))
              item:item-clause)
        ...)
     #:fail-when (second-of #'(file-kw ...)) "too many #:file clauses"
     #:fail-when (second-of #'(bytecode-kw ...)) "too many #:bytecode clauses"
     #:fail-when (second-of #'(rename-kw ...)) "too many #:rename clauses"
     #:do [(define files (syntax->list #'(path ...)))
           (define items (syntax->list #'(item ...)))]
     #:fail-when (and (pair? files) (pair? items) (car items)) "#:file excludes inline items"
     #:fail-unless (or (pair? files) (pair? items)) "expected #:file or at least one item"
     (define d
       (if (pair? files) (file-description stx (car files)) (inline-description stx items)))
     (define bytecode-exprs (syntax->list #'(bytecode.c ...)))
     (define at (if (pair? files) (map (lambda (_) (car files)) (contract-abi-items d)) items))
     (define deploy? (or (pair? bytecode-exprs) (and (contract-abi-bytecode d) #t)))
     (define bindings
       (description-bindings stx #'name d at (syntax->list #'(rename ... ...)) deploy?))
     (with-syntax ([(abi) (generate-temporaries '(abi))])
       #`(begin
           (define abi
             (contract-abi '#,(map abi-item->sexp (contract-abi-items d))
                           #:bytecode #,(if (pair? bytecode-exprs) (car bytecode-exprs) (contract-abi-bytecode d))))
           (define-syntax name
             (contract-binding-info (quote-syntax abi)
                                    (list #,@(for/list ([b (in-list bindings)]) #`(quote-syntax #,(binding-id b))))))
           #,@(apply append (for/list ([b (in-list bindings)]) (binding-definitions b d #'abi)))))]))

(define-syntax contract-bindings-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ id:id ...)
        (define names
          (for/list ([i (in-list (syntax->list #'(id ...)))])
            (define info (syntax-local-value i (lambda () #f)))
            (unless (contract-binding-info? info)
              (raise-syntax-error #f "not a define-contract binding" stx i))
            (cons i (contract-binding-info-names info))))
        (expand-export #`(combine-out #,@(apply append names)) modes)]))))
