#lang racket/base

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
         (only-in "../rpc.rkt" current-provider)
         "binding-rules.rkt"
         "contract-call.rkt"
         "exn.rkt"
         (only-in "contracts.rkt" errors/c from/c provider/c))

(provide contract-bindings-out
         define-contract)

(define address/c (abi-value/c 'address))
(define value/c exact-nonnegative-integer?)
(define block/c (flat-named-contract 'block-param/c block-param/c))
(define gas/c (or/c #f exact-positive-integer?))

(define view-procedure/c
  (->* (address/c)
       (#:from from/c #:block block/c #:gas gas/c #:errors errors/c #:provider provider/c)
       #:rest list?
       any))

(define nonpayable-procedure/c
  (->* (address/c)
       (#:from from/c #:gas gas/c #:errors errors/c #:provider provider/c)
       #:rest list?
       any))

(define payable-procedure/c
  (->* (address/c)
       (#:from from/c #:value value/c #:gas gas/c #:errors errors/c #:provider provider/c)
       #:rest list?
       any))

(define any-procedure/c
  (->* (address/c)
       (#:from from/c #:value value/c #:block block/c #:gas gas/c #:errors errors/c
        #:provider provider/c)
       #:rest list?
       any))

(define nonpayable-deploy/c
  (->* () (#:from from/c #:gas gas/c #:errors errors/c #:provider provider/c) #:rest list? any))

(define payable-deploy/c
  (->* ()
       (#:from from/c #:value value/c #:gas gas/c #:errors errors/c #:provider provider/c)
       #:rest list?
       any))

(struct bound (procedure contract positive name uses))

(define (bound-reference b negative)
  (define neg (or negative 'top-level))
  (hash-ref! (bound-uses b) neg
             (lambda ()
               (contract (bound-contract b) (bound-procedure b) (bound-positive b) neg
                         (bound-name b) #f))))

(define-syntax-parse-rule (here)
  (variable-reference->module-source (#%variable-reference)))

(define-syntax-parse-rule (define-bound id:id procedure:expr procedure/c:id solidity:str
                            (overload ...) deploy?:boolean)
  #:with (record) (generate-temporaries #'(id))
  (begin
    (define record (bound procedure procedure/c (here) 'id (make-hash)))
    (define-syntax id
      (bound-procedure-info (quote-syntax (bound-reference record (here)))
                            'id solidity '(overload ...) deploy?))))

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
  (procedure-rename (make-keyword-procedure (lambda (kws kw-args . positional)
                                              (run kws kw-args positional)))
                    name))

(define (deploy-procedure name d)
  (define constructor (contract-abi-constructor d))
  (define arity (if constructor (length (abi-signature-inputs constructor)) 0))
  (define payable? (and constructor (eq? (abi-constructor-state-mutability constructor) 'payable)))
  (define errors (contract-abi-errors d))
  (define bytecode (contract-abi-bytecode d))
  (define (run kws kw-args args)
    (unless bytecode
      (raise (exn:fail:web3 (format "~a: no bytecode" name) (current-continuation-marks))))
    (unless (= (length args) arity)
      (raise-arity name (arity-message "the constructor" (list arity) (length args) #:deploy? #t)))
    (define opts
      (options name "the constructor" "not payable" (deploy-keywords payable?) kws kw-args))
    (define (opt kw default)
      (hash-ref opts kw default))
    (define-values (address _receipt)
      (deploy-code name bytecode (or constructor "constructor()") args
                   #:from (opt '#:from current-account)
                   #:value (opt '#:value 0)
                   #:gas (opt '#:gas #f)
                   #:errors (append errors (opt '#:errors '()))
                   #:provider (opt '#:provider current-provider)))
    address)
  (procedure-rename (make-keyword-procedure (lambda (kws kw-args . args) (run kws kw-args args)))
                    name))

(begin-for-syntax
  (define (procedure-contract keywords)
    (cond
      [(equal? keywords (function-keywords 'view)) #'view-procedure/c]
      [(equal? keywords (function-keywords 'nonpayable)) #'nonpayable-procedure/c]
      [(equal? keywords (function-keywords 'payable)) #'payable-procedure/c]
      [else #'any-procedure/c]))

  (define (function-keyword-union overloads)
    (filter (lambda (kw)
              (for/or ([o (in-list overloads)])
                (memq kw (function-keywords (overload-mutability o)))))
            all-keywords))

  (define (binding-definition b d abi)
    (define id (binding-id b))
    (define indices (binding-indices b))
    (define items (contract-abi-items d))
    (case (binding-kind b)
      [(event) #`(define #,id (list-ref (contract-abi-items #,abi) #,indices))]
      [(deploy)
       (define constructor (contract-abi-constructor d))
       (define arity (if constructor (length (abi-constructor-parameters constructor)) 0))
       (define mutability
         (if constructor (abi-constructor-state-mutability constructor) 'nonpayable))
       #`(define-bound #,id (deploy-procedure '#,id #,abi)
           #,(if (eq? mutability 'payable) #'payable-deploy/c #'nonpayable-deploy/c)
           "the constructor" (#,(overload arity mutability)) #t)]
      [else
       (define functions (for/list ([k (in-list indices)]) (list-ref items k)))
       (define overloads
         (for/list ([f (in-list functions)])
           (overload (length (abi-function-parameters f)) (abi-function-state-mutability f))))
       #`(define-bound #,id (function-procedure '#,id #,abi '#,indices)
           #,(procedure-contract (function-keyword-union overloads))
           #,(abi-signature-name (car functions)) #,overloads #f)])))

(define-syntax (define-contract stx)
  (syntax-parse stx
    [(_ name:id
        (~alt src:source
              (~optional (~seq #:bytecode
                               (~var bytecode (expr/c #'bytecode/c
                                                      #:name "the #:bytecode expression"))
                               (~bind [bytecode? #t]))
                         #:name "#:bytecode clause"
                         #:too-many "too many #:bytecode clauses"
                         #:defaults ([bytecode? #f]))
              (~optional (~seq #:rename (~describe "([key rename-id] ...) after #:rename"
                                                   (rename:rename-clause ...)))
                         #:name "#:rename clause"
                         #:too-many "too many #:rename clauses"
                         #:defaults ([(rename 1) '()])))
        ...)
     #:fail-when (null? (attribute src.start)) "expected #:file or at least one item"
     #:do [(define-values (second-source reason) (source-conflict (attribute src.start)))]
     #:fail-when second-source reason
     #:do [(define d (car (attribute src.description)))
           (define deploy? (or (attribute bytecode?) (and (contract-abi-bytecode d) #t)))
           (define bindings
             (description-bindings stx #'name d (car (attribute src.where))
                                   (syntax->list #'(rename ...)) deploy?))]
     #:with (abi) (generate-temporaries '(abi))
     #:with items #`'#,(map abi-item->sexp (contract-abi-items d))
     #:with artifact-bytecode (datum->syntax #'name (contract-abi-bytecode d))
     #:with (id ...) (map binding-id bindings)
     #:with (definition ...) (map (lambda (b) (binding-definition b d #'abi)) bindings)
     #'(begin
         (define abi (contract-abi items #:bytecode (~? bytecode.c artifact-bytecode)))
         (define-syntax name
           (contract-binding-info (quote-syntax abi) (list (quote-syntax id) ...)))
         definition ...)]))

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
