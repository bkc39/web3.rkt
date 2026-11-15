#lang racket/base

(require racket/contract/base
         "event-type.rkt"
         (submod "event-type.rkt" private)
         "type.rkt"
         (submod "type.rkt" private))

(provide
 abi-item-spec/c
 (contract-out
  [abi-parameter (-> (or/c #f abi-parameter-name/c) abi-type-spec/c abi-parameter?)]
  [abi-parameter? (-> any/c boolean?)]
  [abi-parameter-name (-> abi-parameter? (or/c #f string?))]
  [abi-parameter-type (-> abi-parameter? abi-type?)]

  [abi-function (->* (abi-identifier/c (listof parameter-spec/c) (listof parameter-spec/c))
                     (function-mutability/c)
                     abi-function?)]
  [abi-function? (-> any/c boolean?)]
  [abi-function-parameters (-> abi-function? (listof abi-parameter?))]
  [abi-function-returns (-> abi-function? (listof abi-parameter?))]
  [abi-function-state-mutability (-> abi-function? function-mutability/c)]
  [abi-error (-> abi-identifier/c (listof parameter-spec/c) abi-error?)]
  [abi-error? (-> any/c boolean?)]
  [abi-error-parameters (-> abi-error? (listof abi-parameter?))]
  [abi-constructor (->* ((listof parameter-spec/c)) (payability/c) abi-constructor?)]
  [abi-constructor? (-> any/c boolean?)]
  [abi-constructor-parameters (-> abi-constructor? (listof abi-parameter?))]
  [abi-constructor-state-mutability (-> abi-constructor? payability/c)]
  [abi-fallback (->* () (payability/c) abi-fallback?)]
  [abi-fallback? (-> any/c boolean?)]
  [abi-fallback-state-mutability (-> abi-fallback? payability/c)]
  [abi-receive (-> abi-receive?)]
  [abi-receive? (-> any/c boolean?)]

  [abi-item? (-> any/c boolean?)]
  [abi-item (-> abi-item-spec/c abi-item?)]
  [string->abi-item (-> string? abi-item?)]
  [sexp->abi-item (-> pair? abi-item?)]
  [abi-item->string (-> abi-item-spec/c string?)]
  [abi-item->sexp (-> abi-item-spec/c pair?)]
  [jsexpr->abi-item (-> any/c abi-item?)]
  [abi-item->jsexpr (-> abi-item-spec/c hash?)]

  [contract-abi (->* ((listof abi-item-spec/c)) (#:bytecode bytecode/c) contract-abi?)]
  [contract-abi? (-> any/c boolean?)]
  [contract-abi-items (-> contract-abi? (listof abi-item?))]
  [contract-abi-bytecode (-> contract-abi? (or/c #f bytes?))]
  [contract-abi-functions (-> contract-abi? (listof abi-function?))]
  [contract-abi-events (-> contract-abi? (listof abi-event?))]
  [contract-abi-errors (-> contract-abi? (listof abi-error?))]
  [contract-abi-constructor (-> contract-abi? (or/c #f abi-constructor?))]
  [contract-abi-fallback (-> contract-abi? (or/c #f abi-fallback?))]
  [contract-abi-receive (-> contract-abi? (or/c #f abi-receive?))]
  [contract-abi-function (->i ([d contract-abi?] [key (d n) (function-key/c d n)])
                              (#:arity [n exact-nonnegative-integer?])
                              [_ abi-function?])]
  [contract-abi-event (->i ([d contract-abi?] [key (d) (event-key/c d)]) [_ abi-event?])]
  [contract-abi-error (->i ([d contract-abi?] [key (d) (error-key/c d)]) [_ abi-error?])]
  [jsexpr->contract-abi (-> any/c contract-abi?)]
  [contract-abi->jsexpr (-> contract-abi? (listof hash?))]))

(define unimplemented
  (make-keyword-procedure (lambda _ (error 'unimplemented))))

(define parameter-spec/c
  (flat-named-contract 'parameter-spec/c (lambda (v) (or (abi-parameter? v) (abi-type-spec/c v)))))

(define function-mutability/c
  (flat-named-contract 'function-mutability/c (or/c 'pure 'view 'nonpayable 'payable)))

(define payability/c (flat-named-contract 'payability/c (or/c 'nonpayable 'payable)))

(define bytecode/c
  (flat-named-contract 'bytecode/c
                       (lambda (v)
                         (or (not v)
                             (bytes? v)
                             (and (string? v) (regexp-match? #px"^0x(?:[0-9a-fA-F]{2})*$" v))))))

(define abi-item-spec/c
  (flat-named-contract 'abi-item-spec/c (lambda (v) (or (abi-item? v) (string? v) (pair? v)))))

(define (function-key/c d n) unimplemented)
(define (event-key/c d) unimplemented)
(define (error-key/c d) unimplemented)

(struct abi-parameter (name type) #:transparent
  #:name abi-parameter-struct #:constructor-name raw-abi-parameter)
(struct abi-function abi-signature (parameters returns state-mutability) #:transparent
  #:name abi-function-struct #:constructor-name raw-abi-function)
(struct abi-error abi-signature (parameters) #:transparent
  #:name abi-error-struct #:constructor-name raw-abi-error)
(struct abi-constructor abi-signature (parameters state-mutability) #:transparent
  #:name abi-constructor-struct #:constructor-name raw-abi-constructor)
(struct abi-fallback (state-mutability) #:transparent
  #:name abi-fallback-struct #:constructor-name raw-abi-fallback)
(struct abi-receive () #:transparent #:name abi-receive-struct #:constructor-name raw-abi-receive)
(struct contract-abi (items bytecode) #:transparent
  #:name contract-abi-struct #:constructor-name raw-contract-abi)

(define (abi-parameter name type) (unimplemented))
(define (abi-function name inputs outputs [mutability 'nonpayable]) (unimplemented))
(define (abi-error name inputs) (unimplemented))
(define (abi-constructor inputs [mutability 'nonpayable]) (unimplemented))
(define (abi-fallback [mutability 'nonpayable]) (unimplemented))
(define (abi-receive) (unimplemented))
(define (contract-abi items #:bytecode [bytecode #f]) (unimplemented))

(define abi-item? unimplemented)
(define abi-item unimplemented)
(define string->abi-item unimplemented)
(define sexp->abi-item unimplemented)
(define abi-item->string unimplemented)
(define abi-item->sexp unimplemented)
(define jsexpr->abi-item unimplemented)
(define abi-item->jsexpr unimplemented)
(define contract-abi-functions unimplemented)
(define contract-abi-events unimplemented)
(define contract-abi-errors unimplemented)
(define contract-abi-constructor unimplemented)
(define contract-abi-fallback unimplemented)
(define contract-abi-receive unimplemented)
(define contract-abi-function unimplemented)
(define contract-abi-event unimplemented)
(define contract-abi-error unimplemented)
(define jsexpr->contract-abi unimplemented)
(define contract-abi->jsexpr unimplemented)
