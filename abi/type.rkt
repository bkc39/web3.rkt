#lang racket/base

;; ABI types: one struct core, read from canonical strings and
;; s-expressions.  Requires nothing beyond racket/base's own libraries, so
;; macros can use it at phase 1.

(require racket/contract/base
         "../private/exn.rkt")

(provide
 (struct-out exn:fail:web3:abi)
 (struct-out exn:fail:web3:abi:type)
 (struct-out exn:fail:web3:abi:value)
 abi-type-spec/c
 (contract-out
  (struct abi-uint ([bits abi-int-bits/c]))
  (struct abi-int ([bits abi-int-bits/c]))
  (struct abi-address ())
  (struct abi-bool ())
  (struct abi-fixed-bytes ([size abi-fixed-bytes-size/c]))
  (struct abi-bytes ())
  (struct abi-string ())
  (struct abi-array ([element abi-type-spec/c]))
  (struct abi-fixed-array ([element abi-type-spec/c] [length exact-positive-integer?]))
  (struct abi-tuple ([components (non-empty-listof abi-type-spec/c)]))
  (struct abi-signature ([name abi-identifier/c]
                         [inputs (listof abi-type-spec/c)]
                         [outputs (or/c #f (listof abi-type-spec/c))]))
  [abi-type? (-> any/c boolean?)]
  [abi-type (-> abi-type-spec/c abi-type?)]
  [string->abi-type (-> string? abi-type?)]
  [sexp->abi-type (-> (or/c symbol? pair?) abi-type?)]
  [abi-type->string (-> abi-type-spec/c string?)]
  [abi-type->sexp (-> abi-type-spec/c (or/c symbol? pair?))]
  [abi-type-dynamic? (-> abi-type-spec/c boolean?)]
  [abi-type-static-size (-> abi-type-spec/c (or/c #f exact-positive-integer?))]
  [abi-value? (-> abi-type-spec/c any/c boolean?)]
  [abi-value/c (-> abi-type-spec/c flat-contract?)]
  [string->abi-signature (-> string? abi-signature?)]
  [abi-signature->string (->* (abi-signature?) (#:outputs? boolean?) string?)]))

(struct exn:fail:web3:abi exn:fail:web3 ())
(struct exn:fail:web3:abi:type exn:fail:web3:abi (input position))
(struct exn:fail:web3:abi:value exn:fail:web3:abi (type value path))

;;;; contracts

(define abi-type-spec/c
  (flat-named-contract 'abi-type-spec/c
                       (lambda (v) (or (abi-type? v) (string? v) (symbol? v) (pair? v)))))

(define abi-int-bits/c
  (flat-named-contract 'abi-int-bits/c
                       (lambda (v) (and (exact-integer? v) (<= 8 v 256) (zero? (remainder v 8))))))

(define abi-fixed-bytes-size/c
  (flat-named-contract 'abi-fixed-bytes-size/c
                       (lambda (v) (and (exact-integer? v) (<= 1 v 32)))))

(define abi-identifier/c
  (flat-named-contract 'abi-identifier/c
                       (lambda (v) (and (string? v) (regexp-match? #px"^[A-Za-z_$][A-Za-z0-9_$]*$" v)))))

;;;; structs

(define (type-guard . fields) (error 'unimplemented))

(struct abi-uint (bits) #:transparent)
(struct abi-int (bits) #:transparent)
(struct abi-address () #:transparent)
(struct abi-bool () #:transparent)
(struct abi-fixed-bytes (size) #:transparent)
(struct abi-bytes () #:transparent)
(struct abi-string () #:transparent)
(struct abi-array (element) #:transparent #:guard type-guard)
(struct abi-fixed-array (element length) #:transparent #:guard type-guard)
(struct abi-tuple (components) #:transparent #:guard type-guard)
(struct abi-signature (name inputs outputs) #:transparent #:guard type-guard)

(define (abi-type? v)
  (or (abi-uint? v) (abi-int? v) (abi-address? v) (abi-bool? v) (abi-fixed-bytes? v)
      (abi-bytes? v) (abi-string? v) (abi-array? v) (abi-fixed-array? v) (abi-tuple? v)))

;;;; conversions

(define (abi-type/who who spec) (error 'unimplemented))
(define (abi-type spec) (error 'unimplemented))
(define (string->abi-type s) (error 'unimplemented))
(define (sexp->abi-type v) (error 'unimplemented))
(define (abi-type->string t) (error 'unimplemented))
(define (abi-type->sexp t) (error 'unimplemented))
(define (abi-type-dynamic? t) (error 'unimplemented))
(define (abi-type-static-size t) (error 'unimplemented))
(define (abi-value? t v) (error 'unimplemented))
(define (abi-value/c t) (error 'unimplemented))
(define (string->abi-signature s) (error 'unimplemented))
(define (abi-signature->string sig #:outputs? [outputs? #f]) (error 'unimplemented))
