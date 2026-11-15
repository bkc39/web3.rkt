#lang racket/base

;; The module graph of a library, by name, for tests that bound what a
;; module may depend on.

(require racket/match
         setup/collects)

(provide collection
         direct-imports
         import-closure
         module-name)

(define (rebase mpi self)
  (define-values (name base) (module-path-index-split mpi))
  (if name
      (module-path-index-join name (if base (rebase base self) self))
      self))

(define (direct-imports self)
  (for*/list ([phase+mpis (in-list (module->imports self))]
              #:when (car phase+mpis)
              [mpi (in-list (cdr phase+mpis))])
    (module-path-index-resolve (rebase mpi self) #t)))

(define (import-closure root)
  (let loop ([todo (list (module-path-index-resolve (module-path-index-join root #f) #t))]
             [seen '()])
    (match todo
      ['() seen]
      [(cons self rest)
       #:when (member self seen)
       (loop rest seen)]
      [(cons self rest)
       #:when (symbol? (resolved-module-path-name self))
       (loop rest (cons self seen))]
      [(cons self rest) (loop (append (direct-imports self) rest) (cons self seen))])))

;; "racket/list" for a collection module, #f for a primitive one.
(define (module-name r)
  (match (resolved-module-path-name r)
    [(or (? path? p) (cons (? path? p) _))
     (match (path->module-path p)
       [(list 'lib s) (regexp-replace #rx"[.]rkt$" s "")]
       [_ (path->string p)])]
    [_ #f]))

(define (collection name)
  (car (regexp-split #rx"/" name)))
