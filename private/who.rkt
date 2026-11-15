#lang racket/base

;; (define/who (name who formal ... . rest) body ...) defines name/who,
;; which takes who first and then name's own arguments, and name, which
;; passes 'name.  A formal [x #:public v] is not one of name's arguments:
;; name passes v for it.

(require (for-syntax racket/base
                     (only-in racket/syntax format-id)
                     syntax/parse)
         syntax/parse/define)

(provide define/who)

(begin-for-syntax
  (define-splicing-syntax-class formal
    (pattern x:id
             #:with (public ...) #'(x)
             #:with (private ...) #'(x)
             #:with (pass ...) #'(x))
    (pattern [x:id #:public value:expr]
             #:with (public ...) #'()
             #:with (private ...) #'(x)
             #:with (pass ...) #'(value))
    (pattern [x:id default:expr]
             #:with (public ...) #'([x default])
             #:with (private ...) #'([x default])
             #:with (pass ...) #'(x))
    (pattern (~seq kw:keyword [x:id default:expr])
             #:with (public ...) #'(kw [x default])
             #:with (private ...) #'(kw [x default])
             #:with (pass ...) #'(kw x))))

(define-syntax-parse-rule (define/who (name:id who:id f:formal ... . (~or* () rest:id))
                            body:expr ...+)
  #:with name/who (format-id #'name "~a/who" #'name #:source #'name)
  (begin
    (define (name/who who f.private ... ... . (~? rest ())) body ...)
    (define (name f.public ... ... . (~? rest ()))
      (~? (apply name/who 'name f.pass ... ... rest)
          (name/who 'name f.pass ... ...)))))
