#lang racket/base

;; Shared by the examples.  `show` prints an expression and then its value,
;; so running an example reads like a REPL transcript.

(require racket/pretty
         syntax/parse/define)

(provide show
         section)

(define (section title)
  (printf "\n;;;; ~a\n\n" title))

(define-syntax-parse-rule (show e:expr)
  (begin
    (printf "> ")
    (pretty-write 'e)
    (call-with-values
     (lambda () e)
     (lambda vs
       (for ([v (in-list vs)]
             #:unless (void? v))
         (pretty-print v))))))
