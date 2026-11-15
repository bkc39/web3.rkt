#lang racket/base

;; Shared by the examples.  `show` prints an expression and then its value,
;; so running an example reads like a REPL transcript.

(require racket/pretty
         racket/string
         syntax/parse/define)

(provide show
         section)

(define (section title)
  (printf "\n;;;; ~a\n\n" title))

(define (echo datum)
  (define lines (string-split (pretty-format datum (- (pretty-print-columns) 2) #:mode 'write) "\n"))
  (printf "> ~a\n" (string-join lines "\n  ")))

(define-syntax-parse-rule (show e:expr)
  (begin
    (echo 'e)
    (call-with-values
     (lambda () e)
     (lambda vs
       (for ([v (in-list vs)]
             #:unless (void? v))
         (pretty-print v))))))
