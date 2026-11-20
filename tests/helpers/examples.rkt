#lang racket/base

(require (only-in racket/port with-output-to-string)
         (only-in racket/string string-split))

(provide example-lines
         example-output)

(define (example-output name)
  (define main `(submod ,(string->symbol (format "web3/examples/~a" name)) main))
  (with-output-to-string (lambda () (dynamic-require main #f))))

(define (example-lines name)
  (string-split (example-output name) "\n"))
