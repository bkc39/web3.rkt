#lang racket/base

(require racket/file
         syntax/parse/define)

(provide call-with-temporary-directory
         with-temporary-directory)

(define (call-with-temporary-directory proc)
  (define dir (make-temporary-directory))
  (dynamic-wind void
                (lambda () (proc dir))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

(define-syntax-parse-rule (with-temporary-directory (dir:id) body:expr ...+)
  (call-with-temporary-directory (lambda (dir) body ...)))
