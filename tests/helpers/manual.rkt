#lang racket/base

(require (only-in racket/contract exn:fail:contract:blame?)
         (only-in racket/file file->string)
         racket/match
         rackunit
         (only-in scribble/example make-base-eval))

(provide check-manual-blocks
         check-manual-examples)

(define (check-manual-blocks blocks)
  (define ev (make-base-eval '(require web3 (only-in keccak keccak256))))
  (for* ([block (in-list blocks)]
         [entry (in-list block)])
    (match-define (list form expectation) entry)
    (check-form ev form expectation)))

(define (check-manual-examples evaluator expectations)
  (define blocks (manual-examples evaluator))
  (check-equal? (map length blocks) (map length expectations) "forms in each example")
  (when (equal? (map length blocks) (map length expectations))
    (check-manual-blocks (map (lambda (forms outcomes) (map list forms outcomes))
                              blocks expectations))))

(define (manual-examples evaluator)
  (define manual (file->string (collection-file-path "scribblings/web3.scrbl" "web3")))
  (define start (pregexp (string-append "@examples\\[#:eval " (regexp-quote evaluator) "\\s")))
  (for/list ([position (in-list (regexp-match-positions* start manual))])
    (example-forms (open-input-string (substring manual (cdr position))))))

(define (example-forms in)
  (regexp-match #px"^\\s*" in)
  (match (peek-char in)
    [#\] '()]
    [_ (cons (unwrap-error (read in)) (example-forms in))]))

(define (unwrap-error form)
  (match form
    [`(eval:error ,inner) inner]
    [_ form]))

(define (outcome ev form)
  (with-handlers ([exn:fail? values])
    (ev form)))

(define (check-form ev form expectation)
  (define name (format "~s" form))
  (define result (outcome ev form))
  (match expectation
    ['(void) (check-pred void? result name)]
    [`(prints ,text) (check-equal? (format "~a" result) text name)]
    [`(value ,v) (check-equal? result v name)]
    [`(raises ,message)
     (check-pred exn:fail? result name)
     (when (exn:fail? result)
       (check-equal? (exn-message result) message name))]
    [`(blames ,who)
     (check-pred exn:fail:contract:blame? result name)
     (when (exn:fail? result)
       (check-regexp-match
        (pregexp (format "^~a: contract violation" (regexp-quote (symbol->string who))))
        (exn-message result)
        name))]))
