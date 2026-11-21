#lang racket/base

(require racket/match
         rackunit
         (only-in scribble/example make-base-eval))

(provide check-manual-blocks)

(define (check-manual-blocks blocks)
  (define ev (make-base-eval '(require web3 (only-in keccak keccak256))))
  (for* ([block (in-list blocks)]
         [entry (in-list block)])
    (match-define (list form expectation) entry)
    (check-form ev form expectation)))

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
       (check-equal? (exn-message result) message name))]))
