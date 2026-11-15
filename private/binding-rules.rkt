#lang racket/base

;; What a bound procedure accepts, and the messages for what it refuses.
;; define-contract checks direct applications at expansion; the procedures
;; check the rest when they run.

(require (only-in racket/list remove-duplicates)
         (only-in racket/string string-join))

(provide all-keywords
         arity-message
         function-keywords
         deploy-keywords
         keyword-message)

(define all-keywords '(#:from #:value #:block #:gas #:errors #:provider))

(define (in-order keywords)
  (filter (lambda (kw) (memq kw keywords)) all-keywords))

(define (function-keywords mutability)
  (in-order (case mutability
              [(view pure) '(#:from #:block #:gas #:errors #:provider)]
              [(payable) '(#:from #:value #:gas #:errors #:provider)]
              [else '(#:from #:gas #:errors #:provider)])))

(define (deploy-keywords payable?)
  (in-order (if payable? '(#:from #:value #:gas #:errors #:provider) '(#:from #:gas #:errors #:provider))))

(define (arguments n)
  (format "~a argument~a" n (if (= n 1) "" "s")))

(define (or-list ns)
  (define strings (map number->string ns))
  (if (null? (cdr strings))
      (car strings)
      (format "~a or ~a" (string-join (reverse (cdr (reverse strings))) ", ") (car (reverse strings)))))

;; The message for given arguments when what takes one of arities; after
;; the address unless it is a deployment.
(define (arity-message what arities given #:deploy? [deploy? #f])
  (define sorted (sort (remove-duplicates arities) <))
  (define takes
    (if (null? (cdr sorted))
        (arguments (car sorted))
        (format "~a arguments" (or-list sorted))))
  (if deploy?
      (format "takes ~a; given ~a" takes given)
      (format "~a takes ~a after the address; given ~a" what takes given)))

;; The message for kw, which is not among allowed; what is the item and
;; how is its mutability or payability.
(define (keyword-message what how kw allowed)
  (if (memq kw all-keywords)
      (format "~a is ~a; ~a is not allowed" what how kw)
      (format "unknown keyword ~a; allowed: ~a" kw (string-join (map (lambda (k) (format "~a" k)) allowed) " "))))
