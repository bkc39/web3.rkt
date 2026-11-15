#lang racket/base

(require (only-in racket/list remove-duplicates)
         (only-in racket/match match)
         (only-in racket/string string-join))

(provide all-keywords
         arity-message
         count-of
         deploy-keywords
         function-keywords
         keyword-message
         or-list)

(define all-keywords '(#:from #:value #:block #:gas #:errors #:provider))

(define (in-order keywords)
  (filter (lambda (kw) (memq kw keywords)) all-keywords))

(define (function-keywords mutability)
  (in-order (case mutability
              [(view pure) '(#:from #:block #:gas #:errors #:provider)]
              [(payable) '(#:from #:value #:gas #:errors #:provider)]
              [else '(#:from #:gas #:errors #:provider)])))

(define (deploy-keywords payable?)
  (in-order (if payable?
                '(#:from #:value #:gas #:errors #:provider)
                '(#:from #:gas #:errors #:provider))))

(define (count-of n what)
  (format "~a ~a~a" n what (if (= n 1) "" "s")))

(define (or-list vs)
  (match (map (lambda (v) (format "~a" v)) vs)
    [(list s) s]
    [(list ss ... s) (format "~a or ~a" (string-join ss ", ") s)]))

(define (arity-message what arities given #:deploy? [deploy? #f])
  (define sorted (sort (remove-duplicates arities) <))
  (define takes
    (if (null? (cdr sorted))
        (count-of (car sorted) "argument")
        (format "~a arguments" (or-list sorted))))
  (cond
    [deploy? (format "takes ~a; given ~a" takes given)]
    [given (format "~a takes ~a after the address; given ~a" what takes given)]
    [else (format "the address is missing; ~a takes it, then ~a" what takes)]))

(define (keyword-message what how kw allowed)
  (if (memq kw all-keywords)
      (format "~a is ~a; ~a is not allowed" what how kw)
      (format "unknown keyword ~a; allowed: ~a"
              kw
              (string-join (map (lambda (k) (format "~a" k)) allowed) " "))))
