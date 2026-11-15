#lang racket/base

;; Runs code that may hang or allocate without bound.

(provide call-bounded)

(define memory-bound? (custodian-memory-accounting-available?))

(unless memory-bound?
  (displayln "memory bound unavailable"))

;; (list 'ok v), (list 'raised e), 'timeout or 'memory.
(define (call-bounded thunk #:seconds [seconds 2] #:megabytes [megabytes 64])
  (define custodian (make-custodian))
  (when memory-bound?
    (custodian-limit-memory custodian (* megabytes 1024 1024) custodian))
  (define result (box #f))
  (define worker
    (parameterize ([current-custodian custodian])
      (thread (lambda ()
                (set-box! result
                          (with-handlers ([(lambda (_) #t) (lambda (e) (list 'raised e))])
                            (list 'ok (thunk))))))))
  (define finished (sync/timeout seconds worker))
  (custodian-shutdown-all custodian)
  (cond
    [(unbox result)]
    [finished 'memory]
    [else 'timeout]))

(module+ test
  (require rackunit)

  (test-case "call-bounded reports results, exceptions, timeouts and memory"
    (check-equal? (call-bounded (lambda () 1)) '(ok 1))
    (check-match (call-bounded (lambda () (error 'x "y"))) (list 'raised (? exn:fail?)))
    (check-equal? (call-bounded (lambda () (let loop () (loop))) #:seconds 0.2) 'timeout)
    (when memory-bound?
      (check-equal? (call-bounded (lambda () (let loop ([xs '()]) (loop (cons (make-vector 1024) xs))))
                                  #:seconds 30 #:megabytes 16)
                    'memory))))
