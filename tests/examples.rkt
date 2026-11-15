#lang racket/base

;; Runs every numbered script in examples/ against its own anvil, so the
;; examples keep working.  Skipped when anvil is not available.

(module+ test
  (require racket/list
           racket/port
           racket/runtime-path
           racket/string
           rackunit
           web3
           "../examples/common.rkt")

  (define-runtime-path examples-dir "../examples")

  (define (example-number name)
    (string->number (cadr (regexp-match #px"^([0-9]{2})-" name))))

  (define example-names
    (sort (for/list ([p (in-list (directory-list examples-dir))]
                     #:when (regexp-match? #px"^[0-9]{2}-.*[.]rkt$" (path->string p)))
            (path->string p))
          string<?))

  (define (run-example name)
    (define custodian (make-custodian))
    (dynamic-wind
      void
      (lambda ()
        (with-output-to-string
          (lambda ()
            (parameterize ([current-custodian custodian]
                           [current-provider #f])
              (dynamic-require (build-path examples-dir name) #f)))))
      (lambda () (custodian-shutdown-all custodian))))

  (test-case "examples are numbered 00, 01, ... without gaps"
    (check-equal? (map example-number example-names)
                  (range (length example-names))))

  (test-case "show indents the continuation lines of an expression"
    (define lines
      (string-split (with-output-to-string
                      (lambda ()
                        (show (string-length
                               (string-append "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                              "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                                              "cccccccccccccccccccccccccccccc")))))
                    "\n"))
    (check-regexp-match #rx"^> [(]string-length$" (first lines))
    (check-true (> (length lines) 2))
    (for ([line (in-list (drop-right (rest lines) 1))])
      (check-regexp-match #rx"^   " line))
    (check-equal? (last lines) "90"))

  (test-case "show-error prints the exception's name and message as comment lines"
    (define out
      (with-output-to-string
        (lambda ()
          (show-error (raise-user-error 'f "first\n  second")))))
    (check-equal? out "> (raise-user-error 'f \"first\\n  second\")\n; exn:fail:user\n; f: first\n;   second\n"))

  (test-case "show-error #:as binds the exception"
    (define out
      (with-output-to-string
        (lambda ()
          (show-error (error 'g "boom") #:as e)
          (check-equal? (exn-message e) "g: boom"))))
    (check-equal? out "> (error 'g \"boom\")\n; exn:fail\n; g: boom\n"))

  (test-case "show-error fails when nothing is raised"
    (check-exn #rx"^show-error: 1 raised no exception$"
               (lambda () (with-output-to-string (lambda () (show-error 1))))))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping examples: anvil not found (set WEB3_ANVIL or use `nix develop`)")]
    [else
     (for ([name (in-list example-names)])
       (test-case name
         (check-regexp-match #rx"^\n;;;; " (run-example name))))]))
