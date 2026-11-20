#lang racket/base

;; Runs every numbered script in examples/ against its own anvil, so the
;; examples keep working.  Skipped when anvil is not available.  A program,
;; an example with a main submodule, is run by its own test in
;; tests/examples/ instead.

(module+ test
  (require racket/list
           racket/port
           racket/runtime-path
           racket/string
           rackunit
           web3
           "../examples/common.rkt")

  (define-runtime-path examples-dir "../examples")
  (define-runtime-path example-tests-dir "examples")

  (define (example-number name)
    (string->number (cadr (regexp-match #px"^([0-9]{2})-" name))))

  (define example-names
    (sort (for/list ([p (in-list (directory-list examples-dir))]
                     #:when (regexp-match? #px"^[0-9]{2}-.*[.]rkt$" (path->string p)))
            (path->string p))
          string<?))

  (define (program? name)
    (module-declared? `(submod (file ,(path->string (build-path examples-dir name))) main) #t))

  (define-values (program-names script-names) (partition program? example-names))

  (define (run-example name)
    (define custodian (make-custodian))
    (dynamic-wind
      void
      (lambda ()
        (with-output-to-string
          (lambda ()
            (parameterize ([current-custodian custodian]
                           [current-provider #f]
                           [current-account #f])
              (dynamic-require (build-path examples-dir name) #f)))))
      (lambda () (custodian-shutdown-all custodian))))

  (test-case "examples are numbered 00, 01, ... without gaps"
    (check-equal? (map example-number example-names)
                  (range (length example-names))))

  (test-case "every program has its own test"
    (check-not-equal? program-names '())
    (for ([name (in-list program-names)])
      (check-true (file-exists? (build-path example-tests-dir name)) name)))

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

  (test-case "show-error replaces each run of control characters in a line with an ellipsis"
    (define out
      (with-output-to-string
        (lambda ()
          (show-error (error 'h "a\0\1b\tc\r\nd\e")))))
    (check-equal? out "> (error 'h \"a\\u0000\\u0001b\\tc\\r\\nd\\e\")\n; exn:fail\n; h: a…b…c…\n; d…\n"))

  (test-case "show-error drops lines of control characters and a trailing colon before them"
    (define out
      (with-output-to-string
        (lambda ()
          (show-error (error 'eth_call "execution reverted: custom error 0xcf479181: \0\0\1\n\0\5")))))
    (check-equal? (member "; exn:fail" (string-split out "\n" #:trim? #f))
                  '("; exn:fail" "; eth_call: execution reverted: custom error 0xcf479181" "")))

  (test-case "show-error shows a syntax error its expression raises when it expands"
    (define out
      (with-output-to-string
        (lambda ()
          (show-error (let ())))))
    (check-equal? (take (string-split out "\n") 3)
                  '("> (let ())" "; exn:fail:syntax" "; let: bad syntax (missing binding pairs or body)")))

  (test-case "show-error fails when nothing is raised"
    (check-exn #rx"^show-error: 1 raised no exception$"
               (lambda () (with-output-to-string (lambda () (show-error 1))))))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping examples: anvil not found (set WEB3_ANVIL or use `nix develop`)")]
    [else
     (for ([name (in-list script-names)])
       (test-case name
         (check-regexp-match #rx"^\n;;;; " (run-example name))))]))
