#lang racket/base

(module ambient racket/base
  (require racket/tcp
           web3)
  (module+ main
    (define listener (tcp-listen 0 4 #t "127.0.0.1"))
    (define-values (_host port _peer-host _peer-port) (tcp-addresses listener #t))
    (printf "~a\n~a\n~a\n" port (current-provider) (current-account))))

(module+ test
  (require racket/file
           racket/list
           racket/match
           racket/runtime-path
           (only-in racket/string string-split)
           (only-in racket/tcp tcp-connect)
           rackunit
           syntax/location
           (only-in web3 current-account current-provider mock-provider with-provider)
           (only-in "helpers/examples.rkt" example-lines module-output))

  (define-runtime-path examples-dir "../examples")
  (define-runtime-path example-tests-dir "examples")

  (define (example-number name)
    (match (regexp-match #px"^([0-9]{2})-" name)
      [(list _ digits) (string->number digits)]))

  (define example-names
    (sort (for/list ([p (in-list (directory-list examples-dir))]
                     #:when (regexp-match? #px"^[0-9]{2}-.*[.]rkt$" (path->string p)))
            (path->string p))
          string<?))

  (define (main-submodule name)
    `(submod (file ,(path->string (build-path examples-dir name))) main))

  (define (base-name name)
    (regexp-replace #rx"[.]rkt$" name ""))

  (test-case "examples are numbered 00, 01, ... without gaps"
    (check-not-equal? example-names '())
    (check-equal? (map example-number example-names)
                  (range (length example-names))))

  (test-case "every example is a program with a main submodule"
    (for ([name (in-list example-names)])
      (check-true (module-declared? (main-submodule name) #t) name)))

  (test-case "every example has its own test, which names it"
    (for ([name (in-list example-names)])
      (define test-file (build-path example-tests-dir name))
      (check-true (and (file-exists? test-file)
                       (regexp-match? (regexp-quote (format "\"~a\"" (base-name name)))
                                      (file->string test-file)))
                  name)))

  (test-case "an example's output is the same each time it runs"
    (define first-run (example-lines "04-abi-encode"))
    (check-not-equal? first-run '())
    (check-equal? (example-lines "04-abi-encode") first-run))

  (test-case "main runs with no provider or account, and what it opens is closed after"
    (match-define (list port provider account)
      (parameterize ([current-account "0x00000000000000000000000000000000000a11ce"])
        (with-provider (mock-provider (hash))
          (string-split (module-output (quote-module-path ".." ambient main)) "\n"))))
    (check-equal? (list provider account) '("#f" "#f"))
    (check-exn exn:fail:network?
               (lambda () (tcp-connect "127.0.0.1" (string->number port))))))
