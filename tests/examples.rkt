#lang racket/base

(module+ test
  (require racket/list
           racket/match
           racket/runtime-path
           rackunit)

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

  (test-case "examples are numbered 00, 01, ... without gaps"
    (check-equal? (map example-number example-names)
                  (range (length example-names))))

  (test-case "every example is a program with a main submodule, and has its own test"
    (check-not-equal? example-names '())
    (for ([name (in-list example-names)])
      (check-true (module-declared? (main-submodule name) #t) name)
      (check-true (file-exists? (build-path example-tests-dir name)) name))))
