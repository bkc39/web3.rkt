#lang racket/base

(module+ test
  (require (only-in racket/file file->lines)
           racket/match
           rackunit)

  (define (exported-names mod)
    (for*/list ([holder (in-list (list mod `(submod ,mod internal) `(submod ,mod private)))]
                #:when (module-declared? holder #t)
                [phases (in-list (call-with-values (lambda () (module->exports holder)) list))]
                [phase (in-list phases)]
                [export (in-list (match phase [(cons _ exports) exports]))])
      (match export [(list name _) name])))

  (test-case "A1: the /who names AGENTS.md says a module's submodules hold are exported there"
    (for ([line (in-list (file->lines (collection-file-path "AGENTS.md" "web3")))])
      (match (regexp-match #px"^\\| `([^`]+\\.rkt)` \\|.*?submodule(.*)$" line)
        [#f (void)]
        [(list _ file text)
         (define names (exported-names `(lib ,(string-append "web3/" file))))
         (for ([name (in-list (regexp-match* #px"`([^` ]+/who)`" text
                                             #:match-select (match-lambda [(list _ name) name])))])
           (check-not-false (memq (string->symbol name) names) (format "~a: ~a" file name)))]))))
