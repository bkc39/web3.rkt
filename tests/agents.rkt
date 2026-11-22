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
           (check-not-false (memq (string->symbol name) names) (format "~a: ~a" file name)))])))

  (define argument-contract-names '(address/c hash-argument/c))
  (define argument-contract-makers
    '(address-or/c argument/c argument-list/c fields/c one-or-more/c))

  (define (argument-contract-form? form)
    (match form
      [(? symbol?) (memq form argument-contract-names)]
      [(cons (? symbol? head) _) (memq head argument-contract-makers)]
      [_ #f]))

  ;; Racket's listof prints a value that is not a list, and its or/c falls back to its own message
  ;; when more than one disjunct is not flat.
  (define (unsafe-form form)
    (match form
      [(list (or 'listof 'non-empty-listof) (? argument-contract-form?)) form]
      [(cons 'or/c disjuncts)
       #:when (< 1 (length (filter argument-contract-form? disjuncts)))
       form]
      [(? pair?) (or (unsafe-form (car form)) (unsafe-form (cdr form)))]
      [_ #f]))

  (define (source-forms path)
    (parameterize ([read-accept-reader #t]
                   [read-accept-lang #t])
      (call-with-input-file path read)))

  (define (last-name path)
    (let-values ([(_directory name _dir?) (split-path path)])
      (path->string name)))

  (define (library-directory? directory)
    (not (regexp-match? #rx"^(?:[.].*|compiled|tests)$" (last-name directory))))

  (define package-root
    (let-values ([(directory _name _dir?) (split-path (collection-file-path "main.rkt" "web3"))])
      directory))

  (test-case "A2: no library module puts an argument contract under listof or a two-way or/c"
    (define sources
      (for/list ([path (in-directory package-root library-directory?)]
                 #:when (regexp-match? #rx"[.]rkt$" (last-name path)))
        path))
    (check-true (< 20 (length sources)))
    (for ([path (in-list sources)])
      (check-false (unsafe-form (source-forms path)) (path->string path))))

  (test-case "A2: the scan finds what it looks for"
    (for ([form (in-list '((listof address/c)
                           (define x (non-empty-listof hash-argument/c))
                           (-> (or/c address/c hash-argument/c) any)
                           (or/c #f address/c (one-or-more/c address/c "addresses"))))])
      (check-not-false (unsafe-form form) (format "~s" form)))
    (for ([form (in-list '((or/c #f address/c) (or/c 'null (address-or/c 'x "y" "z"))
                           (listof string?)))])
      (check-false (unsafe-form form) (format "~s" form)))))
