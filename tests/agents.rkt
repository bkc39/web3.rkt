#lang racket/base

(module+ test
  (require (only-in racket/file file->lines)
           (only-in racket/list append-map remove-duplicates)
           racket/match
           racket/runtime-path
           rackunit)

  (define-runtime-path agents-file "../AGENTS.md")
  (define-runtime-path tests-directory ".")
  (define package-root (simplify-path (build-path tests-directory 'up)))

  (define (exported-names mod)
    (for*/list ([holder (in-list (list mod `(submod ,mod internal) `(submod ,mod private)))]
                #:when (module-declared? holder #t)
                [phases (in-list (call-with-values (lambda () (module->exports holder)) list))]
                [phase (in-list phases)]
                [export (in-list (match phase [(cons _ exports) exports]))])
      (match export [(list name _) name])))

  (test-case "A1: the /who names AGENTS.md says a module's submodules hold are exported there"
    (for ([line (in-list (file->lines agents-file))])
      (match (regexp-match #px"^\\| `([^`]+\\.rkt)` \\|.*?submodule(.*)$" line)
        [#f (void)]
        [(list _ file text)
         (define names (exported-names `(lib ,(string-append "web3/" file))))
         (for ([name (in-list (regexp-match* #px"`([^` ]+/who)`" text
                                             #:match-select (match-lambda [(list _ name) name])))])
           (check-not-false (memq (string->symbol name) names) (format "~a: ~a" file name)))])))

  (define base-argument-contract-names '(address/c hash-argument/c))
  (define argument-contract-makers
    '(address-or/c argument/c argument-list/c fields/c one-or-more/c))
  (define container-makers '(box/c cons/c hash/c list/c listof non-empty-listof vectorof))
  (define arrow-makers '(-> ->* ->i))

  (define (argument-contract-form? form names)
    (match form
      [(? symbol?) (memq form names)]
      [(cons (? symbol? head) _) (memq head argument-contract-makers)]
      [_ #f]))

  (define (holds-argument-contract? form names)
    (match form
      [(list 'quote _) #f]
      [_ #:when (argument-contract-form? form names) #t]
      [(cons head tail) (or (holds-argument-contract? head names)
                            (holds-argument-contract? tail names))]
      [_ #f]))

  (define (not-flat? form names)
    (match form
      [(cons (? symbol? head) _) #:when (memq head arrow-makers) #t]
      [_ (holds-argument-contract? form names)]))

  ;; Racket's list and container contracts print a value of the wrong shape, and its or/c falls
  ;; back to its own message when more than one disjunct is not flat.
  (define (unsafe-form form names)
    (match form
      [(list 'quote _) #f]
      [(cons (? symbol? head) arguments)
       #:when (and (memq head container-makers) (holds-argument-contract? arguments names))
       form]
      [(cons 'or/c disjuncts)
       #:when (< 1 (length (filter (lambda (disjunct) (not-flat? disjunct names)) disjuncts)))
       form]
      [(cons head tail) (or (unsafe-form head names) (unsafe-form tail names))]
      [_ #f]))

  ;; A name bound by define to a form that holds an argument contract is one.
  (define (defined-names form names)
    (match form
      [(list 'quote _) '()]
      [(list 'define (? symbol? name) body)
       #:when (holds-argument-contract? body names)
       (cons name (defined-names body names))]
      [(cons head tail) (append (defined-names head names) (defined-names tail names))]
      [_ '()]))

  (define (argument-contract-names forms)
    (let grow ([names base-argument-contract-names])
      (define more
        (remove-duplicates (append names (append-map (lambda (form) (defined-names form names))
                                                     forms))))
      (if (= (length more) (length names)) names (grow more))))

  (define (source-forms path)
    (parameterize ([read-accept-reader #t]
                   [read-accept-lang #t])
      (call-with-input-file path read)))

  (define (last-name path)
    (let-values ([(_directory name _dir?) (split-path path)])
      (path->string name)))

  (define (library-directory? directory)
    (not (regexp-match? #rx"^(?:[.].*|compiled|tests)$" (last-name directory))))

  (define sources
    (for/list ([path (in-directory package-root library-directory?)]
               #:when (regexp-match? #rx"[.]rkt$" (last-name path)))
      path))

  (define names (argument-contract-names (map source-forms sources)))

  (test-case "A2: no library module puts an argument contract in a container or a two-way or/c"
    (check-true (< 20 (length sources)))
    (check-not-false (member (build-path package-root "private" "argument.rkt") sources))
    (for ([path (in-list sources)])
      (check-false (unsafe-form (source-forms path) names) (path->string path))))

  (test-case "A2: the scan knows the argument contracts the library defines"
    (for ([name (in-list '(address-filter/c filter-object/c from/c sender/c target/c to/c
                           topic-positions/c transaction-object/c))])
      (check-not-false (memq name names) (symbol->string name)))
    (for ([name (in-list '(access-list/c amount/c errors/c provider/c))])
      (check-false (memq name names) (symbol->string name))))

  (test-case "A2: the scan finds what it looks for"
    (for ([form (in-list '((listof address/c)
                           (define x (non-empty-listof hash-argument/c))
                           (-> (or/c address/c hash-argument/c) any)
                           (or/c #f address/c (one-or-more/c address/c "addresses"))
                           (and/c (listof (or/c #f (one-or-more/c hash-argument/c "x"))) list?)
                           (listof (or/c #f address/c))
                           (list/c address/c)
                           (vectorof address/c)
                           (cons/c string? to/c)
                           (box/c sender/c)
                           (hash/c symbol? from/c)
                           (listof transaction-object/c)
                           (or/c transaction-object/c filter-object/c)
                           (or/c address/c (-> any))
                           (or/c (->* () any) address-filter/c)
                           (listof topic-positions/c)
                           (or/c target/c (->i () any))))])
      (check-not-false (unsafe-form form names) (format "~s" form)))
    (for ([form (in-list '((or/c #f address/c)
                           (or/c 'null (address-or/c 'x "y" "z"))
                           (listof string?)
                           (or/c #f (-> any))
                           (listof (cons/c address? bytes?))
                           (quote (listof address/c))
                           (-> (listof string?) address/c any)))])
      (check-false (unsafe-form form names) (format "~s" form))))

  (test-case "A2: a name defined from an argument contract is one"
    (check-equal? (argument-contract-names '((define a/c (or/c #f b/c))
                                             (define b/c (argument/c 'b "a b" b? b-reason))
                                             (define c/c (listof string?))))
                  (append base-argument-contract-names '(b/c a/c)))))
