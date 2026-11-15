#lang racket/base

;; The committed artifacts in examples/contracts/out/ against their sources.

(module+ test
  (require racket/file
           racket/list
           racket/string
           rackunit
           (only-in keccak keccak256)
           web3
           "../examples/common.rkt"
           "helpers/solc.rkt")

  (define sources (fixture-sources))

  (define (metadata name)
    (hash-ref (artifact name) 'metadata))

  (define (json-type parameter)
    (define type (hash-ref parameter 'type))
    (cond
      [(regexp-match #px"^tuple(.*)$" type)
       => (lambda (m)
            (format "(~a)~a"
                    (string-join (map json-type (hash-ref parameter 'components)) ",")
                    (second m)))]
      [else type]))

  (test-case "the fixtures are Token, Counter and Events"
    (check-equal? (map source-name sources) '("Counter" "Events" "Token")))

  (test-case "each artifact was compiled from the committed source"
    (for ([source (in-list sources)])
      (define name (source-name source))
      (define recorded
        (hash-ref (hash-ref (hash-ref (metadata name) 'sources) (string->symbol (format "~a.sol" name)))
                  'keccak256))
      (check-equal? (bytes->hex-string (keccak256 (file->bytes source))) recorded
                    (format "~a.sol changed; run racket tests/generate-contracts.rkt" name))))

  (test-case "the artifacts target cancun with one compiler, and their selectors are web3's"
    (define names (map source-name sources))
    (for ([name (in-list names)])
      (define a (artifact name))
      (check-equal? (hash-ref (hash-ref (metadata name) 'settings) 'evmVersion) "cancun" name)
      (for ([key (in-list '(bytecode deployedBytecode))])
        (check-regexp-match #px"^0x(?:[0-9a-f]{2})+$" (hash-ref (hash-ref a key) 'object) name))
      (define identifiers (hash-ref a 'methodIdentifiers))
      (define functions
        (for/list ([item (in-list (hash-ref a 'abi))]
                   #:when (equal? (hash-ref item 'type) "function"))
          (format "~a(~a)" (hash-ref item 'name)
                  (string-join (map json-type (hash-ref item 'inputs)) ","))))
      (check-equal? (sort functions string<?) (sort (map symbol->string (hash-keys identifiers)) string<?)
                    name)
      (for ([f (in-list functions)])
        (check-equal? (substring (bytes->hex-string (function-selector f)) 2)
                      (hash-ref identifiers (string->symbol f))
                      f)))
    (define versions
      (for/list ([name (in-list names)])
        (hash-ref (hash-ref (metadata name) 'compiler) 'version)))
    (check-equal? (length (remove-duplicates versions)) 1))

  (define pinned (hash-ref (hash-ref (metadata "Token") 'compiler) 'version))
  (define found (solc-version))

  (cond
    [(not found)
     (displayln "skipping fixture recompile: solc not found (set WEB3_SOLC or use `nix develop`)")]
    [(not (equal? found pinned))
     (printf "skipping fixture recompile: solc ~a is not ~a\n" found pinned)]
    [else
     (test-case "recompiling each source gives its artifact"
       (for ([source (in-list sources)])
         (define name (source-name source))
         (check-equal? (compile-fixture source) (artifact name) name)))]))
