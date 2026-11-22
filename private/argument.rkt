#lang racket/base

(require (only-in racket/contract
                  blame-add-context
                  blame-context
                  blame-contract
                  blame-positive
                  blame-value
                  build-chaperone-contract-property
                  build-compound-type-name
                  contract-first-order
                  current-blame-format
                  get/build-late-neg-projection
                  make-chaperone-contract
                  prop:chaperone-contract
                  raise-blame-error)
         (only-in racket/match == match match-define)
         (only-in racket/string string-join string-prefix? string-trim)
         (only-in setup/path-to-relative path->relative-string/library)
         (only-in "../address.rkt" address?)
         (only-in "hex.rkt" address-hex-string? hash-hex-pattern hash-hex-string?)
         (only-in "signature.rkt" signature?)
         (only-in "signer.rkt" signer?))

(provide address/c
         describe-value
         hash-argument/c
         one-or-more/c)

(define (describe-value v)
  (cond
    [(signature? v) "a signature"]
    [(signer? v) "a signer"]
    [(string? v)
     (format "a string of ~a characters~a"
             (string-length v)
             (if (equal? v (string-trim v)) "" ", with leading or trailing whitespace"))]
    [(bytes? v) (format "a byte string of length ~a" (bytes-length v))]
    [(exact-integer? v) "an exact integer"]
    [(boolean? v) "a boolean"]
    [(symbol? v) "a symbol"]
    [else "a value of another kind"]))

;; Addresses and hashes are public, so their refusals show the value: one line saying why, then
;; where, what and whom, never the whole contract.  A string that could hold a secret key, 64 hex
;; digits in a row, is described instead, and so is every value that is not a string.
(struct public-contract (name noun accepts? reason)
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name (lambda (c) (public-contract-name c))
   #:first-order (lambda (c) (public-contract-accepts? c))
   #:late-neg-projection
   (lambda (c)
     (match-define (public-contract _ noun accepts? reason) c)
     (lambda (blame)
       (lambda (v neg)
         (if (accepts? v)
             v
             (parameterize ([current-blame-format refusal-format])
               (raise-blame-error blame #:missing-party neg v "not ~a: ~a" noun (reason v)))))))))

(define (shown v)
  (if (and (string? v) (not (regexp-match? #px"[0-9a-fA-F]{64}" v)))
      (format "~s" v)
      (describe-value v)))

(define (party-text party)
  (if (path? party) (path->relative-string/library party) (format "~a" party)))

;; ->i names an argument by its variable; the keyword it is bound to is in the contract's name.
(define (argument-label argument contract)
  (define named (string->symbol argument))
  (or (let search ([v contract])
        (match v
          [(list* (? keyword? kw) (cons (== named) _) _) kw]
          [(cons head tail) (or (search head) (search tail))]
          [_ #f]))
      argument))

(define (context-line context contract)
  (match context
    [(pregexp #px"^the (.+) argument of$" (list _ argument))
     (format "argument: ~a" (argument-label argument contract))]
    [(pregexp #px"^the (.+) field of$" (list _ field)) (format "field: ~a" field)]
    [_ (format "in: ~a" context)]))

(define (structural? context)
  (or (string-prefix? context "a part of the or/c of")
      (regexp-match? #px"^the [0-9]+(?:st|nd|rd|th) conjunct of$" context)))

(define (refusal-format blame v reason)
  (define where
    (for/list ([context (in-list (reverse (blame-context blame)))]
               #:unless (structural? context))
      (context-line context (blame-contract blame))))
  (define details
    (append where
            (list (format "given: ~a" (shown v))
                  (format "blaming: ~a" (party-text (blame-positive blame))))))
  (string-join (cons (format "~a: ~a" (or (blame-value blame) "contract violation") reason)
                     (map (lambda (line) (string-append "  " line)) details))
               "\n"))

(define (address-refusal v)
  (cond
    [(not (string? v)) "not a string"]
    [(not (address-hex-string? v)) "not 0x and 40 hex digits"]
    [else "mixed case that is not its EIP-55 checksum"]))

(define (hash-refusal v)
  (if (string? v) "not 0x and 64 hex digits" "not a string"))

(define address/c (public-contract 'address? "an address" address? address-refusal))
(define hash-argument/c
  (public-contract `(and/c string? ,hash-hex-pattern) "a hash" hash-hex-string? hash-refusal))

(define (one-or-more/c item/c)
  (define item? (contract-first-order item/c))
  (define project (get/build-late-neg-projection item/c))
  (make-chaperone-contract
   #:name (build-compound-type-name 'or/c item/c (build-compound-type-name 'non-empty-listof item/c))
   #:first-order (lambda (v) (or (item? v) (and (pair? v) (list? v) (andmap item? v))))
   #:late-neg-projection
   (lambda (blame)
     (define one (project blame))
     (define each (project (blame-add-context blame "an element of")))
     (lambda (v neg)
       (cond
         [(and (pair? v) (list? v))
          (for ([item (in-list v)])
            (each item neg))
          v]
         [else (one v neg)])))))
