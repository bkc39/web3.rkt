#lang racket/base

(require (only-in racket/contract
                  ->*
                  and/c
                  any/c
                  blame-add-context
                  blame-add-missing-party
                  blame-context
                  blame-contract
                  blame-positive
                  blame-value
                  build-chaperone-contract-property
                  build-compound-type-name
                  contract-first-order
                  contract-out
                  flat-contract?
                  get/build-late-neg-projection
                  make-exn:fail:contract:blame
                  non-empty-listof
                  or/c
                  prop:chaperone-contract)
         (only-in racket/match == match match-define)
         (only-in racket/string string-join string-prefix? string-trim)
         (only-in setup/path-to-relative path->relative-string/library)
         (only-in "../address.rkt" address?)
         (only-in "hex.rkt" address-hex-string? hash-hex-pattern hash-hex-string?)
         (only-in "signature.rkt" signature?)
         (only-in "signer.rkt" signer?))

(provide address/c
         address-or/c
         address-refusal
         argument/c
         argument-contract?
         (contract-out
          [argument-list/c
           (->* (any/c string? #:most exact-nonnegative-integer?)
                #:rest (and/c (non-empty-listof (or/c flat-contract? argument-contract?))
                              at-most-one-argument-contract?)
                argument-contract?)])
         describe-value
         hash-argument/c
         one-or-more/c
         ordinal
         raise-refusal
         register-describer!)

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
    [(null? v) "an empty list"]
    [(list? v) (format "a list of ~a element~a" (length v) (if (= (length v) 1) "" "s"))]
    [(hash? v) "a hash table"]
    [else "a value of another kind"]))

;; private/opaque.rkt registers the describer that knows secret and public keys, since this module
;; cannot require libsecp256k1.  A program that has not loaded it yet, such as one that requires
;; only web3/abi/event, sees such a key described as a value of another kind.
(define describer describe-value)

(define (register-describer! describe)
  (set! describer describe))

(define (ordinal i)
  (case i
    [(1) "1st"]
    [(2) "2nd"]
    [(3) "3rd"]
    [else (format "~ath" i)]))

;; Addresses and hashes are public, so a refusal shows the value when it is no more than an
;; address could be: 0x and at most 40 hex digits.  Anything else, which could be a secret key or
;; the text one was read from, is described.  The one short message says why, then where, what
;; and whom, never the whole contract.  opaque-> passes these contracts through as they are, and
;; no other contract that is not flat.
(struct argument-contract (name noun accepts? reason project)
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name (lambda (c) (argument-contract-name c))
   #:first-order (lambda (c) (argument-contract-accepts? c))
   #:late-neg-projection (lambda (c) (argument-contract-project c))))

(define (raise-refusal blame neg v noun reason)
  (define complete (blame-add-missing-party blame neg))
  (define message (refusal-message complete v (format "not ~a: ~a" noun reason)))
  (raise (make-exn:fail:contract:blame message (current-continuation-marks) complete)))

;; A #:project is a late-neg projection that refuses only through raise-refusal.
(define (argument/c name noun accepts? reason #:project [project #f])
  (define ((refusing blame) v neg)
    (if (accepts? v) v (raise-refusal blame neg v noun (reason v))))
  (argument-contract name noun accepts? reason (or project refusing)))

(define (shown v)
  (if (and (string? v) (regexp-match? #px"^0x[0-9a-fA-F]{0,40}$" v))
      (format "~s" v)
      (describer v)))

(define (party-text party)
  (if (path? party) (path->relative-string/library party) (format "~a" party)))

;; ->i names an argument by its variable; its position, or the keyword it is bound to, is in the
;; mandatory and optional lists of the ->i's own name.
(define (i-argument-label variable specs)
  (let search ([specs specs] [position 1])
    (match specs
      [(list* (? keyword? kw) (cons (== variable) _) _) kw]
      [(list* (? keyword?) _ more) (search more position)]
      [(cons (cons (== variable) _) _) (ordinal position)]
      [(cons _ more) (search more (add1 position))]
      [_ #f])))

(define (argument-label argument contract)
  (match contract
    [(list* '->i (? list? mandatory) more)
     (define optional (match more [(cons (? list? specs) _) specs] [_ '()]))
     (or (i-argument-label (string->symbol argument) (append mandatory optional)) argument)]
    [_ argument]))

(define (structural? context)
  (or (string-prefix? context "a part of the or/c of")
      (regexp-match? #px"^the [0-9]+(?:st|nd|rd|th) conjunct of$" context)))

;; Only the outermost argument is one of the top contract's own.
(define (where-lines contexts contract)
  (let loop ([contexts contexts] [outermost? #t])
    (match contexts
      ['() '()]
      [(cons (? structural?) more) (loop more outermost?)]
      [(cons (pregexp #px"^the (.+) argument of$" (list _ argument)) more)
       (cons (format "argument: ~a" (if outermost? (argument-label argument contract) argument))
             (loop more #f))]
      [(cons (pregexp #px"^the (.+) field of$" (list _ field)) more)
       (cons (format "field: ~a" field) (loop more outermost?))]
      [(cons context more) (cons (format "in: ~a" context) (loop more outermost?))])))

(define (refusal-message blame v reason)
  (define details
    (append (where-lines (reverse (blame-context blame)) (blame-contract blame))
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

(define address/c (argument/c 'address? "an address" address? address-refusal))

(define (address-or/c name other? other-noun)
  (argument/c name (string-append "an address or " other-noun)
              (lambda (v) (or (address? v) (other? v)))
              (lambda (v)
                (if (string? v)
                    (address-refusal v)
                    (string-append "neither a string nor " other-noun)))))
(define hash-argument/c
  (argument/c `(and/c string? ,hash-hex-pattern) "a hash" hash-hex-string? hash-refusal))

(define (one-or-more/c item/c noun)
  (match-define (argument-contract _ _ item? item-reason _) item/c)
  (define (accepts? v)
    (or (item? v) (and (pair? v) (list? v) (andmap item? v))))
  (define (reason v)
    (if (null? v) "the list is empty" (item-reason v)))
  (define (project blame)
    (define each ((get/build-late-neg-projection item/c) (blame-add-context blame "an element of")))
    (lambda (v neg)
      (cond
        [(and (pair? v) (list? v))
         (for ([item (in-list v)])
           (each item neg))
         v]
        [(item? v) v]
        [else (raise-refusal blame neg v noun (reason v))])))
  (define name
    (build-compound-type-name 'or/c item/c (build-compound-type-name 'non-empty-listof item/c)))
  (argument/c name noun accepts? reason #:project project))

(define (at-most-one-argument-contract? disjuncts)
  (<= (length (filter argument-contract? disjuncts)) 1))

(define (argument-list/c name noun #:most most . disjuncts)
  (define item/c (apply or/c disjuncts))
  (define item? (contract-first-order item/c))
  (define (fits? v)
    (and (list? v) (<= (length v) most)))
  (define (accepts? v)
    (and (fits? v) (andmap item? v)))
  (define (reason v)
    (if (list? v) (format "more than ~a elements" most) "not a list"))
  (define (project blame)
    (define each ((get/build-late-neg-projection item/c) (blame-add-context blame "an element of")))
    (lambda (v neg)
      (unless (fits? v)
        (raise-refusal blame neg v noun (reason v)))
      (for ([item (in-list v)])
        (each item neg))
      v))
  (argument/c name noun accepts? reason #:project project))
