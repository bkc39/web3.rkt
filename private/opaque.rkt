#lang racket/base

;; Deliberately copied from libsecp256k1's private contracts (bkc39/rkt-libsecp256k1#12),
;; with required keyword arguments added.

(require (for-syntax racket/base
                     syntax/parse)
         (only-in libsecp256k1 public-key? secret-key?)
         (only-in racket/contract
                  blame-add-context
                  blame-swap
                  build-compound-type-name
                  build-flat-contract-property
                  coerce-flat-contract
                  contract-name
                  flat-contract-predicate
                  get/build-late-neg-projection
                  make-contract
                  prop:flat-contract
                  raise-blame-error)
         (only-in racket/dict in-dict)
         (only-in racket/list append-map filter-map)
         (only-in racket/match match-define match-lambda)
         (only-in racket/string string-join string-trim)
         syntax/parse/define
         (only-in "../address.rkt" address?)
         (only-in "hex.rkt" address-hex-string?)
         (only-in "signature.rkt" signature?))

(provide address/c
         describe-kind
         digest/c
         message/c
         opaque->
         (rename-out [opaque-contract opaque/c])
         secret-key/c)

(define (miscased-address? v)
  (and (address-hex-string? v) (not (address? v))))

(define web3-kinds
  (list (cons signature? "a signature")
        (cons miscased-address? "an address whose mixed case is not its EIP-55 checksum")))

(define (describe-kind v)
  (or (for/first ([(kind? text) (in-dict web3-kinds)] #:when (kind? v)) text)
      (describe-common-kind v)))

(define (describe-common-kind v)
  (cond
    [(string? v)
     (format "a string of ~a characters~a"
             (string-length v)
             (if (equal? v (string-trim v)) "" ", with leading or trailing whitespace"))]
    [(bytes? v) (format "a byte string of length ~a" (bytes-length v))]
    [(exact-integer? v) "an exact integer"]
    [(secret-key? v) "a secret key"]
    [(public-key? v) "a public key"]
    [(boolean? v) "a boolean"]
    [(symbol? v) "a symbol"]
    [else "a value of another kind"]))

(struct opaque-contract (name expected accepts?)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name (lambda (c) (opaque-contract-name c))
   #:first-order (lambda (c) (opaque-contract-accepts? c))
   #:late-neg-projection
   (lambda (c)
     (define accepts? (opaque-contract-accepts? c))
     (lambda (blame)
       (lambda (v neg)
         (if (accepts? v)
             v
             (raise-blame-error blame #:missing-party neg v
                                '(expected: "~a" given: "~a")
                                (opaque-contract-expected c) (describe-kind v))))))))

(define secret-key/c (opaque-contract 'secret-key? "a secret key" secret-key?))
(define digest/c
  (opaque-contract 'digest/c "a 32-byte digest"
                   (lambda (v) (and (bytes? v) (= (bytes-length v) 32)))))
(define message/c
  (opaque-contract 'message/c "a byte string or a string" (lambda (v) (or (bytes? v) (string? v)))))
(define address/c
  (opaque-contract 'address/c "an address" address?))

(define (opaque v)
  (if (opaque-contract? v)
      v
      (let ([c (coerce-flat-contract 'opaque-> v)])
        (opaque-contract (contract-name c) (format "~a" (contract-name c))
                         (flat-contract-predicate c)))))

(define (count-text n)
  (if (= n 1) "1 argument" (format "~a arguments" n)))

(define (arity-text arity)
  (cond
    [(exact-integer? arity) (count-text arity)]
    [(arity-at-least? arity) (format "at least ~a" (count-text (arity-at-least-value arity)))]
    [else (string-join (map arity-text arity) " or ")]))

(define (keywords-text kws)
  (if (null? kws)
      "no keyword arguments"
      (string-append (if (null? (cdr kws)) "keyword " "keywords ")
                     (string-join (map (lambda (k) (format "~a" k)) kws) " or "))))

(define (ordinal i)
  (case i
    [(1) "1st"]
    [(2) "2nd"]
    [(3) "3rd"]
    [else (format "~ath" i)]))

(define (arity-includes? arity n)
  (if (arity-at-least? arity) (>= n (arity-at-least-value arity)) (= n arity)))

(define (keyword-contract-name k)
  (match-define (list kw c required?) k)
  (define named (list kw (contract-name c)))
  (if required? named (list named)))

(define (make-opaque-arrow doms rest kws rng)
  (define opaque-doms (map opaque doms))
  (define opaque-rest (and rest (opaque rest)))
  (define opaque-kws
    (for/list ([k (in-list kws)])
      (match-define (list kw c required?) k)
      (list kw (opaque c) required?)))
  (define required
    (filter-map (match-lambda [(list kw _ required?) (and required? kw)]) opaque-kws))
  (define opaque-rng (opaque rng))
  (define arity (if rest (arity-at-least (length doms)) (length doms)))
  (make-contract
   #:name (apply build-compound-type-name 'opaque->
                 (append (map contract-name opaque-doms)
                         (if rest (list '#:rest (contract-name opaque-rest)) '())
                         (append-map keyword-contract-name opaque-kws)
                         (list (contract-name opaque-rng))))
   #:first-order (lambda (f) (and (procedure? f) (procedure-arity-includes? f (length doms))))
   #:late-neg-projection
   (lambda (blame)
     (define (argument-projection c label)
       ((get/build-late-neg-projection c) (blame-add-context blame label #:swap? #t)))
     (define dom-projections
       (for/list ([d (in-list opaque-doms)]
                  [i (in-naturals 1)])
         (argument-projection d (format "the ~a argument of" (ordinal i)))))
     (define rest-projection
       (and rest (argument-projection opaque-rest "the rest argument of")))
     (define kw-projections
       (for/list ([k (in-list opaque-kws)])
         (match-define (list kw c _) k)
         (cons kw (argument-projection c (format "the ~a argument of" kw)))))
     (define rng-projection
       ((get/build-late-neg-projection opaque-rng) (blame-add-context blame "the range of")))
     (lambda (f neg)
       (define (refuse expected given)
         (raise-blame-error (blame-swap blame) #:missing-party neg f
                            '(expected: "~a" given: "~a") expected given))
       (define (project-positional args)
         (let loop ([args args] [ps dom-projections])
           (cond
             [(null? args) '()]
             [(null? ps) (for/list ([a (in-list args)]) (rest-projection a neg))]
             [else (cons ((car ps) (car args) neg) (loop (cdr args) (cdr ps)))])))
       (define (call kws kw-args . args)
         (define unknown (for/first ([k (in-list kws)] #:unless (assq k kw-projections)) k))
         (define missing (for/first ([k (in-list required)] #:unless (memq k kws)) k))
         (cond
           [unknown (refuse (keywords-text (map car kw-projections)) (format "keyword ~a" unknown))]
           [missing (refuse (format "keyword ~a" missing) (format "no keyword ~a" missing))]
           [(not (arity-includes? arity (length args)))
            (refuse (arity-text (procedure-arity f)) (count-text (length args)))]
           [else
            (rng-projection
             (keyword-apply f
                            kws
                            (for/list ([k (in-list kws)] [v (in-list kw-args)])
                              ((cdr (assq k kw-projections)) v neg))
                            (project-positional args))
             neg)]))
       (procedure-rename (make-keyword-procedure call (lambda args (apply call '() '() args)))
                         (object-name f))))))

(begin-for-syntax
  (define-syntax-class positional
    #:description "an argument contract"
    (pattern (~and e:expr (~not (_:keyword . _)) (~not _:keyword))))

  (define-splicing-syntax-class keyword-argument
    #:description "a keyword argument: #:keyword contract, or [#:keyword contract] if optional"
    (pattern [kw:keyword c:expr] #:with required? #'#f)
    (pattern (~seq kw:keyword c:expr)
             #:fail-when (and (eq? (syntax-e #'kw) '#:rest) #'kw) "#:rest comes before keywords"
             #:with required? #'#t)))

(define-syntax-parser opaque->
  [(_ dom:positional ... (~optional (~seq #:rest rest:expr)) kw:keyword-argument ... rng:expr)
   #'(make-opaque-arrow (list dom ...)
                        (~? rest #f)
                        (list (list 'kw.kw kw.c kw.required?) ...)
                        rng)])
