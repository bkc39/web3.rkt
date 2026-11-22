#lang racket/base

(require racket/contract/base
         (only-in racket/list make-list)
         (only-in racket/match match match* match-define struct*)
         (only-in racket/string string-join)
         "../private/exn.rkt"
         (only-in "../private/hex.rkt" address-hex-string?))

(provide
 (struct-out exn:fail:web3:abi)
 (struct-out exn:fail:web3:abi:type)
 (struct-out exn:fail:web3:abi:value)
 (struct-out exn:fail:web3:abi:decode)
 abi-signature-spec/c
 abi-type-spec/c
 (contract-out
  (struct abi-uint ([bits abi-int-bits/c]))
  (struct abi-int ([bits abi-int-bits/c]))
  (struct abi-address ())
  (struct abi-bool ())
  (struct abi-fixed-bytes ([size abi-fixed-bytes-size/c]))
  (struct abi-bytes ())
  (struct abi-string ())
  (struct abi-array ([element abi-type-spec/c]))
  (struct abi-fixed-array ([element abi-type-spec/c] [length abi-array-length/c]))
  (struct abi-tuple ([components (non-empty-listof abi-type-spec/c)]))
  (struct abi-signature ([name abi-identifier/c]
                         [inputs (listof abi-type-spec/c)]
                         [outputs (or/c #f (listof abi-type-spec/c))]))
  [abi-type? (-> any/c boolean?)]
  [abi-type (-> abi-type-spec/c abi-type?)]
  [string->abi-type (-> string? abi-type?)]
  [sexp->abi-type (-> (or/c symbol? pair?) abi-type?)]
  [abi-type->string (-> abi-type-spec/c string?)]
  [abi-type->sexp (-> abi-type-spec/c (or/c symbol? pair?))]
  [abi-type-dynamic? (-> abi-type-spec/c boolean?)]
  [abi-type-static-size (-> abi-type-spec/c (or/c #f exact-positive-integer?))]
  [abi-value? (-> abi-type-spec/c any/c boolean?)]
  [abi-value/c (-> abi-type-spec/c flat-contract?)]
  [string->abi-signature (-> string? abi-signature?)]
  [abi-signature->string (->* (abi-signature?) (#:outputs? boolean?) string?)]))

;;;; contracts

(define abi-type-spec/c
  (flat-named-contract 'abi-type-spec/c
                       (lambda (v) (or (abi-type? v) (string? v) (symbol? v) (pair? v)))))

(define abi-signature-spec/c
  (flat-named-contract 'abi-signature-spec/c (lambda (v) (or (string? v) (abi-signature? v)))))

(define abi-int-bits/c
  (flat-named-contract 'abi-int-bits/c
                       (lambda (v) (and (exact-integer? v) (<= 8 v 256) (zero? (remainder v 8))))))

(define abi-fixed-bytes-size/c
  (flat-named-contract 'abi-fixed-bytes-size/c
                       (lambda (v) (and (exact-integer? v) (<= 1 v 32)))))

(define abi-array-length/c (integer-in 1 (sub1 (expt 2 64))))

(define array-length? (flat-contract-predicate abi-array-length/c))

;; The end of the identifier that starts at index i of s, or #f.
(define (identifier-end s i)
  (match (regexp-match-positions #px"^[A-Za-z_$][A-Za-z0-9_$]*" s i)
    [(list (cons _ end)) end]
    [#f #f]))

(define abi-identifier/c
  (flat-named-contract 'abi-identifier/c
                       (lambda (v) (and (string? v) (eqv? (identifier-end v 0) (string-length v))))))

;;;; structs

(struct abi-uint (bits) #:transparent)
(struct abi-int (bits) #:transparent)
(struct abi-address () #:transparent)
(struct abi-bool () #:transparent)
(struct abi-fixed-bytes (size) #:transparent)
(struct abi-bytes () #:transparent)
(struct abi-string () #:transparent)
(struct abi-array (element) #:transparent
  #:guard (lambda (element who) (abi-type/who who element)))
(struct abi-fixed-array (element length) #:transparent
  #:guard (lambda (element k who) (values (abi-type/who who element) k)))
(struct abi-tuple (components) #:transparent
  #:guard (lambda (components who) (types/who who components)))
(struct abi-signature (name inputs outputs) #:transparent
  #:guard (lambda (name inputs outputs who)
            (values name (types/who who inputs) (and outputs (types/who who outputs)))))

(define (abi-type? v)
  (or (abi-uint? v) (abi-int? v) (abi-address? v) (abi-bool? v) (abi-fixed-bytes? v)
      (abi-bytes? v) (abi-string? v) (abi-array? v) (abi-fixed-array? v) (abi-tuple? v)))

;;;; errors

(define (raise-abi-type-error who reason input position [form #f])
  (define where
    (cond
      [position (format "\n  at: position ~a" position)]
      [form (format "\n  form: ~s" form)]
      [else ""]))
  (raise (exn:fail:web3:abi:type (format "~a: ~a: ~s~a" who reason input where)
                                 (current-continuation-marks)
                                 input
                                 position)))

(define (raise-abi-value-error who failure)
  (match-define (list reason type value path) failure)
  (raise (exn:fail:web3:abi:value
          (format "~a: ~a\n  type: ~a\n  value: ~e\n  at: ~a"
                  who reason (type-label type) value (path-label path))
          (current-continuation-marks)
          type
          value
          path)))

(define (type-label t)
  (if (list? t)
      (written (lambda (out) (write-types t out)))
      (type->string t)))

(define (path-label path)
  (match path
    ['() "the argument list"]
    [(cons i indices)
     (string-join (cons (format "argument ~a" i)
                        (map (lambda (j) (format "index ~a" j)) indices))
                  ", ")]))

;;;; reading types

(define (elementary token)
  (match token
    ["address" (abi-address)]
    ["bool" (abi-bool)]
    ["string" (abi-string)]
    ["bytes" (abi-bytes)]
    ["uint" (abi-uint 256)]
    ["int" (abi-int 256)]
    [(pregexp #px"^(u?)int([1-9][0-9]*)$" (list _ u (app string->number bits)))
     #:when (and (<= bits 256) (zero? (remainder bits 8)))
     (if (string=? u "u") (abi-uint bits) (abi-int bits))]
    [(pregexp #px"^bytes([1-9][0-9]*)$" (list _ (app string->number size)))
     #:when (<= size 32)
     (abi-fixed-bytes size)]
    [_ #f]))

(define (elementary-failure token)
  (cond
    [(string=? token "") "expected a type"]
    [(regexp-match? #px"^(?:function|u?fixed(?:[0-9]+x[0-9]+)?)$" token) "not supported"]
    [else "not an ABI type"]))

(define (skip-spaces s i)
  (if (and (< i (string-length s)) (char=? (string-ref s i) #\space))
      (skip-spaces s (add1 i))
      i))

;; Reads goal from start and returns it with the index just after it.
;; Positions are string indices; the string's length means the input
;; ended early.  With names?, each tuple component may be followed by
;; spaces and an identifier, which is dropped.
(define (read-abi-text who s goal start names?)
  (define n (string-length s))
  (define (fail reason position)
    (raise-abi-type-error who reason s position))
  (define (char-at i)
    (and (< i n) (string-ref s i)))
  (define (skip i)
    (skip-spaces s i))
  (define (read-type i)
    (define-values (base j) (read-base i))
    (read-suffixes base j))
  (define (read-base i)
    (define j (or (identifier-end s i) i))
    (define token (substring s i j))
    (define k (skip j))
    (cond
      [(eqv? (char-at i) #\() (read-tuple i)]
      [(and (string=? token "tuple") (eqv? (char-at k) #\()) (read-tuple k)]
      [(string=? token "tuple") (fail "expected (" k)]
      [(and (string=? token "address") (> k j) (regexp-match? #px"^payable(?![A-Za-z0-9_$])" s k))
       (fail "not an ABI type; write address" k)]
      [(elementary token) => (lambda (t) (values t j))]
      [else (fail (elementary-failure token) i)]))
  (define (read-suffixes t i)
    (define length-end
      (match (regexp-match-positions #px"^[1-9][0-9]*\\]" s (min (add1 i) n))
        [(list (cons _ end)) end]
        [#f #f]))
    (define k (and length-end (string->number (substring s (add1 i) (sub1 length-end)))))
    (cond
      [(not (eqv? (char-at i) #\[)) (values t i)]
      [(eqv? (char-at (add1 i)) #\]) (read-suffixes (abi-array t) (+ i 2))]
      [(and k (array-length? k)) (read-suffixes (abi-fixed-array t k) length-end)]
      [else (fail "expected an array length from 1 to 2^64-1" (add1 i))]))
  (define (read-tuple i)
    (define-values (ts j) (read-list i #f))
    (values (abi-tuple ts) j))
  (define (read-component i)
    (define-values (t j) (read-type i))
    (define k (skip j))
    (define name-end (and names? (> k j) (identifier-end s k)))
    (values t (if name-end (skip name-end) j)))
  ;; "(" T ("," T)* ")" from the "(" at i; "()" only when empty-ok?.
  (define (read-list i empty-ok?)
    (define start (skip (add1 i)))
    (cond
      [(and empty-ok? (eqv? (char-at start) #\))) (values '() (add1 start))]
      [else
       (let loop ([at start] [ts '()])
         (define-values (t j) (read-component at))
         (define k (skip j))
         (match (char-at k)
           [#\, (loop (skip (add1 k)) (cons t ts))]
           [#\) (values (reverse (cons t ts)) (add1 k))]
           [#f (fail "expected , or )" n)]
           [_ (fail "expected , or )" j)]))]))
  (define (read-signature i)
    (define name-end (identifier-end s i))
    (define open (and name-end (skip name-end)))
    (cond
      [(not name-end) (fail "expected a function name" i)]
      [(not (eqv? (char-at open) #\()) (fail "expected (" open)]
      [else
       (define-values (inputs inputs-end) (read-list open #t))
       (define outputs-start (skip inputs-end))
       (define-values (outputs end)
         (if (eqv? (char-at outputs-start) #\()
             (read-list outputs-start #t)
             (values #f inputs-end)))
       (values (abi-signature (substring s i name-end) inputs outputs) end)]))
  (match goal
    ['type (read-type start)]
    ['signature (read-signature start)]))

(define (read-whole who s goal)
  (define-values (result end) (read-abi-text who s goal (skip-spaces s 0) #f))
  (cond
    [(= (skip-spaces s end) (string-length s)) result]
    [else (raise-abi-type-error who "unexpected text" s end)]))

(define (read-abi-type s start #:component-names? [names? #f] #:who [who 'read-abi-type])
  (read-abi-text who s 'type start names?))

(define (string->type who s)
  (read-whole who s 'type))

(define (string->signature who s)
  (read-whole who s 'signature))

(define (sexp->type who v [input v])
  (let loop ([v v])
    (match v
      [(? symbol? (app symbol->string (app elementary (? abi-type? t)))) t]
      [(? symbol?) (raise-abi-type-error who (elementary-failure (symbol->string v)) input #f v)]
      [(list 'array t) (abi-array (loop t))]
      [(list 'array t (? array-length? k)) (abi-fixed-array (loop t) k)]
      [(list 'tuple ts ..1) (abi-tuple (map loop ts))]
      [_ (raise-abi-type-error who "not an ABI type" input #f v)])))

(define (abi-type/who who spec)
  (cond
    [(abi-type? spec) spec]
    [(string? spec) (string->type who spec)]
    [else (sexp->type who spec)]))

(define (types/who who specs)
  (map (lambda (spec) (abi-type/who who spec)) specs))

;;;; writing types

(define (write-type t out)
  (match t
    [(struct* abi-uint ([bits n])) (fprintf out "uint~a" n)]
    [(struct* abi-int ([bits n])) (fprintf out "int~a" n)]
    [(struct* abi-address ()) (write-string "address" out)]
    [(struct* abi-bool ()) (write-string "bool" out)]
    [(struct* abi-fixed-bytes ([size n])) (fprintf out "bytes~a" n)]
    [(struct* abi-bytes ()) (write-string "bytes" out)]
    [(struct* abi-string ()) (write-string "string" out)]
    [(struct* abi-array ([element e])) (write-type e out) (write-string "[]" out)]
    [(struct* abi-fixed-array ([element e] [length k])) (write-type e out) (fprintf out "[~a]" k)]
    [(struct* abi-tuple ([components cs])) (write-types cs out)]))

(define (write-types ts out)
  (write-string "(" out)
  (for ([t (in-list ts)] [i (in-naturals)])
    (unless (zero? i) (write-string "," out))
    (write-type t out))
  (write-string ")" out))

(define (written write)
  (define out (open-output-string))
  (write out)
  (get-output-string out))

(define (type->string t)
  (written (lambda (out) (write-type t out))))

(define (type->sexp t)
  (match t
    [(struct* abi-array ([element e])) `(array ,(type->sexp e))]
    [(struct* abi-fixed-array ([element e] [length k])) `(array ,(type->sexp e) ,k)]
    [(struct* abi-tuple ([components cs])) `(tuple ,@(map type->sexp cs))]
    [_ (string->symbol (type->string t))]))

(define (abi-signature->string sig #:outputs? [outputs? #f])
  (match-define (struct* abi-signature ([name name] [inputs inputs] [outputs outputs])) sig)
  (written (lambda (out)
             (write-string name out)
             (write-types inputs out)
             (when (and outputs? outputs)
               (write-types outputs out)))))

;;;; layout

;; Sizes of composite types are cached per node, so each node is measured
;; once however deep it is nested.
(define static-sizes (make-weak-hasheq))

(define (static-size t)
  (match t
    [(or (struct* abi-bytes ()) (struct* abi-string ()) (struct* abi-array ())) #f]
    [(or (struct* abi-fixed-array ()) (struct* abi-tuple ()))
     (hash-ref! static-sizes t (lambda () (composite-static-size t)))]
    [_ 32]))

(define (composite-static-size t)
  (match t
    [(struct* abi-fixed-array ([element e] [length k]))
     (define size (static-size e))
     (and size (* k size))]
    [(struct* abi-tuple ([components cs]))
     (define sizes (map static-size cs))
     (and (andmap values sizes) (apply + sizes))]))

(define (dynamic? t)
  (not (static-size t)))

;;;; values

(define (integer-failure v signed? bits fail)
  (define lo (if signed? (- (expt 2 (sub1 bits))) 0))
  (define hi (sub1 (expt 2 (if signed? (sub1 bits) bits))))
  (define range
    (cond
      [(<= bits 64) (format "from ~a to ~a" lo hi)]
      [signed? (format "from -2^~a to 2^~a-1" (sub1 bits) (sub1 bits))]
      [else (format "from 0 to 2^~a-1" bits)]))
  (cond
    [(not (exact-integer? v)) (fail "expected an exact integer")]
    [(<= lo v hi) #f]
    [else (fail (format "out of range: expected an integer ~a" range))]))

;; #f, or (list reason type value path) for the first part of v that does
;; not fit t.  rpath is the path to v, innermost index first.
(define (value-failure t v rpath #:address-ok? [address-ok? any-address?])
  (let failure ([t t] [v v] [rpath rpath])
    (define (fail reason)
      (list reason t v (reverse rpath)))
    (define (each ts)
      (for/or ([t (in-list ts)]
               [x (in-list v)]
               [i (in-naturals)])
        (failure t x (cons i rpath))))
    (match* (t v)
      [((struct* abi-uint ([bits n])) _) (integer-failure v #f n fail)]
      [((struct* abi-int ([bits n])) _) (integer-failure v #t n fail)]
      [((struct* abi-address ()) (? address-hex-string?))
       (and (not (address-ok? v)) (fail "mixed case that is not the address's EIP-55 checksum"))]
      [((struct* abi-address ()) _) (fail "expected a string of 0x and 40 hex digits")]
      [((struct* abi-bool ()) (? boolean?)) #f]
      [((struct* abi-bool ()) _) (fail "expected #t or #f")]
      [((struct* abi-fixed-bytes ([size n])) (? bytes?)) #:when (= (bytes-length v) n) #f]
      [((struct* abi-fixed-bytes ([size n])) _)
       (fail (format "expected a byte string of length ~a" n))]
      [((struct* abi-bytes ()) (? bytes?)) #f]
      [((struct* abi-bytes ()) _) (fail "expected a byte string")]
      [((struct* abi-string ()) (? string?)) #f]
      [((struct* abi-string ()) _) (fail "expected a string")]
      [((struct* abi-array ([element e])) (? list?)) (each (make-list (length v) e))]
      [((struct* abi-array ()) _) (fail "expected a list")]
      [((struct* abi-fixed-array ([element e] [length k])) (? list?))
       #:when (= (length v) k)
       (each (make-list k e))]
      [((struct* abi-fixed-array ([length k])) _) (fail (format "expected a list of ~a values" k))]
      [((struct* abi-tuple ([components cs])) (? list?)) #:when (= (length v) (length cs))
       (each cs)]
      [((struct* abi-tuple ([components cs])) _)
       (fail (format "expected a list of ~a values" (length cs)))])))

;; v, a value of t, folded from its elementary values up: (leaf type v) at
;; each elementary value, (node type results) at each array and tuple.
(define (fold-abi-value leaf node t v)
  (let loop ([t t] [v v])
    (match t
      [(or (struct* abi-array ([element e])) (struct* abi-fixed-array ([element e])))
       (node t (map (lambda (x) (loop e x)) v))]
      [(struct* abi-tuple ([components cs])) (node t (map loop cs v))]
      [_ (leaf t v)])))

(define (map-abi-value f t v)
  (fold-abi-value f (lambda (_ vs) vs) t v))

(define (any-address? _address)
  #t)

(define (arguments-failure ts vs #:address-ok? [address-ok? any-address?])
  (cond
    [(= (length ts) (length vs))
     (for/or ([t (in-list ts)]
              [v (in-list vs)]
              [i (in-naturals)])
       (value-failure t v (list i) #:address-ok? address-ok?))]
    [else (list (format "expected ~a values, given ~a" (length ts) (length vs)) ts vs '())]))

;;;; the public conversions

(define (abi-type spec)
  (abi-type/who 'abi-type spec))

(define (string->abi-type s)
  (string->type 'string->abi-type s))

(define (sexp->abi-type v)
  (sexp->type 'sexp->abi-type v))

(define (abi-type->string spec)
  (type->string (abi-type/who 'abi-type->string spec)))

(define (abi-type->sexp spec)
  (type->sexp (abi-type/who 'abi-type->sexp spec)))

(define (abi-type-dynamic? spec)
  (dynamic? (abi-type/who 'abi-type-dynamic? spec)))

(define (abi-type-static-size spec)
  (static-size (abi-type/who 'abi-type-static-size spec)))

(define (abi-value? spec v)
  (not (value-failure (abi-type/who 'abi-value? spec) v '())))

(define (abi-value/c spec)
  (define t (abi-type/who 'abi-value/c spec))
  (flat-named-contract `(abi-value/c ,(type->string t))
                       (lambda (v) (not (value-failure t v '())))))

(define (string->abi-signature s)
  (string->signature 'string->abi-signature s))

;; The readers and checks under their caller's name, for web3/abi.
(module* private #f
  (provide abi-identifier/c
           abi-type/who
           arguments-failure
           array-length?
           dynamic?
           elementary
           elementary-failure
           fold-abi-value
           identifier-end
           map-abi-value
           raise-abi-type-error
           raise-abi-value-error
           read-abi-type
           sexp->type
           skip-spaces
           static-size
           string->signature
           string->type
           type->sexp
           type->string
           type-label
           types/who))
