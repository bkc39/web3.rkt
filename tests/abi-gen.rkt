#lang racket/base

(require racket/list
         racket/match
         (only-in web3/abi abi-encode word->bytes)
         (only-in web3/rpc bytes->hex-string)
         web3/abi/description
         web3/abi/event-type
         web3/abi/type
         "util.rkt")

(provide noisy-jsexpr
         random-abi-constructor
         random-abi-error
         random-abi-event
         random-abi-function
         random-abi-item
         random-abi-parameter
         random-contract-abi
         random-abi-type
         random-abi-types
         random-abi-value
         random-address
         random-bytes
         random-data
         random-decode-inputs
         random-word)


(define (random-elementary prng static?)
  (match (random (if static? 5 7) prng)
    [0 (abi-uint (pick prng widths))]
    [1 (abi-int (pick prng widths))]
    [2 (abi-address)]
    [3 (abi-bool)]
    [4 (abi-fixed-bytes (add1 (random 32 prng)))]
    [5 (abi-bytes)]
    [6 (abi-string)]))

(define (random-abi-type prng #:max-depth [max-depth 3] #:static? [static? #f])
  (define (child)
    (random-abi-type prng #:max-depth (sub1 max-depth) #:static? static?))
  (match (if (zero? max-depth) 0 (random (if static? 3 4) prng))
    [0 (random-elementary prng static?)]
    [1 (abi-fixed-array (child) (add1 (random 3 prng)))]
    [2 (abi-tuple (for/list ([_ (in-range (add1 (random 3 prng)))]) (child)))]
    [3 (abi-array (child))]))

(define (random-abi-types prng #:min-count [min-count 0] #:max-count [max-count 4])
  (for/list ([_ (in-range (+ min-count (random (add1 (- max-count min-count)) prng)))])
    (random-abi-type prng)))

(define cast-safe-alphabet
  (append (string->list "abcdefghijklmnopqrstuvwxyz0123456789") (list #\é #\€ #\😀)))

(define (random-char prng cast-safe?)
  (cond
    [cast-safe? (pick prng cast-safe-alphabet)]
    [else
     (define n (random #x10F800 prng))
     (integer->char (if (< n #xD800) n (+ n #x800)))]))

(define (random-integer prng lo hi)
  (match (random 6 prng)
    [0 lo]
    [1 hi]
    [2 (max lo (min hi 0))]
    [3 (max lo (min hi 1))]
    [4 (max lo (min hi -1))]
    [5 (+ lo (random-natural prng (- hi lo)))]))

(define (random-natural prng n)
  (modulo (for/fold ([acc 0]) ([_ (in-range (add1 (quotient (integer-length n) 24)))])
            (+ (* acc 16777216) (random 16777216 prng)))
          (add1 n)))

(define (random-bytes prng n)
  (apply bytes (for/list ([_ (in-range n)]) (random 256 prng))))

(define (random-address prng)
  (bytes->hex-string (random-bytes prng 20)))

(define (random-abi-value prng type #:max-length [max-length 3] #:cast-safe? [cast-safe? #f])
  (let loop ([t (abi-type type)])
    (match t
      [(struct* abi-uint ([bits n])) (random-integer prng 0 (sub1 (expt 2 n)))]
      [(struct* abi-int ([bits n])) (random-integer prng (- (expt 2 (sub1 n))) (sub1 (expt 2 (sub1 n))))]
      [(struct* abi-address ())
       (list->string
        (append (list #\0 #\x)
                (for/list ([_ (in-range 40)]) (pick prng (string->list "0123456789abcdefABCDEF")))))]
      [(struct* abi-bool ()) (zero? (random 2 prng))]
      [(struct* abi-fixed-bytes ([size n])) (random-bytes prng n)]
      [(struct* abi-bytes ()) (random-bytes prng (random 71 prng))]
      [(struct* abi-string ())
       (list->string (for/list ([_ (in-range (random 41 prng))]) (random-char prng cast-safe?)))]
      [(struct* abi-array ([element e]))
       (for/list ([_ (in-range (random (add1 max-length) prng))]) (loop e))]
      [(struct* abi-fixed-array ([element e] [length k])) (for/list ([_ (in-range k)]) (loop e))]
      [(struct* abi-tuple ([components cs])) (map loop cs)])))

(define identifier-start (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
(define identifier-rest (append identifier-start (string->list "0123456789_")))

(define (random-identifier prng)
  (list->string (cons (pick prng identifier-start)
                      (for/list ([_ (in-range (random 9 prng))]) (pick prng identifier-rest)))))

(define parameter-names
  '("a" "b" "c" "d" "e" "from" "to" "value" "owner" "spender" "id" "memo" "amount" "x_1" "Y"
    "payableTo" "indexedBy" "anonymousId" "eventId"))

;; k distinct elements of xs, in their order in xs.
(define (random-subset prng xs k)
  (let loop ([xs xs] [k k] [n (length xs)])
    (cond
      [(zero? k) (map (lambda (_) #f) xs)]
      [(< (random n prng) k) (cons #t (loop (cdr xs) (sub1 k) (sub1 n)))]
      [else (cons #f (loop (cdr xs) k (sub1 n)))])))

(define (random-abi-event prng)
  (define n (random 6 prng))
  (define anonymous? (zero? (random 5 prng)))
  (define types (for/list ([_ (in-range n)]) (random-abi-type prng #:max-depth 2)))
  (define names
    (for/fold ([names '()] #:result (reverse names))
              ([_ (in-range n)])
      (define free (filter (lambda (s) (not (member s names))) parameter-names))
      (cons (and (positive? (random 4 prng)) (pick prng free)) names)))
  (define indexed (random-subset prng types (random (add1 (min n (if anonymous? 4 3))) prng)))
  (abi-event (random-identifier prng)
             (map abi-event-parameter names types indexed)
             anonymous?))

(define name-starts '("get" "set" "balance" "safe" "transfer" "mint" "x" "on" "v2" "a" "uint256"))
(define name-pieces '("Of" "URI" "For" "Token" "ERC20" "_init" "X" "2" "Max" "_" "Received" "b2"))

(define (random-function-name prng)
  (define pieces
    (cons (pick prng name-starts)
          (for/list ([_ (in-range (random 3 prng))]) (pick prng name-pieces))))
  (define s (apply string-append pieces))
  (substring s 0 (min 11 (string-length s))))

(define (random-error-name prng)
  (string-append (pick prng '("Insufficient" "Not" "Bad" "Unauthorized" "E")) (random-identifier prng)))

(define (random-type prng tuples?)
  (define t (random-abi-type prng #:max-depth 2))
  (if (or tuples? (not (regexp-match? #rx"[(]" (abi-type->string t))))
      t
      (random-type prng tuples?)))

(define (random-abi-parameter prng #:named? [named? #t] #:taken [taken '()] #:tuples? [tuples? #t])
  (define free (filter (lambda (s) (not (member s taken))) parameter-names))
  (abi-parameter (and named? (pair? free) (positive? (random 4 prng)) (pick prng free))
                 (random-type prng tuples?)))

(define (random-parameters prng n taken tuples?)
  (for/fold ([ps '()] [taken taken] #:result (reverse ps))
            ([_ (in-range n)])
    (define p (random-abi-parameter prng #:taken taken #:tuples? tuples?))
    (values (cons p ps) (if (abi-parameter-name p) (cons (abi-parameter-name p) taken) taken))))

(define (parameter-names-of ps)
  (filter values (map abi-parameter-name ps)))

(define (random-abi-function prng
                             #:name [name (random-function-name prng)]
                             #:arity [arity (random 4 prng)]
                             #:tuples? [tuples? #t])
  (define inputs (random-parameters prng arity '() tuples?))
  (define outputs (random-parameters prng (random 4 prng) (parameter-names-of inputs) tuples?))
  (abi-function name inputs outputs (pick prng '(pure view nonpayable payable))))

(define (random-abi-error prng #:tuples? [tuples? #t])
  (abi-error (random-error-name prng) (random-parameters prng (random 4 prng) '() tuples?)))

(define (random-abi-constructor prng #:tuples? [tuples? #t])
  (abi-constructor (random-parameters prng (random 4 prng) '() tuples?)
                   (pick prng '(nonpayable payable))))

(define (random-abi-item prng)
  (match (random 6 prng)
    [0 (random-abi-function prng)]
    [1 (random-abi-event prng)]
    [2 (random-abi-error prng)]
    [3 (random-abi-constructor prng)]
    [4 (abi-fallback (pick prng '(nonpayable payable)))]
    [5 (abi-receive)]))

(define (random-shuffle prng xs)
  (map cdr (sort (map (lambda (x) (cons (random prng) x)) xs) < #:key car)))

(define (distinct key items)
  (for/fold ([seen '()] [kept '()] #:result (reverse kept))
            ([item (in-list items)])
    (define k (key item))
    (if (member k seen)
        (values seen kept)
        (values (cons k seen) (cons item kept)))))

(define (signature-key item)
  (abi-signature->string (if (abi-event? item) (abi-event->signature item) item)))

(define (random-contract-abi prng #:cast-safe? [cast-safe? #f] #:same-arity? [same-arity? #f])
  (define tuples? (not cast-safe?))
  (define functions
    (for/fold ([fs '()] #:result (reverse fs))
              ([_ (in-range (random 9 prng))])
      (define roll (random 100 prng))
      (define other (and (pair? fs) (pick prng fs)))
      (define taken
        (for/list ([g (in-list fs)]
                   #:when (equal? (abi-signature-name g) (and other (abi-signature-name other))))
          (length (abi-function-parameters g))))
      (define free (if other (filter (lambda (n) (not (member n taken))) (range 5)) '()))
      (define f
        (cond
          [(and (pair? free) (< roll 20))
           (random-abi-function prng #:name (abi-signature-name other) #:arity (pick prng free)
                                #:tuples? tuples?)]
          [(and same-arity? other (< roll 25))
           (random-abi-function prng #:name (abi-signature-name other)
                                #:arity (length (abi-function-parameters other))
                                #:tuples? tuples?)]
          [else (random-abi-function prng #:tuples? tuples?)]))
      (cons f fs)))
  (define events
    (for/list ([_ (in-range (random 4 prng))]
               #:when #t
               [e (in-value (random-abi-event prng))]
               #:unless (and cast-safe? (regexp-match? #rx"[(]" (abi-event->string e) 7)))
      e))
  (define errors (for/list ([_ (in-range (random 4 prng))]) (random-abi-error prng #:tuples? tuples?)))
  (define (maybe make)
    (if (zero? (random 2 prng)) '() (list (make))))
  (define singles
    (append (maybe (lambda () (random-abi-constructor prng #:tuples? tuples?)))
            (maybe (lambda () (abi-fallback (pick prng '(nonpayable payable)))))
            (maybe abi-receive)))
  (contract-abi (random-shuffle prng (append (distinct signature-key functions)
                                             (distinct signature-key events)
                                             (distinct signature-key errors)
                                             singles))))

(define (legacy-mutability prng item)
  (define mutability (hash-ref item 'stateMutability #f))
  (define bare (hash-remove item 'stateMutability))
  (match mutability
    ["view" (hash-set bare 'constant #t)]
    ["payable" (hash-set bare 'payable #t)]
    ["nonpayable" (if (zero? (random 2 prng)) bare (hash-set* bare 'payable #f 'constant #f))]
    [_ item]))

(define (noisy-parameter prng p event?)
  (define base (hash-set* p 'internalType (hash-ref p 'type) 'x-unknown 1))
  (define components (hash-ref p 'components #f))
  (define with-components
    (if components
        (hash-set base 'components (map (lambda (c) (noisy-parameter prng c #f)) components))
        base))
  (if (and event? (not (hash-ref p 'indexed)) (zero? (random 2 prng)))
      (hash-remove with-components 'indexed)
      with-components))

(define (noisy-jsexpr prng j)
  (for/list ([item (in-list j)])
    (define type (hash-ref item 'type))
    (define event? (equal? type "event"))
    (define params
      (for/fold ([item item]) ([key (in-list '(inputs outputs))]
                               #:when (hash-has-key? item key))
        (hash-set item key (map (lambda (p) (noisy-parameter prng p event?)) (hash-ref item key)))))
    (define legacy (if (zero? (random 2 prng)) (legacy-mutability prng params) params))
    (define untyped (if (and (equal? type "function") (zero? (random 2 prng))) (hash-remove legacy 'type) legacy))
    (define trimmed
      (cond
        [(and (equal? (hash-ref untyped 'outputs #f) '()) (zero? (random 2 prng))) (hash-remove untyped 'outputs)]
        [(and event? (not (hash-ref untyped 'anonymous))) (hash-remove untyped 'anonymous)]
        [else untyped]))
    (hash-set* trimmed 'gas 30000 'x-note "ignored")))

;; Words a decoder treats specially, for data of length n.
(define (interesting-words n)
  (filter (lambda (v) (<= 0 v (sub1 (expt 2 256))))
          (list 0 1 31 32 33 64 (- n 32) n (+ n 32)
                (sub1 (expt 2 64)) (expt 2 64) (expt 2 255) (sub1 (expt 2 256)))))

(define (random-word prng n)
  (if (zero? (random 2 prng))
      (word->bytes (pick prng (interesting-words n)))
      (random-bytes prng 32)))

(define (random-data prng)
  (define n (random 513 prng))
  (define words (for/list ([_ (in-range (quotient (+ n 31) 32))]) (random-word prng n)))
  (subbytes (apply bytes-append #"" words) 0 n))

(define (word-count data)
  (quotient (bytes-length data) 32))

(define (mutate prng kind data)
  (define n (bytes-length data))
  (define out (bytes-copy data))
  (define (word-at i) (subbytes data (* 32 i) (* 32 (add1 i))))
  (define words (word-count data))
  (cond
    [(eq? kind 'append) (bytes-append data (random-bytes prng (add1 (random 64 prng))))]
    [(zero? n) out]
    [(eq? kind 'flip)
     (define i (random n prng))
     (bytes-set! out i (bitwise-xor (bytes-ref out i) (arithmetic-shift 1 (random 8 prng))))
     out]
    [(eq? kind 'truncate) (subbytes data 0 (random n prng))]
    [(and (eq? kind 'overwrite) (positive? words))
     (bytes-copy! out (* 32 (random words prng)) (random-word prng n))
     out]
    [(and (eq? kind 'swap) (positive? words))
     (define i (random words prng))
     (define j (random words prng))
     (bytes-copy! out (* 32 i) (word-at j))
     (bytes-copy! out (* 32 j) (word-at i))
     out]
    [else out]))

;; (list kind types data original): random data for random types (kind
;; 'random, original #f), and each mutation of valid encodings (original
;; is the encoding mutated).
(define (random-decode-inputs prng #:random [random-count 400] #:encodings [encoding-count 400])
  (append
   (for/list ([_ (in-range random-count)])
     (list 'random (random-abi-types prng) (random-data prng) #f))
   (for*/list ([_ (in-range encoding-count)]
               [ts (in-value (random-abi-types prng))]
               [data (in-value (abi-encode ts (for/list ([t (in-list ts)]) (random-abi-value prng t))))]
               [kind (in-list '(flip truncate overwrite swap append))])
     (list kind ts (mutate prng kind data) data))))

(module+ test
  (require rackunit
           "util.rkt")

  (test-case "random-abi-types draws between min-count and max-count types"
    (define prng (seeded 3 1 4 1 5 9))
    (define counts
      (for/list ([_ (in-range 200)])
        (length (random-abi-types prng #:min-count 1 #:max-count 3))))
    (check-equal? (sort (remove-duplicates counts) <) '(1 2 3))))
