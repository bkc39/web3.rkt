#lang racket/base

;; The item, JSON and s-expression readers on hostile input: each returns
;; a description or raises exn:fail:web3:abi:type, within time and memory.

(module+ test
  (require json
           racket/file
           racket/list
           racket/match
           rackunit
           web3
           "../examples/common.rkt"
           "abi-gen.rkt"
           "bounded.rkt"
           "util.rkt")

  (define seed '(6 6 6 1 2 3))
  (define prng (apply seeded seed))

  (define (pick xs)
    (list-ref xs (random (length xs) prng)))

  (define (check-total name input thunk ok?)
    (define label (format "seed ~s: ~a ~s" seed name input))
    (match (call-bounded thunk)
      [(list 'ok v) (check-true (ok? v) label)]
      [(list 'raised (? exn:fail:web3:abi:type?)) (void)]
      [outcome (fail (format "~a: ~s" label outcome))]))

  (define canonical-strings
    (for/list ([_ (in-range 60)]) (abi-item->string (random-abi-item prng))))

  (define keywords '("function" "event" "error" "constructor" "fallback" "receive" "returns" "view"
                     "payable" "pure" "external" "indexed" "anonymous" "memory" "tuple"))

  (define (mutate-string s)
    (define n (string-length s))
    (define i (if (zero? n) 0 (random (add1 n) prng)))
    (match (random 4 prng)
      [0 (if (< i n) (string-append (substring s 0 i) (substring s (add1 i))) s)]
      [1 (string-append (substring s 0 i) (string (pick (string->list "()[], az09"))) (substring s i))]
      [2 (regexp-replace (pregexp (pick keywords)) s (pick keywords))]
      [3 (substring s 0 i)]))

  (define (round-trips? i)
    (equal? (string->abi-item (abi-item->string i)) i))

  (test-case "string->abi-item gives an item or raises at a position within the input"
    (define inputs
      (append (for/list ([_ (in-range 1000)]) (mutate-string (pick canonical-strings)))
              (for/list ([_ (in-range 500)])
                (list->string (for/list ([_ (in-range (random 81 prng))])
                                (pick (string->list "()[], azfnuit0189_\t;event function")))))))
    (for ([s (in-list inputs)])
      (match (call-bounded (lambda () (string->abi-item s)))
        [(list 'ok i) (check-true (round-trips? i) s)]
        [(list 'raised (? exn:fail:web3:abi:type? e))
         (check-true (<= (exn:fail:web3:abi:type-position e) (string-length s)) s)]
        [outcome (fail (format "seed ~s: ~s: ~s" seed s outcome))])))

  (define (random-jsexpr depth)
    (match (if (zero? depth) (random 4 prng) (random 6 prng))
      [0 (random 100 prng)]
      [1 (pick '("" "function" "uint256" "tuple" "x" "view"))]
      [2 (pick '(#t #f null))]
      [3 '()]
      [4 (for/list ([_ (in-range (random 4 prng))]) (random-jsexpr (sub1 depth)))]
      [5 (for/hasheq ([_ (in-range (random 4 prng))])
           (values (pick '(type name inputs outputs components abi bytecode stateMutability indexed))
                   (random-jsexpr (sub1 depth))))]))

  (define (mutate-jsexpr j)
    (define replacement (pick (list 42 'null "" '() (hasheq))))
    (let loop ([j j])
      (match j
        [(? list?) #:when (and (pair? j) (zero? (random 3 prng)))
         (define i (random (length j) prng))
         (append (take j i) (list (list-ref j i)) (drop j i))]
        [(? list?) (map loop j)]
        [(? hash?) #:when (and (positive? (hash-count j)) (zero? (random 3 prng)))
         (define k (pick (hash-keys j)))
         (match (random 3 prng)
           [0 (hash-remove j k)]
           [1 (hash-set (hash-remove j k) (pick '(nam typ input x)) (hash-ref j k))]
           [2 (hash-set j k replacement)])]
        [(? hash?) (for/hasheq ([(k v) (in-hash j)]) (values k (loop v)))]
        [_ j])))

  (test-case "jsexpr->contract-abi gives a description or raises a type error"
    (define inputs
      (append (for/list ([_ (in-range 500)])
                (mutate-jsexpr (contract-abi->jsexpr (random-contract-abi prng))))
              (for/list ([_ (in-range 200)]) (random-jsexpr 4))))
    (for ([j (in-list inputs)])
      (check-total "jsexpr" j (lambda () (jsexpr->contract-abi j)) contract-abi?)))

  (test-case "read-contract-abi gives a description or raises a type error"
    (define fixture-bytes
      (for/list ([name (in-list '("Token" "Counter" "Events"))]) (file->bytes (artifact-path name))))
    (define inputs
      (append (for/list ([_ (in-range 300)]) (random-bytes prng (random 2001 prng)))
              (for/list ([_ (in-range 100)])
                (define bs (pick fixture-bytes))
                (subbytes bs 0 (random (bytes-length bs) prng)))
              (list (make-bytes 100000 (char->integer #\[)))))
    (for ([bs (in-list inputs)])
      (check-total "bytes" (bytes-length bs) (lambda () (read-contract-abi (open-input-bytes bs)))
                   contract-abi?)))

  (define atoms '(function event error constructor uint256 address a #:view #:returns #f))

  (define (random-datum depth)
    (if (or (zero? depth) (zero? (random 3 prng)))
        (pick atoms)
        (for/list ([_ (in-range (random 5 prng))]) (random-datum (sub1 depth)))))

  (test-case "sexp->abi-item gives an item or raises a type error"
    (for ([_ (in-range 500)])
      (define x (cons (pick atoms) (for/list ([_ (in-range (random 4 prng))]) (random-datum 3))))
      (check-total "sexp" x (lambda () (sexp->abi-item x)) abi-item?))))
