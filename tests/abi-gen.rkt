#lang racket/base

(require racket/list
         racket/match
         (only-in web3/abi abi-encode word->bytes)
         web3/abi/type
         "util.rkt")

(provide random-abi-type
         random-abi-types
         random-abi-value
         random-bytes
         random-decode-inputs)

(define (pick prng xs)
  (list-ref xs (random (length xs) prng)))

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
