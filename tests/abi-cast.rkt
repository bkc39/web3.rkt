#lang racket/base

(module+ test
  (require racket/match
           racket/string
           rackunit
           web3/abi
           web3/abi/vectors
           "abi-gen.rkt"
           "cast.rkt"
           "util.rkt")

  (define (signature-of ts)
    (format "f(~a)" (string-join (map abi-type->string ts) ",")))

  ;; The types solc's abi.encodePacked accepts.
  (define (random-packed-type prng)
    (match (random 3 prng)
      [0 (random-abi-type prng #:max-depth 0)]
      [1 (abi-array (random-abi-type prng #:max-depth 0 #:static? #t))]
      [2 (abi-fixed-array (random-abi-type prng #:max-depth 0 #:static? #t) (add1 (random 3 prng)))]))

  (define (check-agrees mode ts vs)
    (define signature (signature-of ts))
    (check-equal? (encode-hex mode signature vs) (cast-encode mode signature vs)
                  (format "~a ~a ~s" mode signature vs)))

  (cond
    [(not cast-path)
     (displayln "skipping cast differential: cast not found")]
    [else
     (test-case "cast --packed still left-pads array elements"
       (check-equal? (run-cast "abi-encode" "--packed" "--" "f(int8[])" "[-1]")
                     (string-append "0x" (make-string 62 #\0) "ff"))
       (check-equal? (run-cast "abi-encode" "--packed" "--" "f(bytes2[])" "[0x6162]")
                     (string-append "0x" (make-string 60 #\0) "6162")))

     (test-case "committed vectors agree with cast"
       (for ([v (in-list abi-encode-vectors)])
         (check-equal? (cast-encode (abi-vector-mode v) (abi-vector-signature v) (abi-vector-values v))
                       (abi-vector-hex v)
                       (abi-vector-name v))))

     (test-case "standard encodings and call data of generated arguments agree with cast"
       (check-agrees 'encode '() '())
       (check-agrees 'call '() '())
       (define prng (vector->pseudo-random-generator (vector 11 12 13 14 15 16)))
       (for ([i (in-range 200)])
         (define ts (random-abi-types prng #:min-count 1))
         (define vs (for/list ([t (in-list ts)]) (random-abi-value prng t #:cast-safe? #t)))
         (check-agrees 'encode ts vs)
         (when (< i 50)
           (check-agrees 'call ts vs))))

     (test-case "packed encodings of generated arguments agree with cast"
       (define prng (vector->pseudo-random-generator (vector 21 22 23 24 25 26)))
       (for ([_ (in-range 100)])
         (define ts (for/list ([_ (in-range (add1 (random 4 prng)))]) (random-packed-type prng)))
         (define vs (for/list ([t (in-list ts)]) (random-abi-value prng t #:cast-safe? #t)))
         (check-agrees 'packed ts vs)))]))
