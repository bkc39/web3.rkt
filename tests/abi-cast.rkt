#lang racket/base

;; Differential test of the encoders against Foundry's cast.  Skipped when
;; cast is not available.

(module+ test
  (require racket/match
           racket/string
           rackunit
           web3/abi
           web3/abi/vectors
           (only-in web3/rpc bytes->hex-string)
           "abi-gen.rkt"
           "cast.rkt")

  (define (signature-of ts)
    (format "f(~a)" (string-join (map abi-type->string ts) ",")))

  ;; The types solc's abi.encodePacked accepts.
  (define (random-packed-type prng)
    (match (random 3 prng)
      [0 (random-abi-type prng #:max-depth 0)]
      [1 (abi-array (random-abi-type prng #:max-depth 0 #:static? #t))]
      [2 (abi-fixed-array (random-abi-type prng #:max-depth 0 #:static? #t) (add1 (random 3 prng)))]))

  (cond
    [(not cast-path)
     (displayln "skipping cast differential: cast not found")]
    [else
     (test-case "D-a committed vectors agree with cast"
       (for ([v (in-list abi-encode-vectors)])
         (check-equal? (cast-encode (abi-vector-mode v) (abi-vector-signature v) (abi-vector-values v))
                       (abi-vector-hex v)
                       (abi-vector-name v))))

     (test-case "D-b generated standard encodings and call data"
       (define prng (vector->pseudo-random-generator (vector 11 12 13 14 15 16)))
       (for ([i (in-range 200)])
         (define ts (random-abi-types prng))
         (define vs (for/list ([t (in-list ts)]) (random-abi-value prng t #:cast-safe? #t)))
         (define signature (signature-of ts))
         (check-equal? (bytes->hex-string (abi-encode ts vs)) (cast-encode 'encode signature vs)
                       (format "~a ~s" signature vs))
         (when (< i 50)
           (check-equal? (bytes->hex-string (apply encode-call signature vs))
                         (cast-encode 'call signature vs)
                         (format "~a ~s" signature vs)))))

     (test-case "D-c generated packed encodings"
       (define prng (vector->pseudo-random-generator (vector 21 22 23 24 25 26)))
       (for ([_ (in-range 100)])
         (define ts (for/list ([_ (in-range (add1 (random 4 prng)))]) (random-packed-type prng)))
         (define vs (for/list ([t (in-list ts)]) (random-abi-value prng t #:cast-safe? #t)))
         (define signature (signature-of ts))
         (check-equal? (bytes->hex-string (abi-encode-packed ts vs)) (cast-encode 'packed signature vs)
                       (format "~a ~s" signature vs))))]))
