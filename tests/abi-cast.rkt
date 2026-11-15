#lang racket/base

(module+ test
  (require racket/match
           rackunit
           web3/abi
           web3/abi/vectors
           web3/address
           (only-in web3/rpc bytes->hex-string hex-string->bytes)
           "abi-gen.rkt"
           "cast.rkt"
           "revert-cases.rkt"
           "util.rkt")

  ;; The types solc's abi.encodePacked accepts.
  (define (random-packed-type prng)
    (match (random 3 prng)
      [0 (random-abi-type prng #:max-depth 0)]
      [1 (abi-array (random-abi-type prng #:max-depth 0 #:static? #t))]
      [2 (abi-fixed-array (random-abi-type prng #:max-depth 0 #:static? #t) (add1 (random 3 prng)))]))

  (define (result-signature ts)
    (signature-string "f" '() ts))

  (define (check-decodes-as-cast ts data [name ""])
    (check-equal? (cast-decode 'result (result-signature ts) (bytes->hex-string data))
                  (map value->cast-json ts (abi-decode ts data))
                  name))

  (define (check-agrees mode ts vs)
    (define signature (signature-string "f" ts))
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
       (define prng (seeded 11 12 13 14 15 16))
       (for ([i (in-range 200)])
         (define ts (random-abi-types prng #:min-count 1))
         (define vs (for/list ([t (in-list ts)]) (random-abi-value prng t #:cast-safe? #t)))
         (check-agrees 'encode ts vs)
         (when (< i 50)
           (check-agrees 'call ts vs))))

     (test-case "packed encodings of generated arguments agree with cast"
       (define prng (seeded 21 22 23 24 25 26))
       (for ([_ (in-range 100)])
         (define ts (for/list ([_ (in-range (add1 (random 4 prng)))]) (random-packed-type prng)))
         (define vs (for/list ([t (in-list ts)]) (random-abi-value prng t #:cast-safe? #t)))
         (check-agrees 'packed ts vs)))

     (test-case "decoded values agree with cast"
       (define prng (seeded 31 32 33 34 35 36))
       (for ([i (in-range 200)])
         (define ts (random-abi-types prng #:min-count 1))
         (define vs (for/list ([t (in-list ts)]) (random-abi-value prng t #:cast-safe? #t)))
         (check-decodes-as-cast ts (abi-encode ts vs) (format "~s" vs))
         (when (< i 50)
           (define signature (signature-string "f" ts))
           (define data (apply encode-call signature vs))
           (check-equal? (cast-decode 'call signature (bytes->hex-string data))
                         (map value->cast-json ts (decode-call signature data))
                         signature))))

     (test-case "whatever web3 decodes, cast decodes the same way"
       (define prng (seeded 1 2 3 4 5 6))
       (for ([input (in-list (random-decode-inputs prng))])
         (match-define (list kind ts data _) input)
         (define accepted? (not (decode-error (lambda () (abi-decode ts data)))))
         (when (and accepted? (pair? ts))
           (check-decodes-as-cast ts data (format "~a ~a" kind (bytes->hex-string data)))))
       (for ([v (in-list abi-decode-vectors)]
             #:when (pair? (abi-vector-values v)))
         (define inputs (signature-inputs (abi-vector-signature v)))
         (check-decodes-as-cast inputs (hex-string->bytes (abi-vector-hex v)) (abi-vector-name v))))

     (test-case "checksums agree with cast"
       (define prng (seeded 41 42 43 44 45 46))
       (for ([_ (in-range 50)])
         (define a (random-abi-value prng 'address))
         (check-equal? (run-cast "to-check-sum-address" "--" a) (address->checksum-string a) a)))

     (test-case "revert arguments agree with cast"
       (for ([c (in-list reverts)])
         (match-define (struct* revert-case ([payload payload] [errors errors] [name name]
                                             [arguments arguments] [display display]))
           c)
         (define signature
           (match* (name errors)
             [("Error" _) "Error(string)"]
             [(_ (list sig)) sig]
             [(_ _) #f]))
         (when (and signature (pair? arguments))
           (check-equal? (cast-decode 'call signature (bytes->hex-string payload))
                         (map value->cast-json (signature-inputs signature) arguments)
                         display))))]))
