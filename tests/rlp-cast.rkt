#lang racket/base

(module+ test
  (require racket/list
           racket/match
           rackunit
           web3/rlp
           (only-in web3/private/hex natural->bytes)
           (only-in web3/rpc bytes->hex-string hex-string->bytes)
           (only-in "abi-gen.rkt" random-bytes)
           "cast.rkt"
           (only-in "helpers/accounts.rkt" random-natural)
           "helpers/rlp.rkt"
           (only-in "util.rkt" seeded))

  (define (sample prng n make)
    (for/list ([_ (in-range n)]) (make prng)))

  (define (random-encoding prng)
    (let loop ()
      (define e (rlp-encode (random-rlp-item prng #:depth 2 #:width 3)))
      (if (<= 2 (bytes-length e) 56) e (loop))))

  (define (long-prefix prng)
    (define base (if (zero? (random 2 prng)) #xb7 #xf7))
    (define k (+ 2 (random 7 prng)))
    (bytes-append (bytes (+ base k)) (natural->bytes 56 k)))

  (define (strictness-samples prng)
    (append
     (sample prng 10 (lambda (prng) (bytes #x81 (random 256 prng))))
     (sample prng 10 (lambda (prng)
                       (define n (add1 (random 55 prng)))
                       (bytes-append (bytes (if (zero? (random 2 prng)) #xb8 #xf8) n)
                                     (make-bytes n 1))))
     (sample prng 10 long-prefix)
     (sample prng 10 (lambda (prng)
                       (define e (random-encoding prng))
                       (subbytes e 0 (random (bytes-length e) prng))))
     (sample prng 10 (lambda (prng)
                       (define e (random-encoding prng))
                       (bytes-append (bytes (+ #xc0 (sub1 (bytes-length e)))) e)))
     (sample prng 10 (lambda (prng)
                       (bytes-append (random-encoding prng)
                                     (random-bytes prng (add1 (random 8 prng))))))))

  (define (cast-phrase stderr)
    (match (regexp-match #px"\n- ([^\n]+)" stderr)
      [(list _ phrase) phrase]
      [#f stderr]))

  (define (fault-phrase fault)
    (match (regexp-match #px"^rlp-[a-z>-]+: ([^:]+):" (exn-message fault))
      [(list _ phrase) phrase]
      [#f (exn-message fault)]))

  (cond
    [(not cast-path) (displayln "SKIPPED: rlp differential: cast not found")]
    [else
     (test-case "C1: rlp-encode agrees with cast to-rlp"
       (define prng (seeded 1 9 8 4 2 7))
       (define items
         (take (filter (lambda (x) (<= (bytes-length (rlp-encode x)) 8192))
                       (for/list ([_ (in-range 400)]) (random-rlp-item prng)))
               200))
       (for ([x (in-list items)])
         (check-equal? (run-cast "to-rlp" (item->cast-argument x))
                       (bytes->hex-string (rlp-encode x))
                       (item->cast-argument x))))

     (test-case "C2: rlp-decode agrees with cast from-rlp, which ignores trailing bytes"
       (define prng (seeded 2 9 8 4 2 7))
       (define corpus
         (append (for/list ([(_ v) (in-hash (rlp-vectors "invalidRLPTest.json"))])
                   (hex-string->bytes (hash-ref v 'out)))
                 (map hex-string->bytes
                      '("0xb900" "0xb90000" "0xc5010203" "0xc18100" "0xc28100" "0xc2c18100"
                        "0xc0c0"))
                 (strictness-samples prng)
                 (take (append* (for/list ([_ (in-range 60)])
                                  (mutations prng (random-encoding prng))))
                       200)))
       (for ([bs (in-list corpus)])
         (define name (input-name bs))
         (define fault (rlp-fault (lambda () (rlp-decode bs))))
         (match (cast-outcome "from-rlp" (bytes->hex-string bs))
           [(list 'failed stderr)
            (check-pred exn:fail:web3:rlp? fault name)
            (when fault
              (check-equal? (fault-phrase fault) (cast-phrase stderr) name))]
           [(list 'ok output)
            #:when (not fault)
            (check-equal? (item->cast-output (rlp-decode bs)) output name)]
           [(list 'ok output)
            (check-eq? (exn:fail:web3:rlp-reason fault) 'trailing name)
            (define k (exn:fail:web3:rlp-offset fault))
            (check-equal? (item->cast-output (rlp-decode (subbytes bs 0 k))) output name)])))

     (test-case "C3: scalars agree with cast from-rlp --as-int"
       (define prng (seeded 3 9 8 4 2 7))
       (for ([n (in-list (append (list 0 1 127 128 255 256 (sub1 (expt 2 256)))
                                 (for/list ([_ (in-range 64)]) (random-natural prng 32))))])
         (check-equal? (run-cast "from-rlp" "--as-int"
                                 (bytes->hex-string (rlp-encode (natural->rlp-scalar n))))
                       (number->string n)))
       (for ([row (in-list '((#"\0" leading-zero) (#"\0\1" leading-zero)
                             (() list) ((#"") list)))])
         (match-define (list item reason) row)
         (define fault (rlp-fault (lambda () (rlp-scalar->natural item))))
         (check-pred exn:fail:web3:rlp? fault)
         (when fault
           (check-eq? (exn:fail:web3:rlp-reason fault) reason)
           (match (cast-outcome "from-rlp" "--as-int" (bytes->hex-string (rlp-encode item)))
             [(list 'failed stderr)
              (check-regexp-match (regexp-quote (fault-phrase fault)) stderr)]
             [other (fail (format "cast accepted ~s: ~s" item other))]))))]))
