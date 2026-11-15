#lang racket/base

;; CREATE and CREATE2 addresses against Foundry's cast.  Skipped when cast
;; is not available.

(module+ test
  (require racket/list
           racket/string
           rackunit
           web3
           "abi-gen.rkt"
           "cast.rkt"
           "util.rkt")

  (define max-nonce (- (expt 2 64) 2))

  (cond
    [(not cast-path)
     (displayln "skipping cast differential: cast not found")]
    [else
     (test-case "create-address agrees with cast compute-address"
       (define prng (seeded 9 9 1 2 3 4))
       (define nonces
         (append '(0 1 127 128 255 256) (list max-nonce)
                 (for/list ([_ (in-range 193)]) (min max-nonce (random-abi-value prng 'uint64)))))
       (for ([nonce (in-list nonces)])
         (define sender (random-address prng))
         (define expected
           (last (string-split (run-cast "compute-address" "--nonce" (number->string nonce) "--" sender))))
         (check-equal? (create-address sender nonce) expected (format "~a ~a" sender nonce))))

     (test-case "create2-address agrees with cast create2"
       (define prng (seeded 2 7 1 8 2 8))
       (for ([_ (in-range 200)])
         (define deployer (random-address prng))
         (define salt (random-bytes prng 32))
         (define code (bytes->hex-string (random-bytes prng (random 301 prng))))
         (define expected
           (first (string-split (run-cast "create2" "--deployer" deployer
                                          "--salt" (bytes->hex-string salt)
                                          "--init-code" code))))
         (check-equal? (create2-address deployer salt code) expected (format "~a ~a" deployer code))))]))
