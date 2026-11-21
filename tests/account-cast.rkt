#lang racket/base

(module+ test
  (require (only-in racket/dict in-dict)
           rackunit
           (only-in libsecp256k1 secret-key->bytes)
           web3/account
           (only-in web3/private/hex bytes->natural natural->bytes)
           (only-in web3/rpc bytes->hex-string hex-string->bytes)
           "cast.rkt"
           "helpers/accounts.rkt")

  (define cases (key-digest-cases 64 3 1 4 1 5 9))

  (define (key-hex k)
    (bytes->hex-string (secret-key->bytes k)))

  (define (with-y-parity-byte sig)
    (define bs (signature->bytes sig))
    (bytes->hex-string (bytes-append (subbytes bs 0 64) (bytes (signature-y-parity sig)))))

  (define (verifies? address digest sig-hex)
    (cast-succeeds? "wallet" "verify" "--no-hash" "--address" address
                    (bytes->hex-string digest) sig-hex))

  (cond
    [(not cast-path)
     (displayln "skipping account differential: cast not found")]
    [else
     (test-case "secret-key->address agrees with cast wallet address"
       (for ([(k _) (in-dict cases)])
         (check-equal? (secret-key->address k)
                       (run-cast "wallet" "address" "--private-key" (key-hex k)))))

     (define (cast-sign k d)
       (run-cast "wallet" "sign" "--no-hash" "--private-key" (key-hex k) (bytes->hex-string d)))

     (test-case "sign-digest agrees with cast wallet sign --no-hash on digests below n"
       (for ([(k d) (in-dict cases)]
             #:when (< (bytes->natural d) secp256k1-order))
         (check-equal? (signature-hex (sign-digest k d)) (cast-sign k d))))

     ;; cast 1.8.4 signs a digest of n or more with another nonce than libsecp256k1 does.
     (test-case "a digest of n or more signs as the digest less n, which is how cast signs it"
       (define d (make-bytes 32 255))
       (define reduced (natural->bytes (- (bytes->natural d) secp256k1-order) 32))
       (for ([k (in-list edge-keys)])
         (check-equal? (signature-hex (sign-digest k d)) (cast-sign k reduced))
         (define theirs (bytes->signature (hex-string->bytes (cast-sign k d))))
         (check-equal? (recover-address theirs d) (secret-key->address k))))

     (test-case "cast verifies what recover-address recovers, and refuses the flipped parity"
       (for ([(k d) (in-dict cases)])
         (define address (secret-key->address k))
         (define sig (sign-digest k d))
         (check-true (verifies? address d (signature-hex sig)))
         (check-true (verifies? address d (with-y-parity-byte sig)))
         (check-true (verifies? address d (signature-hex (other-s sig))))
         (check-false (verifies? address d (signature-hex (flipped-parity sig))))
         (check-not-equal? (recover-address (flipped-parity sig) d) address)))

     (test-case "neither cast nor hex-string->secret-key trims a trailing newline"
       (define text (string-append (car anvil-keys) "\n"))
       (check-false (cast-succeeds? "wallet" "address" "--private-key" text))
       (check-exn exn:fail:contract? (lambda () (hex-string->secret-key text))))]))
