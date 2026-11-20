#lang racket/base

;; Keys, addresses and signatures against Foundry's cast.  Skipped when cast is not available.

(module+ test
  (require racket/match
           rackunit
           (only-in libsecp256k1 secret-key->bytes)
           web3/account
           (only-in web3/private/hex bytes->natural)
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
       (for ([c (in-list cases)])
         (define k (car c))
         (check-equal? (secret-key->address k)
                       (run-cast "wallet" "address" "--private-key" (key-hex k)))))

     (define (cast-sign k d)
       (run-cast "wallet" "sign" "--no-hash" "--private-key" (key-hex k) (bytes->hex-string d)))

     (test-case "sign-digest agrees with cast wallet sign --no-hash on digests below n"
       (for ([c (in-list cases)]
             #:when (< (bytes->natural (cdr c)) secp256k1-order))
         (match-define (cons k d) c)
         (check-equal? (signature-hex (sign-digest k d)) (cast-sign k d))))

     ;; libsecp256k1 derives the nonce from the digest reduced mod n (RFC 6979 3.2d); cast
     ;; 1.8.4 signs digests of n or more with another nonce.
     (test-case "on a digest of n or more, cast's signature and ours both recover the signer"
       (for ([k (in-list edge-keys)])
         (define d (make-bytes 32 255))
         (define theirs (bytes->signature (hex-string->bytes (cast-sign k d))))
         (check-not-equal? theirs (sign-digest k d))
         (check-equal? (recover-address theirs d) (secret-key->address k))))

     (test-case "cast verifies what recover-address recovers, and refuses the flipped parity"
       (for ([c (in-list cases)])
         (match-define (cons k d) c)
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
