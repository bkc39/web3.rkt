#lang racket/base

(module+ test
  (require (only-in libsecp256k1 secret-key->bytes)
           racket/match
           rackunit
           web3/account
           (only-in web3/rpc bytes->hex-string)
           "cast.rkt"
           "helpers/accounts.rkt"
           "helpers/messages.rkt"
           (only-in "util.rkt" seeded))

  (define prng (seeded 7 3 1 9 9 2))

  (define (key-hex key)
    (bytes->hex-string (secret-key->bytes key)))

  (define (random-strings count)
    (for/list ([_ (in-range count)])
      (string-append "m" (random-string-message prng))))

  (define (random-byte-messages count)
    (for/list ([_ (in-range count)])
      (message-bytes (random-message prng))))

  (define (cast-sign key . arguments)
    (apply run-cast "wallet" "sign" "--private-key" (key-hex key) arguments))

  (define (cast-verifies? address message sig)
    (cast-succeeds? "wallet" "verify" "--address" address "--" message (signature-hex sig)))

  (define key0 (match anvil-keys [(cons hex _) (hex-string->secret-key hex)]))

  (cond
    [(not cast-path)
     (displayln "skipping message differential: cast not found")]
    [else
     (test-case "C1: cast hash-message agrees on strings"
       (for ([message (in-list (random-strings 60))])
         (check-equal? (run-cast "hash-message" "--" message)
                       (bytes->hex-string (hash-message message))
                       (format "~s" message))))

     (test-case "C2: cast wallet sign agrees on byte messages given as hex"
       (for ([message (in-list (random-byte-messages 60))])
         (define key (random-key prng))
         (check-equal? (cast-sign key "--" (bytes->hex-string message))
                       (signature-hex (sign-message key message)))))

     (test-case "C3: cast wallet sign agrees on strings, trailing whitespace included"
       (for ([message (in-list (append '("hello " "hello\n" " a b ") (random-strings 37)))])
         (define key (random-key prng))
         (check-equal? (cast-sign key "--" message) (signature-hex (sign-message key message))
                       (format "~s" message))))

     (test-case "C4: cast wallet sign --no-hash on hash-message's digest signs the same"
       (for ([message (in-list (append (random-strings 10) (random-byte-messages 10)))])
         (define key (random-key prng))
         (check-equal? (cast-sign key "--no-hash" "--" (bytes->hex-string (hash-message message)))
                       (signature-hex (sign-message key message)))))

     (test-case "C5: cast wallet verify agrees with message-signed-by?"
       (for ([message (in-list (random-strings 30))]
             [index (in-naturals)])
         (define key (random-key prng))
         (define address (secret-key->address key))
         (define given (if (even? index) (string-downcase address) address))
         (define sig (sign-message key message))
         (check-true (cast-verifies? given message sig) (format "~s" message))
         (check-true (message-signed-by? sig message given)))
       (for ([message (in-list (random-strings 10))]
             [index (in-naturals)])
         (define key (random-key prng))
         (define address (secret-key->address key))
         (define sig (sign-message key message))
         (define other (if (even? index) (string-append message "!") (substring message 1)))
         (check-equal? (cast-verifies? address other sig) (message-signed-by? sig other address)
                       (format "~s" other))))

     (test-case "C6: where cast wallet sign reads hexadecimal and sign-message does not"
       (check-equal? (cast-sign key0 "0xdeadbeef")
                     (signature-hex (sign-message key0 #"\xde\xad\xbe\xef")))
       (check-not-equal? (cast-sign key0 "0xdeadbeef")
                         (signature-hex (sign-message key0 "0xdeadbeef")))
       (check-equal? (run-cast "hash-message" "0xdeadbeef")
                     (bytes->hex-string (hash-message "0xdeadbeef")))
       (check-false (cast-succeeds? "wallet" "sign" "--private-key" (key-hex key0) "0xabc"))
       (check-true (signature? (sign-message key0 "0xabc")))
       (define sig (sign-message key0 "hello"))
       (check-true (cast-verifies? (secret-key->address key0) "hello" (other-s sig)))
       (check-true (message-signed-by? (other-s sig) "hello" (secret-key->address key0))))]))
