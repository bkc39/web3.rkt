#lang racket/base

(module+ test
  (require (only-in libsecp256k1 integer->secret-key)
           racket/match
           rackunit
           web3/account
           (only-in web3/rpc bytes->hex-string hex-string->bytes)
           "helpers/accounts.rkt"
           "helpers/messages.rkt"
           (only-in "helpers/secrets.rkt" check-no-leak marker-hex marker-integer)
           (only-in "util.rkt" anvil-alice anvil-bob check-blames-here seeded))

  (define key0 (match anvil-keys [(cons hex _) (hex-string->secret-key hex)]))

  (define pinned-digests
    `(("hello" "0x50b2c43fd39106bafbba0da34fc430e1f91e3c96ea2acee2bc34119f92b37750")
      ("" "0x5f35dce98ba4fba25530a026ed80b2cecdaa31091ba4958b99b52ea1d068adad")
      (#"" "0x5f35dce98ba4fba25530a026ed80b2cecdaa31091ba4958b99b52ea1d068adad")
      ("Hello World" "0xa1de988600a42c4b4ab089b619297c17d53cffae5d5120d82d8a92d0bb3b78f2")
      ("héllo" "0x42fd95dd8ed22d7d934d6ffff71ed0b730835b0de4bb3532a271568c8760c8d2")
      ("0xdeadbeef" "0xefedd0a9a0294228c3977d7fbb68c7d40279f8b408cf3e24ef1823b179709e58")
      (#"\xde\xad\xbe\xef" "0xd1c7f1a06a4f9a535077e50ad23244ce2c6ae443fcd412965226f3df5d28eaaa")
      ("hello " "0x48e07e40bbfcf99cbd65a4079659329aa7100363b6315f50a854c889ba3e2f20")
      (" hello" "0xc2eae24ee93c01ad014884e6dfe26372aebe2b26913fbd3d83a08429d8558f09")
      ("a\nb" "0x2501af0621ba54325054a679e48c5b16d507d2b4d4dca5fa49b77c8e25e5c237")
      (,(make-string 100 #\a)
       "0x90b496d4433ae2fe10bba2f73ca6a210a57d8499db0ae5dfb05f3fbeeff2b0cf")))

  (define pinned-signatures
    `(("hello"
       ,(string-append "0xf16ea9a3478698f695fd1401bfe27e9e4a7e8e3da94aa72b021125e31fa899cc"
                       "573c48ea3fe1d4ab61a9db10c19032026e3ed2dbccba5a178235ac27f94504311c"))
      (""
       ,(string-append "0xc1977b761f1dd36c29795783460d241885c8e7f9d962dbe7bba2753fd94e89b4"
                       "44a1cd9ed855dd09afa3b73f7c2bd097ec9abc2d2775d737505a02d3f0cafa591b"))
      (#"\xde\xad\xbe\xef"
       ,(string-append "0xa114c834af73872c6c9efe918d85b0b1b34a486d10f9011e2630e28417c828c0"
                       "60dbd65cda67e73d52ebb7c555260621dbc1b0b4036acb61086bba091ac3f1641b"))
      ("0xdeadbeef"
       ,(string-append "0x74d15668584cce98918f02eccc9dec413a5a9b99b2e71b96d7b654bf036ccc48"
                       "418fbbff262f0f6480a9d46012f65b624352b8caf1b89897e7152199d3913ced1c"))
      ("héllo"
       ,(string-append "0x2b7e2472ce826ed1748e48d176e9b4c11103529bca61721fa87a9a0f78c9841f"
                       "73156d7a80d282db7f172a60d156f6dbdfa773c22f4c31d9605597ad51483a7d1c"))))

  (define messages
    (let ([prng (seeded 19 1 4 5 7 2)])
      (append edge-messages (for/list ([_ (in-range 300)]) (random-message prng)))))

  (define key-message-cases
    (let ([prng (seeded 19 1 4 5 7 3)])
      (append (for/list ([key (in-list edge-keys)]
                         [message (in-list edge-messages)])
                (list key message))
              (for/list ([_ (in-range 100)])
                (list (random-key prng) (random-message prng))))))

  (define (altered message)
    (define body (message-bytes message))
    (define changed
      (if (zero? (bytes-length body))
          (bytes 0)
          (let ([copy (bytes-copy body)])
            (bytes-set! copy 0 (bitwise-xor (bytes-ref copy 0) 1))
            copy)))
    (list (bytes-append body #"!") changed))

  (test-case "E1: pinned digests"
    (for ([row (in-list pinned-digests)])
      (match-define (list message digest) row)
      (check-equal? (bytes->hex-string (hash-message message)) digest (format "~s" message))))

  (test-case "E2: hash-message is the EIP-191 model, the length in bytes and in decimal"
    (for ([message (in-list messages)])
      (check-equal? (hash-message message) (model-hash-message message) (format "~s" message))))

  (test-case "E3: a string is hashed as its UTF-8 bytes, never as hexadecimal"
    (for ([message (in-list messages)] #:when (string? message))
      (check-equal? (hash-message message) (hash-message (string->bytes/utf-8 message))
                    (format "~s" message))))

  (test-case "E4: pinned signatures by key 0"
    (for ([row (in-list pinned-signatures)])
      (match-define (list message hex) row)
      (check-equal? (signature-hex (sign-message key0 message)) hex (format "~s" message))))

  (test-case "E5: sign-message signs hash-message's digest, with a low s"
    (for ([case (in-list key-message-cases)])
      (match-define (list key message) case)
      (define sig (sign-message key message))
      (check-equal? sig (sign-digest key (hash-message message)))
      (check-true (signature-low-s? sig))))

  (test-case "E6: the signer is recovered from its message, and not from another"
    (for ([case (in-list key-message-cases)])
      (match-define (list key message) case)
      (define sig (sign-message key message))
      (define address (secret-key->address key))
      (check-equal? (recover-message-address sig message) address)
      (for ([other (in-list (altered message))])
        (check-not-equal? (recover-message-address sig other) address))))

  (define hello-signature (sign-message key0 "hello"))

  (define (uppercase-digits address)
    (string-append "0x" (string-upcase (substring address 2))))

  (test-case "E7: message-signed-by? ignores the address's case and accepts the high-s twin"
    (for ([address (list anvil-alice (string-downcase anvil-alice) (uppercase-digits anvil-alice))])
      (check-true (message-signed-by? hello-signature "hello" address) address))
    (check-false (message-signed-by? hello-signature "hello" anvil-bob))
    (for ([other (in-list (altered "hello"))])
      (check-false (message-signed-by? hello-signature other anvil-alice)))
    (check-false (message-signed-by? (flipped-parity hello-signature) "hello" anvil-alice))
    (check-true (message-signed-by? (other-s hello-signature) "hello" anvil-alice)))

  (test-case "E8: a signature read back from its bytes, in either v form, verifies the same"
    (for ([case (in-list key-message-cases)])
      (match-define (list key message) case)
      (define sig (sign-message key message))
      (define encoded (signature->bytes sig))
      (define parity-form
        (bytes-append (subbytes encoded 0 64) (bytes (signature-y-parity sig))))
      (define address (secret-key->address key))
      (for ([form (list encoded parity-form)])
        (check-true (message-signed-by? (bytes->signature form) message address)))))

  (define marker-key (integer->secret-key marker-integer))

  (define refused
    `((,(lambda () (sign-message key0 5)) "a byte string or a string")
      (,(lambda () (sign-message marker-hex "m")) "a secret key")
      (,(lambda () (sign-message marker-key)) "2 arguments")
      (,(lambda () (sign-message marker-key "m" 1)) "2 arguments")
      (,(lambda () (sign-message marker-key "m" #:chain-id 1)) "no keyword arguments")
      (,(lambda () (hash-message 'm)) "a byte string or a string")
      (,(lambda () (recover-message-address (make-bytes 65) "m")) "a signature")
      (,(lambda () (message-signed-by? hello-signature "m" "0x12")) "an address")))

  (test-case "E9: refused arguments blame the caller, name their kind and show no key"
    (for ([row (in-list refused)])
      (match-define (list call expected) row)
      (check-blames-here (call))
      (define message (with-handlers ([exn:fail? exn-message]) (call)))
      (check-regexp-match (regexp-quote expected) message)
      (check-no-leak message))))
