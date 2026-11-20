#lang racket/base

(module+ test
  (require (only-in racket/contract exn:fail:contract:blame?)
           (only-in racket/dict in-dict)
           racket/list
           racket/match
           rackunit
           (only-in scribble/example make-base-eval)
           (only-in libsecp256k1
                    bytes->public-key
                    ecdsa-recoverable-signature
                    exn:fail:libsecp256k1:key?
                    integer->secret-key
                    public-key->bytes)
           web3/account
           (only-in (submod web3/account private) recoverable->signature)
           (only-in web3/address checksum-address?)
           (only-in web3/rpc bytes->hex-string hex-string->bytes)
           "helpers/accounts.rkt"
           "modules.rkt"
           "util.rkt")

  (define n secp256k1-order)

  (define cases (key-digest-cases 192 1 4 1 4 2 1))

  (define (with-last-byte bs v)
    (bytes-append (subbytes bs 0 64) (bytes v)))

  (define (signature-bytes r s v)
    (bytes-append (word r) (word s) (bytes v)))

  (define (raised-signature-error thunk)
    (with-handlers ([exn:fail:web3:signature? values])
      (thunk)
      #f))

  (define (raised-key-error thunk)
    (with-handlers ([exn:fail:web3:key? values])
      (thunk)
      #f))

  (define (check-signature-error thunk message)
    (define e (raised-signature-error thunk))
    (check-pred exn:fail:web3:signature? e message)
    (when e
      (check-equal? (exn-message e) message)))

  (define invalid-key-message
    (string-append "hex-string->secret-key: invalid secret key;\n"
                   " the value is zero or not less than the curve order"))

  (define (check-key-error s)
    (define e (raised-key-error (lambda () (hex-string->secret-key s))))
    (check-pred exn:fail:web3:key? e s)
    (when e
      (check-equal? (exn-message e) invalid-key-message)))

  (define (v-message v)
    (format "bytes->signature: invalid signature;\n v is ~a, not 0, 1, 27 or 28" v))

  (define (scalar-message name)
    (format "bytes->signature: invalid signature;\n ~a is zero or not less than the curve order"
            name))

  (define (hex-of n)
    (bytes->hex-string (word n)))

  (define anvil-0 (car anvil-keys))
  (define anvil-0-digits (substring anvil-0 2))

  (test-case "a signature recovers the address of the key that made it"
    (for ([c (in-list cases)])
      (match-define (cons k d) c)
      (define address (secret-key->address k))
      (define recovered (recover-address (sign-digest k d) d))
      (check-equal? recovered address)
      (check-pred checksum-address? address)
      (check-pred checksum-address? recovered)))

  (test-case "signing is deterministic, also for two keys read from the same hex"
    (for ([c (in-list cases)])
      (match-define (cons k d) c)
      (check-equal? (sign-digest k d) (sign-digest k d)))
    (define k1 (hex-string->secret-key anvil-0))
    (define k2 (hex-string->secret-key anvil-0))
    (check-false (equal? k1 k2))
    (check-equal? (sign-digest k1 digest-empty) (sign-digest k2 digest-empty)))

  (test-case "every signature has a low s"
    (for ([c (in-list cases)])
      (match-define (cons k d) c)
      (check-true (signature-low-s? (sign-digest k d)))))

  (test-case "the other s with the other parity is high and recovers the same address"
    (for ([c (in-list cases)])
      (match-define (cons k d) c)
      (define high (other-s (sign-digest k d)))
      (check-false (signature-low-s? high))
      (check-equal? (recover-address high d) (secret-key->address k))))

  (test-case "the wrong parity never recovers the signer"
    (for ([c (in-list cases)])
      (match-define (cons k d) c)
      (define recovered (recover-address (flipped-parity (sign-digest k d)) d))
      (check-not-equal? recovered (secret-key->address k))
      (when recovered
        (check-pred checksum-address? recovered))))

  (test-case "65 bytes round trip through a signature"
    (for ([c (in-list cases)])
      (match-define (cons k d) c)
      (define sig (sign-digest k d))
      (check-equal? (bytes->signature (signature->bytes sig)) sig))
    (define prng (seeded 6 5 6 5 6 5))
    (define (random-scalar) (add1 (modulo (random-natural prng) (sub1 n))))
    (for ([_ (in-range 200)])
      (define y (random 2 prng))
      (define bs (signature-bytes (random-scalar) (random-scalar) (+ 27 y)))
      (check-equal? (signature->bytes (bytes->signature bs)) bs)
      (check-equal? (signature->bytes (bytes->signature (with-last-byte bs y))) bs)))

  (test-case "65 bytes are r, s and v, big-endian"
    (for ([c (in-list cases)])
      (match-define (cons k d) c)
      (match-define (and sig (signature r s y)) (sign-digest k d))
      (define bs (signature->bytes sig))
      (check-equal? (subbytes bs 0 32) (word r))
      (check-equal? (subbytes bs 32 64) (word s))
      (check-equal? (bytes-ref bs 64) (signature-v sig))
      (check-equal? (signature-v sig) (+ 27 y))))

  (test-case "bytes->signature refuses every other v, and r or s out of range"
    (define bs (signature-bytes 5 6 27))
    (for ([v (in-range 256)]
          #:unless (memv v '(0 1 27 28)))
      (check-signature-error (lambda () (bytes->signature (with-last-byte bs v))) (v-message v)))
    (for ([bad (in-list (list 0 n max-word))])
      (check-signature-error (lambda () (bytes->signature (signature-bytes bad 6 27)))
                             (scalar-message "r"))
      (check-signature-error (lambda () (bytes->signature (signature-bytes 5 bad 27)))
                             (scalar-message "s")))
    (check-signature-error (lambda () (bytes->signature (signature-bytes 0 0 2)))
                           (v-message 2))
    (check-signature-error (lambda () (bytes->signature (signature-bytes 0 0 27)))
                           (scalar-message "r"))
    (check-equal? (bytes->signature (signature-bytes (sub1 n) (sub1 n) 28))
                  (signature (sub1 n) (sub1 n) 1)))

  (test-case "anvil's ten keys give its ten addresses"
    (check-equal? (map (lambda (k) (secret-key->address (hex-string->secret-key k))) anvil-keys)
                  anvil-addresses))

  (test-case "the edge keys give cast's addresses, and 0 and n are refused"
    (check-equal? (secret-key->address (hex-string->secret-key (hex-of 1)))
                  "0x7E5F4552091A69125d5DfCb7b8C2659029395Bdf")
    (check-equal? (secret-key->address (hex-string->secret-key (hex-of (sub1 n))))
                  "0x80C0dbf239224071c59dD8970ab9d542E3414aB2")
    (check-equal? (hex-of (sub1 n))
                  "0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140")
    (check-key-error (hex-of 0))
    (check-key-error (hex-of n)))

  (test-case "anvil key 0's public key is cast's"
    (check-equal? (bytes->hex-string
                   (subbytes (public-key->bytes (secret-key->public-key
                                                 (hex-string->secret-key anvil-0))
                                                #:compressed? #f)
                             1))
                  (string-append "0x8318535b54105d4a7aae60c08fc45f9687181b4fdfc625bd1a753fa7397fed75"
                                 "3547f11ca8696646f2f3acb08e31016afac23e630c5d11f59f61fef57b0d2aa5")))

  (define vectors
    `((0 ,digest-empty
         ,(string-append "0xf9a45c11d904141226963b371ccbc2128a29d146ce3c0d43326414711e3e153d"
                         "536804917f10c70c5d0cd7b730b559d95eb812d1d783f6dd534e5854f01b3e551b"))
      (1 ,digest-empty
         ,(string-append "0x6c34308e58656c8487b9b2173422910d87f964c18e8a771fccabf4ca292469a6"
                         "4dba120621e2527af3bce8836644944eb42d9dae4d795ea27824479b2fddca571b"))
      (2 ,digest-empty
         ,(string-append "0x89006c0850e1d600b7ab2c2a61f7761cee209b5dbfa2141807d1b7c053a66b5b"
                         "4bc4f0695d74d2994ebee75d084a1a4c1ba74501ad0642c2d572b6d81325c93d1c"))
      (3 ,digest-empty
         ,(string-append "0x18c5f54134f527dcf40904802cf7336f7ce4c3abcb4bd366752875bab612b4b9"
                         "62659649370da829fa1632e614e083f6becb3efce86d0357bab83d4e509ddfcf1c"))
      (4 ,digest-empty
         ,(string-append "0xee5f36f463ece6de1369b938e1b569d77a13b8bbd7c54e624d7808340da64c5c"
                         "55c8465a6db010b1ebad6f8c253f538b8934257d8ce0ff0b3b7bb17c3f48de631c"))
      (key-1 ,digest-empty
             ,(string-append "0x03925438bf9bdfed9cb8d9d9467f8fc624f389846f0db71f4f5c84b483077da6"
                             "2ca68cb1027ace392bc6a84fe0ba29288aa07942f571f56201dd33c009ee38861c"))
      (0 ,digest-hello
         ,(string-append "0x73eebf81a611136662d65778960c853fdcaf6eca86793ed9cabc30f2195937af"
                         "78a07e601627da5b4cc80c0ab35f6894da19b4a01759d90c101d9c9dd1c6745d1b"))))

  (test-case "signatures match cast's"
    (check-equal? (bytes->hex-string digest-empty)
                  "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470")
    (check-equal? (bytes->hex-string digest-hello)
                  "0x1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8")
    (for ([v (in-list vectors)])
      (match-define (list which d expected) v)
      (define key
        (hex-string->secret-key (if (eq? which 'key-1) (hex-of 1) (list-ref anvil-keys which))))
      (check-equal? (signature-hex (sign-digest key d)) expected (format "~a" which)))
    (check-equal? (remove-duplicates (map (match-lambda [(list _ _ hex) (string-ref hex 131)]) vectors))
                  '(#\b #\c)))

  (test-case "the flipped and the high-s forms of anvil 0's signature recover what the node does"
    (define sig (sign-digest (hex-string->secret-key anvil-0) digest-empty))
    (check-equal? (string-downcase (recover-address (flipped-parity sig) digest-empty))
                  "0xd149ced82ece765ecd4db5715679fa668bdca75a")
    (define high (other-s sig))
    (check-equal? (hex-of (signature-s high))
                  "0xac97fb6e80ef38f3a2f32848cf4aa6255bf6ca14d7c4a95e6c840637e01b02ec")
    (check-equal? (signature-v high) 28)
    (check-equal? (recover-address high digest-empty) anvil-alice))

  (define whitespace '(" " "\t" "\n" "\r\n"))

  (define refused-key-texts
    (append
     (list (substring anvil-0-digits 1)
           (string-append anvil-0-digits "0")
           ""
           "0x"
           (string-append (substring anvil-0-digits 1) "g")
           (string-append (substring anvil-0-digits 1) "０")
           (string-append (substring anvil-0-digits 1) "\u0000")
           (string-append "0x0x" anvil-0-digits))
     (for*/list ([w (in-list whitespace)]
                 [s (in-list (list (string-append w anvil-0) (string-append anvil-0 w)))])
       s)))

  (test-case "keys are read with 0x, 0X or no prefix, in either case"
    (define forms
      (for*/list ([prefix (in-list '("0x" "0X" ""))]
                  [digits (in-list (list anvil-0-digits (string-upcase anvil-0-digits)))])
        (string-append prefix digits)))
    (for ([s (in-list forms)])
      (check-equal? (secret-key->address (hex-string->secret-key s)) anvil-alice s)))

  (test-case "key text of the wrong shape is the caller's contract violation"
    (for ([s (in-list refused-key-texts)])
      (check-blames-here (hex-string->secret-key s)))
    (check-blames-here (hex-string->secret-key (string->bytes/utf-8 anvil-0)))
    (check-blames-here (hex-string->secret-key (string->symbol anvil-0))))

  (test-case "key digits of zero or at least n raise exn:fail:web3:key"
    (check-key-error (make-string 64 #\0))
    (check-key-error (hex-of n))
    (check-key-error (make-string 64 #\f)))

  (test-case "public-key->address agrees on a key's 33-byte and 65-byte forms"
    (for ([(k _) (in-dict cases)])
      (define pk (secret-key->public-key k))
      (define compressed (bytes->public-key (public-key->bytes pk #:compressed? #t)))
      (define uncompressed (bytes->public-key (public-key->bytes pk #:compressed? #f)))
      (check-equal? (public-key->address compressed) (public-key->address uncompressed))
      (check-equal? (public-key->address pk) (secret-key->address k))))

  (test-case "recovery ids 2 and 3 cannot be encoded; 0 and 1 are the y-parity"
    (for ([id (in-list '(2 3))])
      (check-signature-error
       (lambda () (recoverable->signature 'sign-digest (ecdsa-recoverable-signature 5 6 id)))
       (format "sign-digest: the signature's recovery id is ~a; Ethereum encodes only 0 and 1"
               id)))
    (for ([id (in-list '(0 1))])
      (check-equal? (recoverable->signature 'sign-digest (ecdsa-recoverable-signature 5 6 id))
                    (signature 5 6 id))))

  (define (on-curve? x)
    (with-handlers ([exn:fail:libsecp256k1:key? (lambda (_) #f)])
      (bytes->public-key (bytes-append #"\2" (word x)))
      #t))

  (test-case "an r that is no curve point's x recovers nothing"
    (define r0 (for/first ([r (in-naturals 1)] #:unless (on-curve? r)) r))
    (for ([(_ d) (in-dict cases)])
      (check-false (recover-address (signature r0 1 0) d))))

  (define good-key (integer->secret-key 7))
  (define good-public-key (secret-key->public-key good-key))
  (define good-signature (signature 5 6 0))
  (define bad-keys (list anvil-0 (string->bytes/utf-8 anvil-0) good-public-key))
  (define bad-digests (list (make-bytes 31 1) (make-bytes 33 1) (bytes->hex-string digest-empty)))
  (define bad-signatures
    (list (make-bytes 64 1) (make-bytes 65 27) 5 (ecdsa-recoverable-signature 5 6 0)))
  (define bad-messages (list 5 'm (list #"m") good-key))
  (define bad-addresses (list "0x12" (make-bytes 20 1) 5 (substring anvil-alice 2)))

  (define surface
    (list (list hex-string->secret-key (list anvil-0)
                (list (list (string->bytes/utf-8 anvil-0) 'key good-key)))
          (list secret-key->address (list good-key) (list bad-keys))
          (list public-key->address (list good-public-key) (list (list anvil-0 #"\2" good-key)))
          (list sign-digest (list good-key digest-empty) (list bad-keys bad-digests))
          (list recover-address (list good-signature digest-empty)
                (list bad-signatures bad-digests))
          (list signature-low-s? (list good-signature) (list bad-signatures))
          (list signature-v (list good-signature) (list bad-signatures))
          (list signature->bytes (list good-signature) (list bad-signatures))
          (list bytes->signature (list (signature->bytes good-signature))
                (list (list (make-bytes 64 1) (make-bytes 66 27) 5 anvil-0)))
          (list hash-message (list "m") (list bad-messages))
          (list sign-message (list good-key "m") (list bad-keys bad-messages))
          (list recover-message-address (list good-signature "m")
                (list bad-signatures bad-messages))
          (list message-signed-by? (list good-signature "m" anvil-alice)
                (list bad-signatures bad-messages bad-addresses))))

  (test-case "an argument of the wrong kind is the caller's contract violation"
    (for ([entry (in-list surface)])
      (match-define (list f good bads) entry)
      (for ([position (in-naturals)]
            [wrong (in-list bads)])
        (for ([v (in-list wrong)])
          (check-blames-here (apply f (list-set good position v)))))))

  (test-case "a wrong arity or an unknown keyword is the caller's contract violation"
    (for ([entry (in-list surface)])
      (match-define (list f good _) entry)
      (check-blames-here (apply f (cons 1 good)))
      (check-blames-here (apply f (cdr good)))
      (check-blames-here (keyword-apply f '(#:chain-id) '(1) good))))

  (test-case "the signature constructor checks its fields"
    (check-blames-here (signature 0 6 0))
    (check-blames-here (signature n 6 0))
    (check-blames-here (signature 5 0 0))
    (check-blames-here (signature 5 n 0))
    (check-blames-here (signature 5 6 2))
    (check-equal? (signature-r (signature (sub1 n) 1 1)) (sub1 n)))

  (test-case "signature is a constructor and a match pattern, but no struct type to extend"
    (define ev (make-base-eval '(require racket/match web3)))
    (check-equal? (ev '(match (map signature '(5 7) '(6 8) '(0 1))
                         [(list (signature r s y) ...) (list r s y)]))
                  '((5 7) (6 8) (0 1)))
    (for ([form (in-list '((struct-copy signature (signature 5 6 0) [r 0])
                           (struct forged signature ())
                           (struct forged signature () #:constructor-name forged)))])
      (check-exn exn:fail? (lambda () (ev form)) (format "~s" form))))

  (define (refusal thunk)
    (with-handlers ([exn:fail:contract:blame? values])
      (thunk)
      #f))

  (define expected-kinds
    `((,(lambda () (sign-digest good-key (make-bytes 31 1))) "a 32-byte digest")
      (,(lambda () (recover-address good-signature digest-empty #"")) "2 arguments")
      (,(lambda () (recover-address good-signature anvil-0)) "a 32-byte digest")
      (,(lambda () (sign-digest anvil-0 digest-empty)) "a secret key")
      (,(lambda () (secret-key->address 7)) "a secret key")
      (,(lambda () (signature-r anvil-0)) "a signature")
      (,(lambda () (signature 0 1 0)) "an integer in [1, n)")
      (,(lambda () (signature 1 1 2)) "0 or 1")))

  (test-case "a refused argument is named by the kind the manual names"
    (for ([entry (in-list expected-kinds)])
      (match-define (list call expected) entry)
      (define e (refusal call))
      (check-pred exn:fail:contract:blame? e expected)
      (when e
        (check-regexp-match (regexp (string-append "\n  expected: " (regexp-quote expected) "\n"))
                            (exn-message e)))))

  (define (acceptable-raise? e)
    (or (exn:fail:contract:blame? e) (exn:fail:web3:key? e) (exn:fail:web3:signature? e)))

  (test-case "every export raises only contract violations and its own exceptions"
    (define inputs
      (append refused-key-texts bad-keys bad-digests bad-signatures
              (list (make-string 64 #\0) (hex-of n) (signature-bytes 0 6 27)
                    (signature-bytes 5 n 27) (signature-bytes 5 6 37))))
    (for ([entry (in-list surface)])
      (match-define (list f good _) entry)
      (for* ([position (in-range (length good))]
             [v (in-list inputs)])
        (define raised
          (with-handlers ([(lambda (_) #t) values])
            (apply f (list-set good position v))
            #f))
        (when raised
          (check-true (acceptable-raise? raised) (format "~a" (object-name f)))))))

  (test-case "web3/account needs no network, JSON or node modules"
    (define names (filter values (map module-name (import-closure 'web3/account))))
    (check-equal? (filter (lambda (n) (member (collection n) '("net" "json"))) names) '())
    (check-equal? (filter (lambda (n) (member n '("web3/rpc" "web3/eth" "web3/contract"
                                                  "web3/anvil")))
                          names)
                  '()))

  (test-case "web3 and libsecp256k1 can be required together and share secret-key?"
    (define ev (make-base-eval))
    (ev '(module both racket/base
           (require web3 libsecp256k1 (prefix-in l: libsecp256k1))
           (provide same?)
           (define same? (eq? secret-key? l:secret-key?))))
    (ev '(require (quote both)))
    (check-true (ev 'same?)))

  (test-case "a missing native library is reported on use, not on require"
    (define env (environment-variables-copy (current-environment-variables)))
    (environment-variables-set! env #"LIBSECP256K1_LIBRARY" #"/nonexistent/libsecp256k1.so")
    (define ev
      (parameterize ([current-environment-variables env])
        (make-base-eval '(require web3 libsecp256k1))))
    (check-equal? (ev '(with-handlers ([exn:fail:libsecp256k1:load? (lambda (_) 'load)])
                         (sign-digest (generate-secret-key) (make-bytes 32 0))))
                  'load))

  (test-case "signing and recovering from 8 threads gives the sequential results"
    (define inputs (key-digest-cases 50 8 8 8 1 2 3))
    (define (results)
      (for/list ([(k d) (in-dict inputs)])
        (define sig (sign-digest k d))
        (cons sig (recover-address sig d))))
    (define expected (results))
    (define channels
      (for/list ([_ (in-range 8)])
        (define ch (make-channel))
        (thread (lambda () (channel-put ch (results))))
        ch))
    (for ([ch (in-list channels)])
      (check-equal? (channel-get ch) expected))))
