#lang racket/base

(module+ test
  (require racket/list
           rackunit
           web3/abi/type
           web3/address
           "abi-gen.rkt"
           "modules.rkt"
           "util.rkt")

  ;; The eight from EIP-55, then two from cast to-check-sum-address.
  (define vectors
    '("0x52908400098527886E0F7030069857D2E4169EE7"
      "0x8617E340B3D01FA5F11F306F4090FD50E238070D"
      "0xde709f2102306220921060314715629080e2fb77"
      "0x27b1fdb04752bbc536007a920d24acb045561c26"
      "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed"
      "0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359"
      "0xdbF03B407c01E7cD3CBea99509d93f8DDDC8C6FB"
      "0xD1220A0cf47c7B9Be7A2E6BA89F429762e7b9aDb"
      "0x0000000000000000000000000000000000000000"
      "0xFFfFfFffFFfffFFfFFfFFFFFffFFFffffFfFFFfF"))

  (define (upcase-hex a)
    (string-append "0x" (string-upcase (substring a 2))))

  (define (flip-case c)
    (if (char-upper-case? c) (char-downcase c) (char-upcase c)))

  (test-case "EIP-55 vectors"
    (for ([a (in-list vectors)])
      (check-equal? (address->checksum-string (string-downcase a)) a)
      (check-equal? (address->checksum-string (upcase-hex a)) a)
      (check-true (checksum-address? a) a)))

  (test-case "checksums keep the digits, ignore the input's case and are fixed points"
    (define prng (vector->pseudo-random-generator (vector 5 5 5 5 5 5)))
    (for ([_ (in-range 200)])
      (define a (random-abi-value prng 'address))
      (define c (address->checksum-string a))
      (check-equal? (string-downcase c) (string-downcase a))
      (check-equal? (address->checksum-string c) c)
      (check-equal? (address->checksum-string (string-downcase a)) c)
      (check-equal? (address->checksum-string (upcase-hex a)) c)
      (check-true (checksum-address? c) c)))

  (test-case "checksum-address? rejects other spellings and other values"
    (for* ([a (in-list vectors)]
           [i (in-range 2 42)]
           #:when (char-alphabetic? (string-ref a i)))
      (define flipped (string-copy a))
      (string-set! flipped i (flip-case (string-ref a i)))
      (check-false (checksum-address? flipped) flipped))
    (define a "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed")
    (for ([v (in-list (list (string-downcase a)
                            (string-append "0X" (substring a 2))
                            (substring a 0 41)
                            (string-append a "0")
                            'a
                            (string->bytes/utf-8 a)))])
      (check-false (checksum-address? v) (format "~s" v))))

  (test-case "address->checksum-string blames its caller"
    (check-blames-here (address->checksum-string "0x123"))
    (check-blames-here (address->checksum-string 42))
    (check-blames-here (address->checksum-string "5aaeb6053f3e94c9b9a09f33669435e7ef1beaed")))

  (test-case "web3/address needs only racket/base libraries, keccak and web3/abi/type"
    (define names (filter values (map module-name (import-closure 'web3/address))))
    (check-equal? (filter (lambda (n) (member (collection n) '("net" "json"))) names) '())
    (check-equal? (sort (remove-duplicates (filter (lambda (n) (equal? (collection n) "web3")) names))
                        string<?)
                  '("web3/abi/type" "web3/address" "web3/private/exn"))))
