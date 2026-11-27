#lang racket/base

(module+ test
  (require racket/list
           racket/match
           rackunit
           web3/abi/type
           web3/address
           "abi-gen.rkt"
           "modules.rkt"
           "util.rkt")

  ;; The eight from EIP-55, then two from cast to-check-sum-address.
  (define vectors
    `("0x52908400098527886E0F7030069857D2E4169EE7"
      "0x8617E340B3D01FA5F11F306F4090FD50E238070D"
      "0xde709f2102306220921060314715629080e2fb77"
      "0x27b1fdb04752bbc536007a920d24acb045561c26"
      "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed"
      "0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359"
      "0xdbF03B407c01E7cD3CBea99509d93f8DDDC8C6FB"
      "0xD1220A0cf47c7B9Be7A2E6BA89F429762e7b9aDb"
      ,zero-address
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
    (define prng (seeded 5 5 5 5 5 5))
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

  (define address-cases
    '(("0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed" #t)
      ("0x5AAEB6053F3E94C9B9A09F33669435E7EF1BEAED" #t)
      ("0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed" #t)
      ("0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359" #t)
      ("0xdbF03B407c01E7cD3CBea99509d93f8DDDC8C6FB" #t)
      ("0xD1220A0cf47c7B9Be7A2E6BA89F429762e7b9aDb" #t)
      ("0x52908400098527886E0F7030069857D2E4169EE7" #t)
      ("0x8617E340B3D01FA5F11F306F4090FD50E238070D" #t)
      ("0xde709f2102306220921060314715629080e2fb77" #t)
      ("0x27b1fdb04752bbc536007a920d24acb045561c26" #t)
      ("0x0000000000000000000000000000000000000000" #t)
      ("0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD" #f)
      ("0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAe" #f)
      ("0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed0" #f)
      ("5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed" #f)
      ("0X5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed" #f)
      ("0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaeg" #f)
      (#"0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed" #f)
      (42 #f)
      (address #f)))

  (test-case "address?: lowercase, uppercase or the EIP-55 checksum"
    (for ([row (in-list address-cases)])
      (match-define (list v expected) row)
      (check-equal? (address? v) expected (format "~s" v))))

  (test-case "address? refuses each checksummed vector with one letter's case flipped"
    (for* ([a (in-list vectors)]
           [i (in-range 2 42)]
           #:when (char-alphabetic? (string-ref a i)))
      (define flipped (string-copy a))
      (string-set! flipped i (flip-case (string-ref a i)))
      (check-false (address? flipped) flipped))
    (for ([a (in-list vectors)])
      (check-true (address? (string-downcase a)) a)
      (check-true (address? (upcase-hex a)) a)))

  (test-case "address->checksum-string blames its caller"
    (check-blames-here (address->checksum-string "0x123"))
    (check-blames-here (address->checksum-string 42))
    (check-blames-here (address->checksum-string "5aaeb6053f3e94c9b9a09f33669435e7ef1beaed"))
    (check-blames-here (address->checksum-string "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD")))

  (test-case "an address is its 20 bytes and its number, and back in its EIP-55 form"
    (for ([a (in-list vectors)])
      (define bs (address->bytes a))
      (check-equal? (bytes-length bs) 20 a)
      (check-equal? (address->bytes (string-downcase a)) bs a)
      (check-equal? (address->bytes (upcase-hex a)) bs a)
      (check-equal? (bytes->address bs) a a)
      (define n (address->integer a))
      (check-equal? n (string->number (substring a 2) 16) a)
      (check-equal? (address->integer (string-downcase a)) n a)
      (check-equal? (integer->address n) a a))
    (check-equal? (integer->address 0) zero-address)
    (check-equal? (integer->address (sub1 (expt 2 160)))
                  "0xFFfFfFffFFfffFFfFFfFFFFFffFFFffffFfFFFfF")
    (check-equal? (integer->address 1) "0x0000000000000000000000000000000000000001")
    (check-equal? (address->bytes zero-address) (make-bytes 20 0))
    (check-equal? (address->integer zero-address) 0))

  (test-case "the conversions refuse what is not an address, 20 bytes or 160 bits"
    (check-blames-here (bytes->address (make-bytes 19 1)))
    (check-blames-here (bytes->address (make-bytes 21 1)))
    (check-blames-here (bytes->address "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed"))
    (check-blames-here (integer->address -1))
    (check-blames-here (integer->address (expt 2 160)))
    (check-blames-here (integer->address 1.0))
    (check-blames-here (address->bytes "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD"))
    (check-blames-here (address->bytes (make-bytes 20 1)))
    (check-blames-here (address->integer "0x123"))
    (check-blames-here (address->integer 42)))

  (test-case "web3/address needs only racket/base libraries, keccak and web3/private/hex"
    (define names (filter values (map module-name (import-closure 'web3/address))))
    (check-equal? (filter (lambda (n) (member (collection n) '("net" "json"))) names) '())
    (check-equal? (sort (remove-duplicates (filter (lambda (n) (equal? (collection n) "web3")) names))
                        string<?)
                  '("web3/address" "web3/private/hex"))))
