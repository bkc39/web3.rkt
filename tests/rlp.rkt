#lang racket/base

(module+ test
  (require (only-in racket/contract flat-contract-predicate)
           racket/list
           racket/match
           rackunit
           (only-in keccak keccak256)
           (only-in web3/address address->checksum-string)
           (only-in web3/contract create-address)
           web3/rlp
           (submod web3/rlp internal)
           (only-in web3/private/hex bytes->natural natural->bytes)
           (only-in web3/rpc bytes->hex-string exn:fail:web3? hex-string->bytes)
           (only-in "abi-gen.rkt" random-address random-bytes)
           "bounded.rkt"
           (only-in "helpers/accounts.rkt" random-natural)
           "helpers/rlp.rkt"
           "modules.rkt"
           (only-in "util.rkt" check-blames-here seeded))

  (define reason? (flat-contract-predicate rlp-reason/c))

  (define phrases
    #hasheq((short . "input too short")
            (single-byte . "non-canonical single byte")
            (size . "non-canonical size")
            (leading-zero . "leading zero")
            (trailing . "trailing bytes")
            (list . "unexpected list")))

  (define (model-encode x)
    (define (be n)
      (if (zero? n) #"" (bytes-append (be (quotient n 256)) (bytes (remainder n 256)))))
    (define (prefixed short long payload)
      (define n (bytes-length payload))
      (if (< n 56)
          (bytes-append (bytes (+ short n)) payload)
          (bytes-append (bytes (+ long (bytes-length (be n)))) (be n) payload)))
    (match x
      [(? bytes?) #:when (and (= (bytes-length x) 1) (< (bytes-ref x 0) 128)) x]
      [(? bytes?) (prefixed 128 183 x)]
      [_ (prefixed 192 247 (apply bytes-append (map model-encode x)))]))

  (define items
    (let ([prng (seeded 2 0 1 3 1 1)])
      (append boundary-items (for/list ([_ (in-range 500)]) (random-rlp-item prng)))))

  (define encodings (map model-encode items))

  (define mutated
    (let ([prng (seeded 6 7 1 1 2 3)])
      (take (for*/list ([e (in-list (append encodings encodings))]
                        [m (in-list (mutations prng e))])
              m)
            5000)))

  (define (hex . parts)
    (hex-string->bytes (apply string-append "0x" parts)))

  (define (repeat n byte)
    (make-bytes n byte))

  (test-case "R1: decoding an encoding gives the item back"
    (for ([x (in-list items)]
          [i (in-naturals)])
      (check-equal? (rlp-decode (rlp-encode x)) x (format "item ~a" i))))

  (test-case "R2: every input rlp-decode accepts is the encoding of what it decodes to"
    (for ([bs (in-list (append encodings mutated))])
      (match (rlp-fault (lambda () (rlp-decode bs)))
        [#f (check-equal? (rlp-encode (rlp-decode bs)) bs)]
        [_ (void)])))

  (test-case "R3: rlp-encode is the Yellow Paper's encoding"
    (for ([x (in-list items)]
          [e (in-list encodings)]
          [i (in-naturals)])
      (check-equal? (rlp-encode x) e (format "item ~a" i))))

  (test-case "R4: encodings at the size boundaries"
    (for ([row (in-list `((#"" "0x80") (#"\0" "0x00") (#"\x7f" "0x7f") (#"\x80" "0x8180")
                          (#"\xff" "0x81ff") (() "0xc0") ((#"") "0xc180")))])
      (match-define (list x expected) row)
      (check-equal? (bytes->hex-string (rlp-encode x)) expected (format "~s" x)))
    (for ([row (in-list '((bytes 55 "b7") (bytes 56 "b838") (bytes 255 "b8ff") (bytes 256 "b90100")
                          (bytes 65535 "b9ffff") (bytes 65536 "ba010000")
                          (list 55 "f7") (list 56 "f838") (list 256 "f90100")))])
      (match-define (list kind n header) row)
      (define x (match kind ['bytes (repeat n #x61)] ['list (make-list n #"\1")]))
      (define e (rlp-encode x))
      (check-equal? (bytes->hex-string (subbytes e 0 (quotient (string-length header) 2)))
                    (string-append "0x" header)
                    (format "~a ~a" kind n))
      (check-equal? (bytes-length e) (+ n (quotient (string-length header) 2)))))

  (test-case "R5: results share no storage with arguments"
    (define bs (hex "c88363617483646f67"))
    (define decoded (rlp-decode bs))
    (bytes-fill! bs 0)
    (check-equal? decoded '(#"cat" #"dog"))
    (define item (bytes 5))
    (define encoded (rlp-encode item))
    (bytes-set! encoded 0 9)
    (check-equal? item #"\5")
    (check-false (immutable? (rlp-encode #"\x05")))
    (check-false (immutable? (rlp-decode #"\x05"))))

  (test-case "Q1: rlp-item? holds for byte strings and lists of items, and nothing else"
    (for ([v (in-list (list #"" #"abc" (bytes 1 2) '() '(#"" (#"a" ())) '(() (()))))])
      (check-true (rlp-item? v) (format "~s" v)))
    (for ([v (in-list (list "a" 'a 5 1.0 (vector #"") (cons #"" #"") '(((1))) (box #"")
                            '(#"" . #"") (list #"" "a")))])
      (check-false (rlp-item? v) (format "~s" v))))

  (test-case "§2.5: the messages"
    (for ([row (in-list
                '((rlp-decode "0x" "rlp-decode: input too short: 1 byte needed, 0 bytes left\n  offset: 0")
                  (rlp-decode "0xb900"
                              "rlp-decode: input too short: 3 bytes needed, 2 bytes left\n  offset: 0")
                  (rlp-decode "0x8100"
                              "rlp-decode: non-canonical single byte: 0x00 is its own encoding\n  offset: 0")
                  (rlp-decode "0xb801ff"
                              "rlp-decode: non-canonical size: a length of 1 takes the short form\n  offset: 0")
                  (rlp-decode "0xb800" "rlp-decode: leading zero: in the length of the item\n  offset: 0")
                  (rlp-decode "0xc0c0" "rlp-decode: trailing bytes: 1 byte after the item\n  offset: 1")
                  (rlp-scalar->natural #"\0\4"
                                       "rlp-scalar->natural: leading zero: a scalar of 2 bytes starts with 0x00")
                  (rlp-scalar->natural () "rlp-scalar->natural: unexpected list: a scalar is a byte string")))])
      (define e
        (match row
          [(list 'rlp-decode in _) (rlp-fault (lambda () (rlp-decode (hex-string->bytes in))))]
          [(list 'rlp-scalar->natural in _) (rlp-fault (lambda () (rlp-scalar->natural in)))]))
      (check-pred exn:fail:web3:rlp? e (format "~s" row))
      (when e
        (check-equal? (exn-message e) (last row)))))

  (test-case "S1: a byte below #x80 with a prefix is refused"
    (for ([b (in-range 256)])
      (if (< b #x80)
          (check-decode-fault (bytes #x81 b) 'single-byte 0
                              (format (string-append "rlp-decode: non-canonical single byte: 0x~a~a"
                                                     " is its own encoding\n  offset: 0")
                                      (if (< b 16) "0" "") (number->string b 16)))
          (check-equal? (rlp-decode (bytes #x81 b)) (bytes b)))))

  (define size-inputs
    (for*/list ([n (in-range 1 56)]
                [row (in-list `((#xb8 #x61) (#xf8 #x01)))])
      (match-define (list prefix filler) row)
      (bytes-append (bytes prefix n) (repeat n filler))))

  (test-case "S2: a length below 56 in the long form is refused"
    (for ([bs (in-list size-inputs)])
      (check-decode-fault bs 'size 0))
    (check-equal? (rlp-decode (bytes-append (hex "f838") (repeat 56 1))) (make-list 56 #"\1")))

  (define leading-zero-inputs
    (append (list (hex "b800") (hex "f800"))
            (for*/list ([prefix (in-list (append (range #xb9 #xc0) (range #xf9 #x100)))]
                        [payload (in-list (list #"" (repeat 56 1)))])
              (define k (- prefix (if (< prefix #xc0) #xb7 #xf7)))
              (bytes-append (bytes prefix) (natural->bytes 56 k) payload))))

  (test-case "S3: a length with a leading zero is refused before its size or payload is read"
    (for ([bs (in-list leading-zero-inputs)])
      (check-decode-fault bs 'leading-zero 0)))

  (test-case "S4: every proper prefix of an encoding is too short"
    (for ([e (in-list encodings)]
          [i (in-naturals)])
      (check-equal? (for/list ([n (in-range (bytes-length e))]
                               #:unless (decode-fault? (subbytes e 0 n) 'short 0))
                      n)
                    '()
                    (format "item ~a" i))))

  (define overrun-inputs
    (for/list ([e (in-list encodings)]
               #:when (<= 2 (bytes-length e) 56))
      (bytes-append (bytes (+ #xc0 (sub1 (bytes-length e)))) e)))

  (test-case "S5: an item that runs past its list's payload is too short"
    (for ([bs (in-list overrun-inputs)])
      (check-decode-fault bs 'short 1)))

  (define trailing-inputs
    (let ([prng (seeded 8 1 2 1 2 3)])
      (for/list ([e (in-list encodings)])
        (bytes-append e (random-bytes prng (add1 (random 8 prng)))))))

  (test-case "S6: bytes after the item are refused at the first of them"
    (for ([bs (in-list trailing-inputs)]
          [e (in-list encodings)])
      (define t (- (bytes-length bs) (bytes-length e)))
      (check-decode-fault bs 'trailing (bytes-length e)
                          (format "rlp-decode: trailing bytes: ~a byte~a after the item\n  offset: ~a"
                                  t (if (= t 1) "" "s") (bytes-length e)))))

  (define random-rlp
    (hex-string->bytes (hash-ref (hash-ref (rlp-vectors "invalidRLPTest.json") 'randomRLP) 'out)))

  (define precedence
    `(("b900" short 0 "3 bytes needed, 2 bytes left")
      ("b90000" leading-zero 0)
      ("c5010203" short 0)
      ("c18100" short 1 "2 bytes needed, 1 byte left")
      ("c28100" single-byte 1)
      ("c2c18100" short 2)
      (,random-rlp leading-zero 4)
      ("c0c0" trailing 1)))

  (define (precedence-input in)
    (if (bytes? in) in (hex in)))

  (test-case "S7: the first fault in reading order is the one reported"
    (for ([row (in-list precedence)])
      (match row
        [(list in reason offset detail)
         (check-decode-fault (precedence-input in) reason offset
                             (format "rlp-decode: ~a: ~a\n  offset: ~a"
                                     (hash-ref phrases reason) detail offset))]
        [(list in reason offset) (check-decode-fault (precedence-input in) reason offset)])))

  (define special-naturals
    (list 0 1 127 128 255 256 (sub1 (expt 2 64)) (expt 2 64) (sub1 (expt 2 256)) (expt 2 256)))

  (define random-naturals
    (let ([prng (seeded 4 4 1 1 2 3)])
      (for/list ([_ (in-range 200)])
        (random-natural prng (random 257 prng)))))

  (test-case "N1: scalars read back as the integers they write"
    (for ([n (in-list (append special-naturals random-naturals))])
      (check-equal? (rlp-scalar->natural (natural->rlp-scalar n)) n)
      (check-equal? (rlp-scalar->natural (rlp-decode (rlp-encode (natural->rlp-scalar n)))) n)))

  (test-case "N2: scalars are minimal"
    (check-equal? (natural->rlp-scalar 0) #"")
    (for ([n (in-list (append special-naturals random-naturals))]
          #:when (positive? n))
      (define bs (natural->rlp-scalar n))
      (check-equal? (bytes-length bs) (quotient (+ (integer-length n) 7) 8) (format "~a" n))
      (check-not-equal? (bytes-ref bs 0) 0 (format "~a" n))
      (check-equal? (bytes->natural bs) n)))

  (test-case "N3: a scalar with a leading zero, or a list, is refused"
    (define prng (seeded 3 3 1 1 2 3))
    (for ([n (in-sequences (in-range 1 41) (for/list ([_ (in-range 100)]) (add1 (random 300 prng))))])
      (define bs (bytes-append #"\0" (random-bytes prng (sub1 n))))
      (define e (rlp-fault (lambda () (rlp-scalar->natural bs))))
      (check-pred exn:fail:web3:rlp? e)
      (when e
        (check-equal? (exn:fail:web3:rlp-reason e) 'leading-zero)
        (check-false (exn:fail:web3:rlp-offset e))
        (check-equal? (exn-message e)
                      (format "rlp-scalar->natural: leading zero: a scalar of ~a byte~a starts with 0x00"
                              n (if (= n 1) "" "s")))))
    (for ([item (in-list '(() (#"") (()) (#"\1" (#"\2"))))])
      (define e (rlp-fault (lambda () (rlp-scalar->natural item))))
      (check-pred exn:fail:web3:rlp? e (format "~s" item))
      (when e
        (check-equal? (exn:fail:web3:rlp-reason e) 'list)
        (check-false (exn:fail:web3:rlp-offset e))))
    (check-equal? (rlp-scalar->natural #"") 0))

  (test-case "B1: arguments outside the contracts blame the caller"
    (check-blames-here (rlp-encode "cat"))
    (check-blames-here (rlp-encode 5))
    (check-blames-here (rlp-encode (vector #"")))
    (check-blames-here (rlp-encode (cons #"" #"")))
    (check-blames-here (rlp-encode (list #"" 'a)))
    (check-blames-here (rlp-encode (list (list "x"))))
    (check-blames-here (rlp-decode "0xc0"))
    (check-blames-here (rlp-decode (list #"")))
    (check-blames-here (natural->rlp-scalar -1))
    (check-blames-here (natural->rlp-scalar 1.0))
    (check-blames-here (natural->rlp-scalar "1"))
    (check-blames-here (rlp-scalar->natural 5))
    (check-blames-here (rlp-scalar->natural "0x01")))

  (test-case "B1: a wrong arity or an unknown keyword is a contract error naming the procedure"
    (check-exn (lambda (e) (and (exn:fail:contract:arity? e)
                                (regexp-match? #rx"^rlp-encode: arity mismatch" (exn-message e))))
               (lambda () (rlp-encode)))
    (check-exn (lambda (e) (and (exn:fail:contract:arity? e)
                                (regexp-match? #rx"^rlp-decode: arity mismatch" (exn-message e))))
               (lambda () (rlp-decode #"" #"")))
    (check-exn (lambda (e) (and (exn:fail:contract? e)
                                (regexp-match? #rx"rlp-decode" (exn-message e))))
               (lambda () (rlp-decode #"\x80" #:strict? #f))))

  (define fuzz-corpus
    (let ([prng (seeded 9 1 2 1 2 3)])
      (append (for/list ([b (in-range 256)]) (bytes #x81 b))
              size-inputs
              leading-zero-inputs
              (for*/list ([e (in-list encodings)]
                          #:when (<= (bytes-length e) 300)
                          [n (in-range (bytes-length e))])
                (subbytes e 0 n))
              overrun-inputs
              trailing-inputs
              (map (lambda (row) (precedence-input (first row))) precedence)
              (for/list ([(_ v) (in-hash (rlp-vectors "invalidRLPTest.json"))])
                (hex-string->bytes (hash-ref v 'out)))
              (for/list ([_ (in-range 10000)]) (random-input prng))
              mutated)))

  (define (message-pattern reason offset)
    (pregexp (format "^rlp-decode: ~a: [^\n]*\n  offset: ~a$" (hash-ref phrases reason) offset)))

  (test-case "B2, B3: any input decodes in bounds, or raises an rlp error with a short message"
    (for ([bs (in-list fuzz-corpus)])
      (define name (input-name bs))
      (match (call-bounded (lambda () (rlp-decode bs)))
        [(list 'ok item) (check-equal? (rlp-encode item) bs name)]
        [(list 'raised (? exn:fail:web3:rlp? e))
         (define reason (exn:fail:web3:rlp-reason e))
         (define offset (exn:fail:web3:rlp-offset e))
         (check-pred reason? reason name)
         (check-not-eq? reason 'list name)
         (check-true (if (zero? (bytes-length bs)) (eqv? offset 0) (< offset (bytes-length bs))) name)
         (check-regexp-match (message-pattern reason offset) (exn-message e))
         (check-true (< (string-length (exn-message e)) 200) name)
         (check-pred exn:fail:web3? e)]
        [other (fail (format "~a: ~e" name other))])))

  (test-case "B4: the internal entry points raise under the caller's name"
    (check-regexp-match #rx"^bytes->transaction: trailing bytes"
                        (exn-message (rlp-fault (lambda ()
                                                  (rlp-decode/who 'bytes->transaction (bytes #xc0 0))))))
    (check-regexp-match #rx"^f: leading zero"
                        (exn-message (rlp-fault (lambda () (rlp-scalar->natural/who 'f #"\0"))))))

  (define deep (deep-item 100000))

  (test-case "H1: nesting is not limited"
    (match (call-bounded (lambda () (rlp-decode (rlp-encode deep))) #:seconds 10 #:megabytes 128)
      [(list 'ok item) (check-equal? item deep)]
      [other (fail (format "~e" other))]))

  (test-case "H2: a fault at the bottom of deep nesting is reported where it is"
    (define bs (rlp-encode deep))
    (define at (sub1 (bytes-length bs)))
    (bytes-set! bs at #x81)
    (match (call-bounded (lambda () (rlp-decode bs)) #:seconds 10 #:megabytes 128)
      [(list 'raised (? exn:fail:web3:rlp? e))
       (check-equal? (exn:fail:web3:rlp-reason e) 'short)
       (check-equal? (exn:fail:web3:rlp-offset e) at)
       (check-equal? (exn-message e)
                     (format "rlp-decode: input too short: 2 bytes needed, 1 byte left\n  offset: ~a"
                             at))]
      [other (fail (format "~e" other))]))

  (test-case "H3: a claimed length is checked before anything is allocated for it"
    (for* ([prefix (in-list (append (range #xb8 #xc0) (range #xf8 #x100)))]
           [n (in-range 17)])
      (define k (- prefix (if (< prefix #xc0) #xb7 #xf7)))
      (define bs (bytes-append (bytes prefix) (repeat k #xff) (repeat n 1)))
      (match (call-bounded (lambda () (rlp-decode bs)))
        [(list 'raised (? exn:fail:web3:rlp? e))
         (check-equal? (exn:fail:web3:rlp-reason e) 'short (bytes->hex-string bs))
         (check-equal? (exn:fail:web3:rlp-offset e) 0 (bytes->hex-string bs))]
        [other (fail (format "~a: ~e" (bytes->hex-string bs) other))])))

  (test-case "E1: web3/rlp needs no network, JSON, hashing or signing modules"
    (define names (filter values (map module-name (import-closure 'web3/rlp))))
    (check-equal? (filter (lambda (n) (member (collection n) '("net" "json" "keccak" "libsecp256k1")))
                          names)
                  '())
    (check-equal? (sort (filter (lambda (n) (equal? (collection n) "web3")) names) string<?)
                  '("web3/private/exn" "web3/private/hex" "web3/private/who" "web3/rlp")))

  (test-case "A3: create-address hashes the RLP list of sender and nonce"
    (define prng (seeded 5 1 5 1 2 3))
    (for* ([sender (in-list (for/list ([_ (in-range 3)]) (random-address prng)))]
           [nonce (in-sequences (in-range 301) (list (- (expt 2 64) 2)))])
      (define preimage (rlp-encode (list (hex-string->bytes sender) (natural->rlp-scalar nonce))))
      (check-equal? (address->checksum-string (bytes->hex-string (subbytes (keccak256 preimage) 12)))
                    (create-address sender nonce)
                    (format "~a ~a" sender nonce)))))
