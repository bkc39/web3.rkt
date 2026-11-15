#lang racket/base

(module+ test
  (require racket/list
           racket/match
           rackunit
           syntax/parse/define
           web3/abi
           (only-in web3/rpc bytes->hex-string hex-string->bytes)
           "abi-gen.rkt"
           "util.rkt")

  (define (value-error thunk)
    (with-handlers ([exn:fail:web3:abi:value? values])
      (thunk)
      #f))

  (define-syntax-parse-rule (check-value-error expr type value path)
    (let ([e (value-error (lambda () expr))])
      (check-pred exn:fail:web3:abi:value? e (format "~s" 'expr))
      (when e
        (check-equal? (exn:fail:web3:abi:value-type e) type (format "~s" 'expr))
        (check-equal? (exn:fail:web3:abi:value-value e) value (format "~s" 'expr))
        (check-equal? (exn:fail:web3:abi:value-path e) path (format "~s" 'expr)))))

  (define (word n) (word->bytes n))
  (define (encode1 t v) (abi-encode (list t) (list v)))
  (define (packed1 t v) (abi-encode-packed (list t) (list v)))

  (test-case "static encodings have the static size"
    (define prng (seeded 7 7 7 7 7 7))
    (for ([_ (in-range 300)])
      (define t (random-abi-type prng #:static? #t))
      (define v (random-abi-value prng t))
      (define size (abi-type-static-size t))
      (check-equal? (bytes-length (encode1 t v)) size (abi-type->string t))
      (check-true (and (positive? size) (zero? (remainder size 32))))))

  (test-case "elementary values encode as one word"
    (for ([n (in-list widths)])
      (for ([v (in-list (list 0 1 (sub1 (expt 2 n)) (quotient (expt 2 n) 3)))])
        (check-equal? (encode1 (abi-uint n) v) (word v)))
      (for ([v (in-list (list 0 1 -1 (- (expt 2 (sub1 n))) (sub1 (expt 2 (sub1 n)))))])
        (check-equal? (encode1 (abi-int n) v) (word (modulo v (expt 2 256))))))
    (check-equal? (encode1 'bool #t) (word 1))
    (check-equal? (encode1 'bool #f) (word 0))
    (define spellings
      '("0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"
        "0xd8da6bf26964af9d7eed9e03e53415d37aa96045"
        "0xD8DA6BF26964AF9D7EED9E03E53415D37AA96045"))
    (for ([a (in-list spellings)])
      (check-equal? (encode1 'address a) (word (string->number (substring a 2) 16))))
    (for ([n (in-range 1 33)])
      (define b (make-bytes n 171))
      (check-equal? (encode1 (abi-fixed-bytes n) b) (bytes-append b (make-bytes (- 32 n) 0)))))

  (test-case "values at and past their type's bounds"
    (for ([n (in-list widths)])
      (define u (abi-uint n))
      (define i (abi-int n))
      (define lo (- (expt 2 (sub1 n))))
      (define hi (sub1 (expt 2 (sub1 n))))
      (check-equal? (encode1 u 0) (word 0))
      (check-equal? (encode1 u (sub1 (expt 2 n))) (word (sub1 (expt 2 n))))
      (check-value-error (encode1 u -1) u -1 '(0))
      (check-value-error (encode1 u (expt 2 n)) u (expt 2 n) '(0))
      (check-not-exn (lambda () (encode1 i lo)))
      (check-not-exn (lambda () (encode1 i hi)))
      (check-value-error (encode1 i (sub1 lo)) i (sub1 lo) '(0))
      (check-value-error (encode1 i (add1 hi)) i (add1 hi) '(0))
      (define size (quotient n 8))
      (define b (abi-fixed-bytes size))
      (check-value-error (encode1 b (make-bytes (add1 size) 0)) b (make-bytes (add1 size) 0) '(0))
      (when (> size 1)
        (check-value-error (encode1 b (make-bytes (sub1 size) 0)) b (make-bytes (sub1 size) 0) '(0))))
    (for ([row (in-list '(("uint8" 1.0) ("uint8" "1") ("bool" 0) ("address" "0x1234")
                          ("string" #"ab") ("bytes" "ab") ("uint8[]" #(1))))])
      (match-define (list t v) row)
      (check-value-error (encode1 t v) (abi-type t) v '(0))))

  (define (padded b)
    (bytes-append b (make-bytes (modulo (- (bytes-length b)) 32) 0)))

  (test-case "bytes and string are a length and right-padded data"
    (for ([len (in-list '(0 1 31 32 33 64))])
      (define b (make-bytes len 7))
      (define encoded (encode1 'bytes b))
      (check-equal? encoded (bytes-append (word 32) (word len) (padded b)))
      (check-equal? (bytes-length encoded) (+ 64 (* 32 (ceiling (/ len 32))))))
    (define s "héllo €😀")
    (check-equal? (encode1 'string s) (encode1 'bytes (string->bytes/utf-8 s))))

  (test-case "arrays encode as tuples"
    (define prng (seeded 7 7 7 7 7 7))
    (for ([_ (in-range 200)])
      (define t (random-abi-type prng #:max-depth 2))
      (define k (add1 (random 3 prng)))
      (define xs (for/list ([_ (in-range k)]) (random-abi-value prng t)))
      (check-equal? (encode1 (abi-fixed-array t k) xs) (encode1 (abi-tuple (make-list k t)) xs))
      (define ys (take xs (random (add1 k) prng)))
      (check-equal? (encode1 (abi-array t) ys)
                    (bytes-append (word 32) (word (length ys))
                                  (abi-encode (make-list (length ys) t) ys)))))

  (test-case "the argument list encodes as a tuple"
    (define prng (seeded 7 7 7 7 7 7))
    (for ([_ (in-range 200)])
      (define ts (random-abi-types prng))
      (define vs (map (lambda (t) (random-abi-value prng t)) ts))
      (unless (null? ts)
        (define tuple (abi-tuple ts))
        (check-equal? (encode1 tuple vs)
                      (if (abi-type-dynamic? tuple)
                          (bytes-append (word 32) (abi-encode ts vs))
                          (abi-encode ts vs)))))
    (check-equal? (abi-encode '() '()) #""))

  (test-case "value errors name the arity or the path"
    (check-value-error (abi-encode '(uint8 bool) '(1)) (list (abi-uint 8) (abi-bool)) '(1) '())
    (check-value-error (abi-encode '("uint8[2]") '((1 2 3))) (abi-type "uint8[2]") '(1 2 3) '(0))
    (check-value-error (abi-encode '((array (tuple uint8 string))) '(((1 "a") (256 "b"))))
                       (abi-uint 8) 256 '(0 1 0))
    (check-value-error (encode-call "add(uint256,uint256)" 1)
                       (list (abi-uint 256) (abi-uint 256)) '(1) '())
    (check-exn #rx"^abi-encode: " (lambda () (abi-encode '(uint8) '(256))))
    (check-exn #rx"^encode-call: " (lambda () (encode-call "add(uint256,uint256)" 1)))
    (check-exn #rx"^abi-encode-packed: " (lambda () (abi-encode-packed '(uint8) '(256))))
    (check-exn (regexp (string-append "^abi-encode: out of range: expected an integer from 0 to 255\n"
                                      "  type: uint8\n"
                                      "  value: 256\n"
                                      "  at: argument 0, index 1, index 0$"))
               (lambda () (abi-encode '("(uint8,string)[]") '(((1 "a") (256 "b"))))))
    (check-exn #rx"^encode-call: not an ABI type" (lambda () (encode-call "f(uint7)" 1)))
    (check-exn #rx"^abi-encode: not an ABI type" (lambda () (abi-encode '("uint7") '(1)))))

  (define a-address "0x00000000000000000000000000000000000000aa")

  (test-case "encode-call is the selector and the encoded inputs"
    (for ([row (in-list `(("add(uint256,uint256)" 2 40)
                          ("transfer(address,uint256)" ,a-address 1000)
                          ("submit(string,bytes[])" "hi" (#"" #"\1"))
                          ("f()")))])
      (match-define (cons s vs) row)
      (define sig (string->abi-signature s))
      (define expected
        (bytes-append (function-selector (abi-signature->string sig))
                      (abi-encode (abi-signature-inputs sig) vs)))
      (check-equal? (apply encode-call s vs) expected s)
      (check-equal? (apply encode-call sig vs) expected s)
      (check-equal? (apply encode-call (abi-signature (abi-signature-name sig)
                                                      (abi-signature-inputs sig)
                                                      (list 'bool))
                           vs)
                    expected s))
    (check-equal? (encode-call "balanceOf(address)(uint256)" a-address)
                  (encode-call "balanceOf(address)" a-address))
    (check-equal? (bytes->hex-string (function-selector "transfer(address, uint)")) "0xa9059cbb")
    (check-equal? (function-selector (string->abi-signature "transfer(address,uint)"))
                  (hex-string->bytes "0xa9059cbb"))
    (check-not-equal? (bytes->hex-string (function-selector #"transfer(address, uint)"))
                      "0xa9059cbb")
    (check-equal? (bytes->hex-string (encode-call "add(uint256,uint256)" 2 40))
                  (string-append "0x771602f7" (substring (bytes->hex-string (word 2)) 2)
                                 (substring (bytes->hex-string (word 40)) 2))))

  (define packed-widths
    `((uint8 255 1) (int16 -1 2) (uint256 1 32) (int256 -1 32) (address ,a-address 20)
      (bool #t 1) (bytes1 #"a" 1) (bytes3 #"abc" 3) (bytes32 ,(make-bytes 32 1) 32)))

  (test-case "packed encoding"
    (for ([n (in-list widths)])
      (check-equal? (bytes-length (packed1 (abi-uint n) 1)) (quotient n 8))
      (check-equal? (bytes-length (packed1 (abi-int n) -1)) (quotient n 8)))
    (for ([row (in-list packed-widths)])
      (match-define (list t v width) row)
      (check-equal? (bytes-length (packed1 t v)) width (format "~a" t)))
    (check-equal? (packed1 'int16 -1) #"\377\377")
    (check-equal? (packed1 'uint16 1) #"\0\1")
    (check-equal? (packed1 'address a-address) (bytes-append (make-bytes 19 0) #"\252"))
    (define ts (map first packed-widths))
    (define vs (map second packed-widths))
    (check-equal? (abi-encode-packed ts vs) (apply bytes-append (map packed1 ts vs)))
    (for ([row (in-list `((uint256 12345) (int256 -5) (bytes32 ,(make-bytes 32 9))))])
      (match-define (list t v) row)
      (check-equal? (packed1 t v) (encode1 t v)))
    (check-equal? (packed1 'bytes #"\1\2\3") #"\1\2\3")
    (check-equal? (packed1 'string "héllo") (string->bytes/utf-8 "héllo"))
    (check-equal? (packed1 'string "hi") #"hi")
    (check-equal? (packed1 "int8[]" '(-1)) (make-bytes 32 255))
    (check-equal? (packed1 "uint16[2]" '(1 2)) (bytes-append (word 1) (word 2)))
    (check-equal? (packed1 "bytes2[]" '(#"ab")) (encode1 'bytes2 #"ab"))
    (check-equal? (packed1 "bool[]" '()) #"")
    (check-equal? (bytes->hex-string (abi-encode-packed '(uint16 bytes1 string) (list 1 #"A" "hi")))
                  "0x0001416869")
    (for ([row (in-list '(("(uint8,uint8)" (1 2)) ("uint8[][]" ((1))) ("string[]" ("a"))
                          ("bytes[2]" (#"a" #"b")) ("(uint8)[]" ((1)))))])
      (match-define (list t v) row)
      (check-exn (lambda (e)
                   (and (exn:fail:web3:abi:type? e)
                        (regexp-match? #rx"^abi-encode-packed: not supported in packed encoding"
                                       (exn-message e))))
                 (lambda () (packed1 t v))
                 t)))

  (test-case "encoders blame their caller"
    (check-blames-here (abi-encode "uint256" '(1)))
    (check-blames-here (abi-encode '(uint256) 1))
    (check-blames-here (abi-encode-packed '(42) '(1)))
    (check-blames-here (encode-call 'transfer 1))
    (check-blames-here (function-selector 42))
    (check-blames-here (word->bytes (expt 2 256)))
    (check-blames-here (word->bytes -1))
    (check-equal? (word->bytes (sub1 (expt 2 256))) (make-bytes 32 255)))

  (test-case "function-selector reads a string as a signature"
    (check-exn (lambda (e)
                 (and (exn:fail:web3:abi:type? e)
                      (regexp-match? #rx"^function-selector: " (exn-message e))))
               (lambda () (function-selector "foo"))))

  (test-case "doc examples"
    (check-equal? (bytes->hex-string (abi-encode '(int8 bytes3) (list -1 #"abc")))
                  (bytes->hex-string (bytes-append (make-bytes 32 255) #"abc" (make-bytes 29 0))))
    (check-equal? (abi-type->string "tuple(uint, int)[]") "(uint256,int256)[]")
    (check-equal? (abi-type->sexp "uint8[2][]") '(array (array uint8 2)))
    (check-equal? (abi-type-static-size "(uint256,bool)[3]") 192)
    (check-equal? (bytes->hex-string (subbytes (encode-call "add(uint256,uint256)" 2 40) 0 4))
                  "0x771602f7")))
