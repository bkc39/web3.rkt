#lang racket/base

(module+ test
  (require racket/list
           racket/match
           (only-in racket/string string-contains?)
           rackunit
           web3/abi
           (submod web3/abi internal)
           web3/abi/vectors
           (only-in web3/private/abi-decode raise-abi-decode-error)
           (only-in web3/rpc exn:fail:web3? hex-string->bytes)
           "abi-gen.rkt"
           "bounded.rkt"
           "util.rkt")

  (define (padded bs) (bytes-append bs (make-bytes (modulo (- (bytes-length bs)) 32) 0)))
  (define (words . ns) (apply bytes-append (map word ns)))
  (define alice-number (string->number (substring alice 2) 16))

  (define (random-case prng)
    (define ts (random-abi-types prng))
    (values ts (for/list ([t (in-list ts)]) (random-abi-value prng t))))

  (test-case "decoding inverts encoding, with addresses checksummed"
    (define prng (seeded 1 1 2 3 5 8))
    (for ([_ (in-range 500)])
      (define-values (ts vs) (random-case prng))
      (define data (abi-encode ts vs))
      (define decoded (abi-decode ts data))
      (define name (format "~s ~s" (map abi-type->string ts) vs))
      (check-equal? decoded (checksummed ts vs) name)
      (check-equal? (downcased ts decoded) (downcased ts vs) name)
      (check-decoded ts data decoded name)
      (define extra (random-bytes prng (add1 (random 64 prng))))
      (check-equal? (abi-decode ts (bytes-append data extra)) decoded name)))

  (test-case "static data shorter than its head is short at 0"
    (define prng (seeded 2 7 1 8 2 8))
    (for ([_ (in-range 200)])
      (define ts (for/list ([_ (in-range (add1 (random 4 prng)))])
                   (random-abi-type prng #:static? #t)))
      (define vs (for/list ([t (in-list ts)]) (random-abi-value prng t)))
      (define data (abi-encode ts vs))
      (define strings (map abi-type->string ts))
      (for ([m (in-range (bytes-length data))])
        (check-decode-error (abi-decode ts (subbytes data 0 m)) 'abi-decode 'short 0
                            #:type strings))))

  (test-case "truncated dynamic data is rejected or loses only zero bytes"
    (define prng (seeded 3 1 4 1 5 9))
    (define cases
      (for*/fold ([cases '()])
                 ([_ (in-naturals)]
                  #:break (= (length cases) 200))
        (define-values (ts vs) (random-case prng))
        (if (ormap abi-type-dynamic? ts) (cons (cons ts vs) cases) cases)))
    (for ([c (in-list cases)])
      (match-define (cons ts vs) c)
      (define data (abi-encode ts vs))
      (for ([_ (in-range 20)])
        (define m (random (bytes-length data) prng))
        (define name (format "~s ~s cut at ~a" (map abi-type->string ts) vs m))
        (match (call-bounded (lambda () (abi-decode ts (subbytes data 0 m))))
          [(list 'ok decoded)
           (check-equal? decoded (checksummed ts vs) name)
           (check-equal? (subbytes data m) (make-bytes (- (bytes-length data) m) 0) name)]
          [(list 'raised (? exn:fail:web3:abi:decode? e))
           (check-not-false (memq (exn:fail:web3:abi:decode-reason e) '(short offset length)) name)]
          [other (fail (format "~a: ~e" name other))]))))

  (test-case "each word decodes only from its canonical form"
    (for ([n (in-list widths)])
      (define u (abi-uint n))
      (define i (abi-int n))
      (define (decode1 t data) (car (abi-decode (list t) data)))
      (for ([v (in-list (list 0 1 (sub1 (expt 2 n))))])
        (check-equal? (decode1 u (word v)) v))
      (for ([v (in-list (list (- (expt 2 (sub1 n))) -1 0 (sub1 (expt 2 (sub1 n)))))])
        (check-equal? (decode1 i (word (modulo v (expt 2 256)))) v))
      (when (< n 256)
        (for ([dirty (in-list (list (expt 2 n) max-word))])
          (check-decode-error (abi-decode (list u) (word dirty)) 'abi-decode 'non-canonical 0
                              #:type (abi-type->string u)))
        (for ([dirty (in-list (list (expt 2 (sub1 n)) (- (expt 2 256) (expt 2 (sub1 n)) 1)))])
          (check-decode-error (abi-decode (list i) (word dirty)) 'abi-decode 'non-canonical 0
                              #:type (abi-type->string i)))))
    (check-equal? (abi-decode '(bool bool) (words 0 1)) '(#f #t))
    (for ([dirty (in-list (list 2 (expt 2 255)))])
      (check-decode-error (abi-decode '(bool) (word dirty)) 'abi-decode 'non-canonical 0 #:type "bool"))
    (check-equal? (abi-decode '(address) (word alice-number)) (list alice-checksummed))
    (for ([dirty (in-list (list (+ (expt 2 160) alice-number) (expt 2 255)))])
      (check-decode-error (abi-decode '(address) (word dirty)) 'abi-decode 'non-canonical 0
                          #:type "address"))
    (for ([n (in-range 1 33)])
      (define t (abi-fixed-bytes n))
      (define b (make-bytes n 171))
      (check-equal? (abi-decode (list t) (padded b)) (list b))
      (when (< n 32)
        (define dirty (padded b))
        (bytes-set! dirty n 1)
        (check-decode-error (abi-decode (list t) dirty) 'abi-decode 'non-canonical 0
                            #:type (abi-type->string t))))
    (check-equal? (abi-decode '(bytes32) (make-bytes 32 255)) (list (make-bytes 32 255))))

  (define (nested type suffix depth)
    (for/fold ([t type]) ([_ (in-range depth)]) (string-append t suffix)))

  (define (nested-value depth)
    (for/fold ([v 7]) ([_ (in-range depth)]) (list v)))

  (define hostile
    `((#:name "bomb-shared-inner" #:types ("uint256[][]")
       #:data ,(bytes-append (words 32 4096) (apply words (make-list 4096 131072))
                             (word 4096) (apply words (make-list 4096 7)))
       #:reason overlap #:offset 96 #:type "uint256[]")
      (#:name "bomb-shared-string" #:types ("string[]")
       #:data ,(bytes-append (words 32 4096) (apply words (make-list 4096 131072))
                             (word 131072) (make-bytes 131072 97))
       #:reason overlap #:offset 96 #:type "string")
      (#:name "huge-bytes" #:types ("bytes") #:data ,(words 32 max-word)
       #:reason length #:offset 32 #:type "bytes")
      (#:name "huge-array" #:types ("uint256[]") #:data ,(words 32 (expt 2 255))
       #:reason length #:offset 32 #:type "uint256[]")
      (#:name "huge-static-type" #:types ("uint256[100000000]") #:data ,(word 1)
       #:reason short #:offset 0 #:type ("uint256[100000000]"))))

  (define (check-rejects-within-bounds name types data reason offset type)
    (match (call-bounded (lambda () (abi-decode types data)))
      [(list 'raised e) (check-decode-error (raise e) 'abi-decode reason offset #:type type)]
      [other (fail (format "~a: ~e" name other))]))

  (test-case "hostile inputs are rejected within bounds"
    (for ([v (in-list abi-reject-vectors)])
      (match-define (abi-reject-vector name signature hex reason offset type) v)
      (check-rejects-within-bounds name (signature-inputs signature) (hex-string->bytes hex)
                                   reason offset type))
    (for ([row (in-list hostile)])
      (match-define (list #:name name #:types types #:data data #:reason reason #:offset offset
                          #:type type)
        row)
      (check-rejects-within-bounds name types data reason offset type)))

  (test-case "deeply nested types encode and decode in time linear in their depth"
    (for ([suffix (in-list '("[]" "[1]"))])
      (define deep (list (nested "uint256" suffix 1000)))
      (define deep-value (list (nested-value 1000)))
      (check-equal? (call-bounded (lambda () (abi-decode deep (abi-encode deep deep-value)))
                                  #:seconds 1)
                    (list 'ok deep-value)
                    suffix)))

  (test-case "a decode error's reason is one the decoder knows"
    (check-blames-here (raise-abi-decode-error 'f 'shrot '() 0))
    (check-decode-error (raise-abi-decode-error 'f 'short '() 0) 'f 'short 0 #:type '()))

  (test-case "decode-call and decode-result invert encode-call and abi-encode"
    (define prng (seeded 4 4 4 4 4 4))
    (for ([_ (in-range 100)])
      (define-values (ts vs) (random-case prng))
      (define-values (outs out-vs) (random-case prng))
      (define with-outputs (signature-string "g" ts outs))
      (define data (apply encode-call (signature-string "g" ts) vs))
      (define name (format "~a ~s" with-outputs vs))
      (for ([sig (in-list (list (signature-string "g" ts) with-outputs (string->abi-signature with-outputs)))])
        (check-equal? (decode-call sig data) (checksummed ts vs) name))
      (check-decoded ts (subbytes data 4) (decode-call with-outputs data) name)
      (define result (abi-encode outs out-vs))
      (for ([sig (in-list (list with-outputs (string->abi-signature with-outputs)))])
        (check-equal? (decode-result sig result) (checksummed outs out-vs) name))
      (check-decoded outs result (decode-result with-outputs result) name)
      (define flipped (bytes-copy data))
      (bytes-set! flipped 2 (bitwise-xor (bytes-ref flipped 2) 1))
      (check-decode-error (decode-call with-outputs flipped) 'decode-call 'selector 0
                          #:type (map abi-type->string ts)))
    (for ([n (in-list '(0 1 3))])
      (check-decode-error (decode-call "f(uint256)" (make-bytes n 0)) 'decode-call 'selector 0
                          #:type '("uint256")))
    (check-decode-error (decode-call "f(string)"
                                     (bytes-append (function-selector "f(string)") (word (expt 2 64))))
                        'decode-call 'offset 4 #:type "string")
    (check-exn exn:fail:web3:abi:type? (lambda () (decode-result "f(uint256)" #"")))
    (check-exn #rx"^decode-result: signature has no outputs; write name[(][.][.][.][)][(][.][.][.][)]: \"f[(]uint256[)]\""
               (lambda () (decode-result "f(uint256)" #"")))
    (check-equal? (decode-result "f()()" #"\1\2") '())
    (check-equal? (abi-decode '() #"\1") '()))

  (define messages
    `((abi-decode ("uint256") ,(make-bytes 31 0) "data too short" "(uint256)" 0)
      (abi-decode ("string") ,(word 64) "offset out of range" "string" 0)
      (abi-decode ("string" "string") ,(bytes-append (words 64 64 1) (padded #"a")) "overlapping tail" "string" 32)
      (abi-decode ("bytes") ,(words 32 max-word) "length out of range" "bytes" 32)
      (abi-decode ("uint8") ,(word 256) "not canonically encoded" "uint8" 0)
      (abi-decode ("string") ,(bytes-append (words 32 1) (padded #"\377")) "invalid UTF-8" "string" 64)
      (decode-call ("uint256") #"\1\2" "selector mismatch" "(uint256)" 0)))

  (test-case "messages name the caller, the reason, the type and the offset"
    (for ([row (in-list messages)])
      (match-define (list who types data phrase type offset) row)
      (define e
        (decode-error (lambda ()
                        (match who
                          ['abi-decode (abi-decode types data)]
                          ['decode-call (decode-call (signature-string "f" types) data)]))))
      (check-pred exn:fail:web3:abi:decode? e phrase)
      (define message (exn-message e))
      (check-regexp-match (regexp (format "^~a: ~a" who (regexp-quote phrase))) message)
      (check-true (string-contains? message (format "\n  type: ~a" type)) message)
      (check-true (string-contains? message (format "\n  offset: ~a" offset)) message))
    (check-regexp-match #rx"^abi-decode: length out of range: 115792089237316195423570985008687907853269984665640564039457584007913129639935 bytes claimed, 0 remain\n  type: bytes\n  offset: 32$"
                        (exn-message (decode-error (lambda ()
                                                     (abi-decode '(bytes) (words 32 max-word))))))
    (check-regexp-match #rx"^decode-revert: "
                        (exn-message (decode-error (lambda ()
                                                     (decode-revert (bytes-append (error-selector "Error(string)")
                                                                                  (word (expt 2 64)))))))))

  (test-case "decode errors are ABI errors and web3 errors"
    (define e (decode-error (lambda () (abi-decode '(uint8 string) (word 1)))))
    (check-true (exn:fail:web3:abi? e))
    (check-true (exn:fail:web3? e))
    (check-true (andmap abi-type? (exn:fail:web3:abi:decode-type e)))
    (check-true (exact-nonnegative-integer? (exn:fail:web3:abi:decode-offset e)))
    (check-not-false (memq (exn:fail:web3:abi:decode-reason e)
                           '(short offset overlap length non-canonical utf-8 selector)))
    (check-pred abi-type? (exn:fail:web3:abi:decode-type
                           (decode-error (lambda () (abi-decode '(bool) (word 2))))))
    (check-exn exn:fail:web3:abi:type? (lambda () (abi-decode '("uint7") #""))))

  (test-case "decoders blame their caller"
    (check-blames-here (abi-decode "uint256" #""))
    (check-blames-here (abi-decode '(uint256) "0x00"))
    (check-blames-here (abi-decode '(42) #""))
    (check-blames-here (decode-result 'f #""))
    (check-blames-here (decode-call "f()" "0x"))
    (check-blames-here (abi-revert->string 'x)))

  (test-case "the internal decoders raise under their caller's name"
    (define data (abi-encode '(uint256 string) '(5 "hi")))
    (define prefixed (bytes-append (make-bytes 7 9) data))
    (check-equal? (abi-decode/who 'decode-log '(uint256 string) prefixed 7) '(5 "hi"))
    (check-equal? (abi-decode/who 'decode-log (list (abi-uint 256)) (word 5) 0) '(5))
    (check-decode-error (abi-decode/who 'decode-log '(uint8) (bytes-append (make-bytes 64) (word 256)) 64)
                        'decode-log 'non-canonical 64 #:type "uint8")
    (check-decode-error (abi-decode/who 'decode-log '(string) (bytes-append (make-bytes 7) (word 0)) 7)
                        'decode-log 'overlap 7 #:type "string")
    (check-decode-error (abi-decode/who 'decode-log '(uint256) (make-bytes 40) 9)
                        'decode-log 'short 9 #:type '("uint256"))
    (check-equal? (decode-result/who 'contract-call "f()(uint256)" (word 7)) '(7))
    (check-decode-error (decode-result/who 'contract-call "f()(bool)" (word 2))
                        'contract-call 'non-canonical 0 #:type "bool")
    (check-exn #rx"^contract-call: signature has no outputs"
               (lambda () (decode-result/who 'contract-call "f()" #"")))))
