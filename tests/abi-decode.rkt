#lang racket/base

(module+ test
  (require racket/list
           racket/match
           (only-in racket/string string-contains?)
           rackunit
           web3/abi
           (submod web3/abi internal)
           web3/abi/vectors
           (only-in web3/rpc exn:fail:web3? hex-string->bytes)
           "abi-gen.rkt"
           "bounded.rkt"
           "util.rkt")

  (define (w n) (word->bytes n))
  (define (s bs) (bytes-append bs (make-bytes (modulo (- (bytes-length bs)) 32) 0)))
  (define (words . ns) (apply bytes-append (map w ns)))
  (define (seeded . seed) (vector->pseudo-random-generator (list->vector seed)))
  (define A "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed")
  (define A* "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed")
  (define A-number (string->number (substring A 2) 16))

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
        (check-equal? (decode1 u (w v)) v))
      (for ([v (in-list (list (- (expt 2 (sub1 n))) -1 0 (sub1 (expt 2 (sub1 n)))))])
        (check-equal? (decode1 i (w (modulo v (expt 2 256)))) v))
      (when (< n 256)
        (for ([word (in-list (list (expt 2 n) (sub1 (expt 2 256))))])
          (check-decode-error (abi-decode (list u) (w word)) 'abi-decode 'non-canonical 0
                              #:type (abi-type->string u)))
        (for ([word (in-list (list (expt 2 (sub1 n)) (- (expt 2 256) (expt 2 (sub1 n)) 1)))])
          (check-decode-error (abi-decode (list i) (w word)) 'abi-decode 'non-canonical 0
                              #:type (abi-type->string i)))))
    (check-equal? (abi-decode '(bool bool) (words 0 1)) '(#f #t))
    (for ([word (in-list (list 2 (expt 2 255)))])
      (check-decode-error (abi-decode '(bool) (w word)) 'abi-decode 'non-canonical 0 #:type "bool"))
    (check-equal? (abi-decode '(address) (w A-number)) (list A*))
    (for ([word (in-list (list (+ (expt 2 160) A-number) (expt 2 255)))])
      (check-decode-error (abi-decode '(address) (w word)) 'abi-decode 'non-canonical 0
                          #:type "address"))
    (for ([n (in-range 1 33)])
      (define t (abi-fixed-bytes n))
      (define b (make-bytes n 171))
      (check-equal? (abi-decode (list t) (s b)) (list b))
      (when (< n 32)
        (define dirty (s b))
        (bytes-set! dirty n 1)
        (check-decode-error (abi-decode (list t) dirty) 'abi-decode 'non-canonical 0
                            #:type (abi-type->string t))))
    (check-equal? (abi-decode '(bytes32) (make-bytes 32 255)) (list (make-bytes 32 255))))

  (define (row-type name)
    (match name
      ["short-static" '("uint256")]
      ["short-head" '("uint256" "string")]
      ["tuple-head-past-end" "(uint256,string)"]
      ["fixed-dynamic-offset" "string[2]"]
      ["length-huge-bytes" "bytes"]
      [(or "length-huge-array" "length-array-off-by-one" "overlap-shared-inner") "uint256[]"]
      [(or "dirty-uint8" "dirty-uint8-ones" "dirty-in-array") "uint8"]
      ["dirty-address" "address"]
      [(or "dirty-bool" "dirty-in-tuple") "bool"]
      ["dirty-bytes1" "bytes1"]
      [(or "dirty-int8-positive" "dirty-int8-sign") "int8"]
      [_ "string"]))

  (define (nested-array depth)
    (for/fold ([t "uint256"]) ([_ (in-range depth)]) (string-append t "[]")))

  (define (nested-value depth)
    (for/fold ([v 7]) ([_ (in-range depth)]) (list v)))

  (define hostile
    `(("bomb-shared-inner" ("uint256[][]")
       ,(bytes-append (words 32 4096) (apply words (make-list 4096 131072))
                      (w 4096) (apply words (make-list 4096 7)))
       overlap 96 "uint256[]")
      ("bomb-shared-string" ("string[]")
       ,(bytes-append (words 32 4096) (apply words (make-list 4096 131072))
                      (w 131072) (make-bytes 131072 97))
       overlap 96 "string")
      ("huge-bytes" ("bytes") ,(words 32 (sub1 (expt 2 256))) length 32 "bytes")
      ("huge-array" ("uint256[]") ,(words 32 (expt 2 255)) length 32 "uint256[]")
      ("huge-static-type" ("uint256[100000000]") ,(w 1) short 0 ("uint256[100000000]"))))

  (define (check-rejects-within-bounds name types data reason offset type)
    (match (call-bounded (lambda () (abi-decode types data)))
      [(list 'raised e) (check-decode-error (raise e) 'abi-decode reason offset #:type type)]
      [other (fail (format "~a: ~e" name other))]))

  (test-case "hostile inputs are rejected within bounds"
    (for ([v (in-list abi-reject-vectors)])
      (match-define (abi-reject-vector name signature hex reason offset) v)
      (check-rejects-within-bounds name (signature-inputs signature) (hex-string->bytes hex)
                                   reason offset (row-type name)))
    (for ([row (in-list hostile)])
      (apply check-rejects-within-bounds row))
    (define deep (list (nested-array 200)))
    (define deep-value (list (nested-value 200)))
    (check-equal? (call-bounded (lambda () (abi-decode deep (abi-encode deep deep-value))))
                  (list 'ok deep-value)))

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
                                     (bytes-append (function-selector "f(string)") (w (expt 2 64))))
                        'decode-call 'offset 4 #:type "string")
    (check-exn exn:fail:web3:abi:type? (lambda () (decode-result "f(uint256)" #"")))
    (check-exn #rx"^decode-result: signature has no outputs; write name[(][.][.][.][)][(][.][.][.][)]: \"f[(]uint256[)]\""
               (lambda () (decode-result "f(uint256)" #"")))
    (check-equal? (decode-result "f()()" #"\1\2") '())
    (check-equal? (abi-decode '() #"\1") '()))

  (define messages
    `((abi-decode ("uint256") ,(make-bytes 31 0) "data too short" "(uint256)" 0)
      (abi-decode ("string") ,(w 64) "offset out of range" "string" 0)
      (abi-decode ("string" "string") ,(bytes-append (words 64 64 1) (s #"a")) "overlapping tail" "string" 32)
      (abi-decode ("bytes") ,(words 32 (sub1 (expt 2 256))) "length out of range" "bytes" 32)
      (abi-decode ("uint8") ,(w 256) "not canonically encoded" "uint8" 0)
      (abi-decode ("string") ,(bytes-append (words 32 1) (s #"\377")) "invalid UTF-8" "string" 64)
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
                                                     (abi-decode '(bytes) (words 32 (sub1 (expt 2 256))))))))
    (check-regexp-match #rx"^decode-revert: "
                        (exn-message (decode-error (lambda ()
                                                     (decode-revert (bytes-append (error-selector "Error(string)")
                                                                                  (w (expt 2 64)))))))))

  (test-case "decode errors are ABI errors and web3 errors"
    (define e (decode-error (lambda () (abi-decode '(uint8 string) (w 1)))))
    (check-true (exn:fail:web3:abi? e))
    (check-true (exn:fail:web3? e))
    (check-true (andmap abi-type? (exn:fail:web3:abi:decode-type e)))
    (check-true (exact-nonnegative-integer? (exn:fail:web3:abi:decode-offset e)))
    (check-not-false (memq (exn:fail:web3:abi:decode-reason e)
                           '(short offset overlap length non-canonical utf-8 selector)))
    (check-pred abi-type? (exn:fail:web3:abi:decode-type
                           (decode-error (lambda () (abi-decode '(bool) (w 2))))))
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
    (check-equal? (abi-decode/who 'decode-log (list (abi-uint 256)) (w 5) 0) '(5))
    (check-decode-error (abi-decode/who 'decode-log '(uint8) (bytes-append (make-bytes 64) (w 256)) 64)
                        'decode-log 'non-canonical 64 #:type "uint8")
    (check-decode-error (abi-decode/who 'decode-log '(string) (bytes-append (make-bytes 7) (w 0)) 7)
                        'decode-log 'overlap 7 #:type "string")
    (check-decode-error (abi-decode/who 'decode-log '(uint256) (make-bytes 40) 9)
                        'decode-log 'short 9 #:type '("uint256"))
    (check-equal? (decode-result/who 'contract-call "f()(uint256)" (w 7)) '(7))
    (check-decode-error (decode-result/who 'contract-call "f()(bool)" (w 2))
                        'contract-call 'non-canonical 0 #:type "bool")
    (check-exn #rx"^contract-call: signature has no outputs"
               (lambda () (decode-result/who 'contract-call "f()" #""))))

  (test-case "doc examples"
    (check-equal? (abi-decode '(uint8 string) (abi-encode '(uint8 string) '(7 "hi"))) '(7 "hi"))
    (check-equal? (abi-decode '(address) (abi-encode '(address) (list A))) (list A*))
    (check-exn #rx"^abi-decode: not canonically encoded" (lambda () (abi-decode '(uint8) (w 256))))
    (check-equal? (decode-result "balanceOf(address)(uint256)" (w 1000)) '(1000))
    (check-equal? (decode-call "transfer(address,uint256)"
                               (encode-call "transfer(address,uint256)" A 1000))
                  (list A* 1000))))
