#lang racket/base

(module+ test
  (require racket/list
           racket/match
           rackunit
           web3/abi
           web3/abi/vectors
           (only-in web3/rpc hex-string->bytes)
           "util.rkt")

  (test-case "every committed vector reproduces"
    (for ([v (in-list abi-encode-vectors)])
      (check-equal? (encode-hex (abi-vector-mode v) (abi-vector-signature v) (abi-vector-values v))
                    (abi-vector-hex v)
                    (abi-vector-name v))))

  (define required-names
    '("uint256-zero" "uint256-max" "uint8-max" "int8-bounds" "int256-min" "int256-max"
      "int24-negative" "uint-widths" "address-case" "bool-pair" "bytesN-padding" "bytes-empty"
      "bytes-32" "bytes-33" "string-empty" "string-utf8" "string-64" "uint-array-empty"
      "uint-array" "uint-fixed" "string-array" "string-fixed" "bytes-array-empty"
      "nested-arrays" "suffix-order" "tuple-static" "tuple-dynamic" "tuple-array"
      "tuple-array-empty" "tuple-nested" "static-in-dynamic" "static-tuple-array" "mixed"
      "no-args" "packed-widths" "packed-dynamic" "packed-empty" "packed-arrays" "packed-wide"
      "call-transfer" "call-outputs-ignored" "call-no-args" "call-negative" "call-dynamic"))

  (test-case "the vector file is well formed and covers the required cases"
    (define names (map abi-vector-name abi-encode-vectors))
    (check-false (check-duplicates names))
    (for ([v (in-list abi-encode-vectors)])
      (check-regexp-match #px"^0x(?:[0-9a-f]{2})*$" (abi-vector-hex v))
      (define inputs (signature-inputs (abi-vector-signature v)))
      (check-equal? (length (abi-vector-values v)) (length inputs) (abi-vector-name v))
      (for ([t (in-list inputs)]
            [x (in-list (abi-vector-values v))])
        (check-true (abi-value? t x) (abi-vector-name v))))
    (for ([name (in-list required-names)])
      (check-not-false (member name names) name)))

  (test-case "every encoding and call vector decodes back"
    (for ([v (in-list abi-encode-vectors)])
      (match-define (abi-vector mode name signature vs hex) v)
      (define inputs (signature-inputs signature))
      (define data (hex-string->bytes hex))
      (match mode
        ['encode (check-equal? (abi-decode inputs data) (checksummed inputs vs) name)]
        ['call (check-equal? (decode-call signature data) (checksummed inputs vs) name)]
        ['packed (void)])))

  (test-case "decode vectors decode to their values"
    (for ([v (in-list abi-decode-vectors)])
      (match-define (abi-vector mode name signature vs hex) v)
      (define inputs (signature-inputs signature))
      (define data (hex-string->bytes hex))
      (check-equal? mode 'decode name)
      (check-equal? (abi-decode inputs data) vs name)
      (check-decoded inputs data vs name)))

  (test-case "reject vectors raise their reason at their offset"
    (for ([v (in-list abi-reject-vectors)])
      (match-define (abi-reject-vector name signature hex reason offset) v)
      (check-decode-error (abi-decode (signature-inputs signature) (hex-string->bytes hex))
                          'abi-decode reason offset)))

  (test-case "vector names are unique across the three lists"
    (define names (append (map abi-vector-name abi-encode-vectors)
                          (map abi-vector-name abi-decode-vectors)
                          (map abi-reject-vector-name abi-reject-vectors)))
    (check-false (check-duplicates names))
    (for ([v (in-list abi-decode-vectors)])
      (for ([t (in-list (signature-inputs (abi-vector-signature v)))]
            [x (in-list (abi-vector-values v))])
        (check-true (abi-value? t x) (abi-vector-name v))))))
