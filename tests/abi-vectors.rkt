#lang racket/base

;; web3/abi/vectors: every committed vector reproduces.

(module+ test
  (require racket/list
           racket/match
           rackunit
           web3/abi
           web3/abi/vectors
           (only-in web3/rpc bytes->hex-string))

  (define (vector-bytes v)
    (match-define (struct* abi-vector ([mode mode] [signature signature] [values vs])) v)
    (match mode
      ['encode (abi-encode (abi-signature-inputs (string->abi-signature signature)) vs)]
      ['packed (abi-encode-packed (abi-signature-inputs (string->abi-signature signature)) vs)]
      ['call (apply encode-call signature vs)]))

  (test-case "V1 every vector reproduces"
    (for ([v (in-list abi-encode-vectors)])
      (check-equal? (bytes->hex-string (vector-bytes v)) (abi-vector-hex v) (abi-vector-name v))))

  (define required-names
    '("uint256-zero" "uint256-max" "uint8-max" "int8-bounds" "int256-min" "int256-max"
      "int24-negative" "uint-widths" "address-case" "bool-pair" "bytesN-padding" "bytes-empty"
      "bytes-32" "bytes-33" "string-empty" "string-utf8" "string-64" "uint-array-empty"
      "uint-array" "uint-fixed" "string-array" "string-fixed" "bytes-array-empty"
      "nested-arrays" "suffix-order" "tuple-static" "tuple-dynamic" "tuple-array"
      "tuple-array-empty" "tuple-nested" "static-in-dynamic" "static-tuple-array" "mixed"
      "no-args" "packed-widths" "packed-dynamic" "packed-empty" "packed-arrays" "packed-wide"
      "call-transfer" "call-outputs-ignored" "call-no-args" "call-negative" "call-dynamic"))

  (test-case "V2 the file is well formed"
    (define names (map abi-vector-name abi-encode-vectors))
    (check-false (check-duplicates names))
    (for ([v (in-list abi-encode-vectors)])
      (check-regexp-match #px"^0x(?:[0-9a-f]{2})*$" (abi-vector-hex v))
      (define inputs (abi-signature-inputs (string->abi-signature (abi-vector-signature v))))
      (check-equal? (length (abi-vector-values v)) (length inputs) (abi-vector-name v))
      (for ([t (in-list inputs)]
            [x (in-list (abi-vector-values v))])
        (check-true (abi-value? t x) (abi-vector-name v))))
    (for ([name (in-list required-names)])
      (check-not-false (member name names) name))))
