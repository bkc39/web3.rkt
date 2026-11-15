#lang racket/base

(module+ test
  (require racket/list
           rackunit
           web3/abi
           web3/abi/vectors
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
      (check-not-false (member name names) name))))
