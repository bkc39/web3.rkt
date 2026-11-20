#lang racket/base

(module+ test
  (require (only-in racket/file file->bytes)
           racket/match
           rackunit
           web3/rlp
           (only-in web3/rpc bytes->hex-string hex-string->bytes)
           "helpers/rlp.rkt")

  (define upstream-repo "ethereum/tests")
  (define upstream-commit "7693364be004b4a00f0efd8c1cba77becf2f87e0")
  (define upstream-path "RLPTests")

  (define pinned
    '(("rlptest.json" "cb0d9fd399f140d6bab9d411033f8be131df8df0")
      ("invalidRLPTest.json" "21d0b2d14332292c9a9bd5becb73ad6a5bacba62")
      ("LICENSE" "65f52d6f57b0ec2759930e9f4d3ec6a803d30bcf")))

  (define invalid-reasons
    '((short 0 int32Overflow int32Overflow2 emptyEncoding
             lessThanShortLengthArray1 lessThanShortLengthArray2
             lessThanShortLengthList1 lessThanShortLengthList2
             lessThanLongLengthArray1 lessThanLongLengthArray2
             lessThanLongLengthList1 lessThanLongLengthList2)
      (size 0 wrongSizeList wrongSizeList2
            nonOptimalLongLengthArray1 nonOptimalLongLengthArray2
            nonOptimalLongLengthList1 nonOptimalLongLengthList2)
      (leading-zero 0 incorrectLengthInArray
                    leadingZerosInLongLengthArray1 leadingZerosInLongLengthArray2
                    leadingZerosInLongLengthList1 leadingZerosInLongLengthList2)
      (leading-zero 4 randomRLP)
      (single-byte 0 bytesShouldBeSingleByte00 bytesShouldBeSingleByte01
                   bytesShouldBeSingleByte7F)))

  (define (git-blob-sha1 content)
    (bytes->hex-string
     (sha1-bytes (bytes-append (string->bytes/utf-8 (format "blob ~a" (bytes-length content)))
                               #"\0"
                               content))))

  (define (json->item v)
    (match v
      [(pregexp #px"^#([0-9]+)$" (list _ digits)) (natural->rlp-scalar (string->number digits))]
      [(? string?)
       (check-true (for/and ([c (in-string v)]) (char<? c #\u80)) v)
       (string->bytes/utf-8 v)]
      [(? exact-nonnegative-integer?) (natural->rlp-scalar v)]
      [(? list?) (map json->item v)]))

  (define (json->natural v)
    (match v
      [(pregexp #px"^#([0-9]+)$" (list _ digits)) (string->number digits)]
      [(? exact-nonnegative-integer?) v]
      [_ #f]))

  (test-case "V0: the vendored files are upstream's"
    (for ([row (in-list pinned)])
      (match-define (list name sha) row)
      (check-equal? (git-blob-sha1 (file->bytes (rlp-vector-path name)))
                    (string-append "0x" sha)
                    (format "~a/~a/~a@~a" upstream-repo upstream-path name upstream-commit))))

  (test-case "V1: every valid vector encodes to its bytes and decodes back"
    (define cases (rlp-vectors "rlptest.json"))
    (check-equal? (hash-count cases) 28)
    (for ([(name v) (in-hash cases)])
      (define item (json->item (hash-ref v 'in)))
      (define out (hash-ref v 'out))
      (check-equal? (bytes->hex-string (rlp-encode item)) out (symbol->string name))
      (check-equal? (rlp-decode (hex-string->bytes out)) item (symbol->string name))
      (define n (json->natural (hash-ref v 'in)))
      (when n
        (check-equal? (rlp-scalar->natural (rlp-decode (hex-string->bytes out))) n
                      (symbol->string name)))))

  (test-case "V2: every invalid vector is refused with its reason and offset"
    (define cases (rlp-vectors "invalidRLPTest.json"))
    (define expected
      (for/fold ([table (hasheq)]) ([row (in-list invalid-reasons)])
        (match-define (list reason offset names ...) row)
        (for/fold ([table table]) ([name (in-list names)])
          (hash-set table name (list reason offset)))))
    (check-equal? (sort (hash-keys expected) symbol<?) (sort (hash-keys cases) symbol<?))
    (check-equal? (hash-count cases) 26)
    (for ([(name v) (in-hash cases)])
      (match-define (list reason offset) (hash-ref expected name '(#f #f)))
      (check-decode-fault (hex-string->bytes (hash-ref v 'out)) reason offset
                          #:name (symbol->string name)))))
