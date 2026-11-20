#lang racket/base

(module+ test
  (require rackunit
           "helpers/manual.rkt")

  (define created "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512")

  (define blocks
    `(([(bytes->hex-string (rlp-encode (list #"cat" #"dog"))) (value "0xc88363617483646f67")]
       [(rlp-decode (hex-string->bytes "0xc88363617483646f67")) (value (#"cat" #"dog"))])
      ([(bytes->hex-string (rlp-encode (list (natural->rlp-scalar 0) (natural->rlp-scalar 1024))))
        (value "0xc480820400")]
       [(map rlp-scalar->natural (rlp-decode (hex-string->bytes "0xc480820400")))
        (value (0 1024))])
      ([(define sender "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266") (void)]
       [(define preimage (rlp-encode (list (hex-string->bytes sender) (natural->rlp-scalar 1))))
        (void)]
       [(address->checksum-string (bytes->hex-string (subbytes (keccak256 preimage) 12)))
        (value ,created)]
       [(create-address sender 1) (value ,created)])
      ([(rlp-decode (bytes #x81 #x05))
        (raises "rlp-decode: non-canonical single byte: 0x05 is its own encoding\n  offset: 0")]
       [(rlp-decode (bytes #xc0 #x00))
        (raises "rlp-decode: trailing bytes: 1 byte after the item\n  offset: 1")])
      ([(rlp-item? (list #"" (list #"a"))) (value #t)]
       [(rlp-item? (list "a")) (value #f)])
      ([(bytes->hex-string (rlp-encode #"\x7f")) (value "0x7f")]
       [(bytes->hex-string (rlp-encode #"\x80")) (value "0x8180")]
       [(bytes->hex-string (rlp-encode (list #"" '()))) (value "0xc280c0")])
      ([(rlp-decode (bytes #xc3 #x80 #xc1 #x01)) (value (#"" (#"\1")))]
       [(rlp-decode (bytes #xb8 #x01 #xff))
        (raises "rlp-decode: non-canonical size: a length of 1 takes the short form\n  offset: 0")])
      ([(natural->rlp-scalar 0) (value #"")]
       [(natural->rlp-scalar 1024) (value #"\4\0")])
      ([(rlp-scalar->natural #"\4\0") (value 1024)]
       [(rlp-scalar->natural #"\0\4")
        (raises "rlp-scalar->natural: leading zero: a scalar of 2 bytes starts with 0x00")])))

  (test-case "M1: the manual's RLP examples give what the manual shows"
    (check-manual-blocks blocks))

  (test-case "M1: byte strings print as the manual shows them"
    (check-equal? (format "~s" (bytes 4 0)) "#\"\\4\\0\"")))
