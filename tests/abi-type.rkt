#lang racket/base

(module+ test
  (require racket/list
           racket/match
           (only-in racket/contract contract-name)
           rackunit
           syntax/parse/define
           web3/abi/type
           (only-in web3/rpc exn:fail:web3?)
           "abi-gen.rkt"
           "modules.rkt"
           "util.rkt"
           (for-syntax racket/base
                       web3/abi/type))

  (define-syntax-parse-rule (check-raises-type input position expr)
    (let ([e (with-handlers ([exn:fail:web3:abi:type? values])
               expr
               #f)])
      (check-pred exn:fail:web3:abi:type? e (format "~s" 'expr))
      (when e
        (check-equal? (exn:fail:web3:abi:type-input e) input (format "~s" 'expr))
        (check-equal? (exn:fail:web3:abi:type-position e) position (format "~s" 'expr)))))

  (define (generated-types n)
    (define prng (seeded 1 2 3 4 5 6))
    (for/list ([_ (in-range n)]) (random-abi-type prng)))

  (define canonical-pairs
    '(("uint" "uint256")
      ("int" "int256")
      ("uint[]" "uint256[]")
      ("(uint,int)[2]" "(uint256,int256)[2]")
      ("tuple(uint256,address)" "(uint256,address)")
      (" ( uint256 , string )[]" "(uint256,string)[]")
      ("tuple(tuple(uint8),bytes)[3][]" "((uint8),bytes)[3][]")
      ("uint8[2][]" "uint8[2][]")))

  (test-case "aliases, tuple() and spaces canonicalise"
    (for ([pair (in-list canonical-pairs)])
      (match-define (list in out) pair)
      (check-equal? (abi-type->string (string->abi-type in)) out in)))

  (test-case "canonical strings are fixed points"
    (for ([c (in-list (append (map second canonical-pairs)
                              (map abi-type->string (generated-types 500))))])
      (check-equal? (abi-type->string (string->abi-type c)) c)))

  (test-case "string and s-expression forms round-trip"
    (for ([t (in-list (generated-types 500))])
      (check-equal? (string->abi-type (abi-type->string t)) t)
      (check-equal? (sexp->abi-type (abi-type->sexp t)) t)
      (check-equal? (abi-type (abi-type->sexp t)) t)
      (check-equal? (abi-type t) t)))

  ;; (string sexp struct-expression)
  (define three-forms
    '(("uint256" uint256 (abi-uint 256))
      ("uint8[]" (array uint8) (abi-array (abi-uint 8)))
      ("uint8[2]" (array uint8 2) (abi-fixed-array (abi-uint 8) 2))
      ("uint8[2][]" (array (array uint8 2)) (abi-array (abi-fixed-array (abi-uint 8) 2)))
      ("(address,uint256[])" (tuple address (array uint256))
                             (abi-tuple (list (abi-address) (abi-array (abi-uint 256)))))
      ("(uint256,string)[2]" (array (tuple uint256 string) 2)
                             (abi-fixed-array (abi-tuple (list (abi-uint 256) (abi-string))) 2))
      ("bytes32" bytes32 (abi-fixed-bytes 32))))

  (define-namespace-anchor anchor)
  (define ns (namespace-anchor->namespace anchor))

  (test-case "the string, s-expression and struct forms agree"
    (for ([row (in-list three-forms)])
      (match-define (list s x struct-expr) row)
      (define t (eval struct-expr ns))
      (check-equal? (string->abi-type s) t s)
      (check-equal? (sexp->abi-type x) t s)
      (check-equal? (abi-type s) t s)
      (check-equal? (abi-type x) t s)
      (check-equal? (abi-type->string t) s)
      (check-equal? (abi-type->sexp t) x)
      (check-equal? (abi-type->string x) s)
      (check-equal? (abi-type->sexp s) x))
    (check-equal? (abi-type 'uint) (abi-uint 256)))

  (define string-rejections
    '(("uint7" 0) ("uint264" 0) ("uint08" 0) ("bytes0" 0) ("bytes33" 0) ("byte" 0)
      ("Uint256" 0) ("fixed128x18" 0) ("ufixed" 0) ("function" 0) ("foo" 0) ("" 0)
      ("tuple" 5) ("address payable" 8) ("uint256[0]" 8) ("uint256[01]" 8) ("uint256[-1]" 8)
      ("uint256[" 8) ("uint256 []" 7) ("uint256[]x" 9) ("(uint256" 8) ("(uint256,)" 9)
      ("()" 1) ("uint 256" 4) ("(uint256) []" 9)
      ("uint256[18446744073709551616]" 8) ("uint256[1000000000000000000000]" 8)))

  (test-case "malformed strings raise with a position"
    (for ([row (in-list string-rejections)])
      (match-define (list s position) row)
      (check-raises-type s position (string->abi-type s))
      (check-raises-type s position (abi-type s))))

  (test-case "type error messages"
    (check-exn #rx"^abi-type: not an ABI type: \"uint7\"\n  at: position 0$"
               (lambda () (abi-type "uint7")))
    (check-exn #rx"^string->abi-type: " (lambda () (string->abi-type "uint7")))
    (check-exn #rx"not supported" (lambda () (abi-type "function")))
    (check-exn #rx"not supported" (lambda () (abi-type "fixed128x18")))
    (check-exn #rx"write address" (lambda () (abi-type "address payable")))
    (check-exn #rx"expected an array length from 1 to 2\\^64-1"
               (lambda () (abi-type "uint256[18446744073709551616]"))))

  (test-case "the largest array length is 2^64-1"
    (define k (sub1 (expt 2 64)))
    (check-equal? (abi-type "uint8[18446744073709551615]") (abi-fixed-array (abi-uint 8) k))
    (check-equal? (abi-type `(array uint8 ,k)) (abi-fixed-array (abi-uint 8) k)))

  (define sexp-rejections
    '(uint7 Uint256 array (array) (array uint256 0) (array uint256 -1) (array uint256 2 3)
      (tuple) (tuple . address) (vector uint256) (uint256 address) (array "uint256")
      (array uint256 18446744073709551616)))

  (test-case "malformed s-expressions raise"
    (for ([x (in-list sexp-rejections)])
      (check-raises-type x #f (sexp->abi-type x))
      (check-raises-type x #f (abi-type x)))
    (check-exn #rx"^abi-type: not an ABI type: \\(array \"uint256\"\\)\n  form: \"uint256\"$"
               (lambda () (abi-type '(array "uint256")))))

  (test-case "constructors and conversions blame their caller"
    (check-blames-here (abi-type 42))
    (check-blames-here (abi-uint 7))
    (check-blames-here (abi-int 264))
    (check-blames-here (abi-fixed-bytes 33))
    (check-blames-here (abi-fixed-array 'uint256 0))
    (check-blames-here (abi-fixed-array 'uint256 (expt 2 64)))
    (check-blames-here (abi-tuple '()))
    (check-blames-here (abi-signature "1f" '() #f))
    (check-blames-here (string->abi-type 'uint))
    (check-raises-type "uint7" 0 (abi-array "uint7")))

  (define dynamic-and-size
    '(("uint256" #f 32)
      ("bytes32" #f 32)
      ("string" #t #f)
      ("bytes" #t #f)
      ("uint256[]" #t #f)
      ("uint256[3]" #f 96)
      ("string[2]" #t #f)
      ("(uint256,bool)" #f 64)
      ("(uint256,bool)[3]" #f 192)
      ("(uint8[2],address)[3]" #f 288)
      ("(uint256,string)" #t #f)
      ("((string))[1]" #t #f)))

  (test-case "dynamic types have no static size"
    (for ([row (in-list dynamic-and-size)])
      (match-define (list t dynamic? size) row)
      (check-equal? (abi-type-dynamic? t) dynamic? t)
      (check-equal? (abi-type-static-size t) size t))
    (for ([t (in-list (generated-types 500))])
      (check-equal? (not (abi-type-static-size t)) (abi-type-dynamic? t))))

  (define-syntax-parse-rule (canonical t)
    #:with s (datum->syntax #'t (abi-type->string (syntax->datum #'t)))
    s)

  (test-case "web3/abi/type is usable at phase 1 and needs no I/O libraries"
    (check-equal? (canonical (array (tuple address uint))) "(address,uint256)[]")
    (define names (filter values (map module-name (import-closure 'web3/abi/type))))
    (check-not-false (member "racket/contract/base" names))
    (check-equal? (filter (lambda (n) (member (collection n) '("ffi" "net" "json" "file" "keccak")))
                          names)
                  '())
    (check-equal? (sort (remove-duplicates (filter (lambda (n) (equal? (collection n) "web3")) names))
                        string<?)
                  '("web3/abi/type" "web3/private/exn" "web3/private/hex"))
    (define allowed
      '("racket/base" "racket/contract/base" "racket/list" "racket/string" "racket/match"
        "web3/private/exn" "web3/private/hex"))
    (for ([name (in-list (filter values
                                 (map module-name
                                      (direct-imports
                                       (module-path-index-resolve
                                        (module-path-index-join 'web3/abi/type #f) #t)))))])
      (check-not-false (member name allowed) name))
    (define abi-names (filter values (map module-name (import-closure 'web3/abi))))
    (check-not-false (member "keccak/main" abi-names))
    (check-equal? (filter (lambda (n) (member (collection n) '("net" "json"))) abi-names) '()))

  (test-case "signatures"
    (define transfer (string->abi-signature "transfer(address,uint256)"))
    (check-equal? (abi-signature-name transfer) "transfer")
    (check-equal? (length (abi-signature-inputs transfer)) 2)
    (check-false (abi-signature-outputs transfer))
    (define balance-of (string->abi-signature "balanceOf(address)(uint)"))
    (check-equal? (abi-signature-outputs balance-of) (list (abi-uint 256)))
    (check-equal? (abi-signature->string balance-of #:outputs? #t) "balanceOf(address)(uint256)")
    (check-equal? (abi-signature->string balance-of) "balanceOf(address)")
    (check-equal? (abi-signature-inputs (string->abi-signature "f()")) '())
    (check-equal? (abi-signature-outputs (string->abi-signature "f()()")) '())
    (check-equal? (abi-signature->string (string->abi-signature "transfer (address, uint)"))
                  "transfer(address,uint256)")
    (for ([row (in-list '(("transfer(address,uint256" 24) ("1f()" 0) ("f" 1)
                          ("f(uint256)x" 10) ("f((uint256) [])" 11) ("" 0)))])
      (match-define (list s position) row)
      (check-raises-type s position (string->abi-signature s))))

  (test-case "ABI errors are web3 errors"
    (check-pred exn:fail:web3? (raised (lambda () (abi-type "uint7"))))
    (check-true (exn:fail:web3?
                 (exn:fail:web3:abi:value "m" (current-continuation-marks) (abi-bool) 0 '(0)))))

  (test-case "abi-value? and abi-value/c"
    (for ([n (in-list widths)])
      (define u (abi-uint n))
      (define i (abi-int n))
      (define b (abi-fixed-bytes (quotient n 8)))
      (check-true (abi-value? u 0))
      (check-true (abi-value? u (sub1 (expt 2 n))))
      (check-false (abi-value? u -1))
      (check-false (abi-value? u (expt 2 n)))
      (check-true (abi-value? i (- (expt 2 (sub1 n)))))
      (check-true (abi-value? i (sub1 (expt 2 (sub1 n)))))
      (check-false (abi-value? i (sub1 (- (expt 2 (sub1 n))))))
      (check-false (abi-value? i (expt 2 (sub1 n))))
      (check-true (abi-value? b (make-bytes (quotient n 8) 1)))
      (check-false (abi-value? b (make-bytes (add1 (quotient n 8)) 1)))
      (when (> n 8)
        (check-false (abi-value? b (make-bytes (sub1 (quotient n 8)) 1)))))
    (for ([row (in-list '(("uint8" 1.0) ("uint8" "1") ("bool" 0) ("address" "0x1234")
                          ("string" #"ab") ("bytes" "ab") ("uint8[]" #(1))))])
      (match-define (list t v) row)
      (check-false (abi-value? t v) (format "~s ~s" t v)))
    (check-true (abi-value? "(uint8,string)[]" '((1 "a") (2 "b"))))
    (check-true (abi-value? 'address "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"))
    (check-equal? (format "~s" (contract-name (abi-value/c "uint8"))) "(abi-value/c \"uint8\")")
    (check-equal? (format "~v" (abi-value/c 'uint8)) "(abi-value/c \"uint8\")")
    (check-true ((abi-value/c "uint8") 255))
    (check-false ((abi-value/c "uint8") 256)))

  (test-case "abi-signature-spec/c accepts strings and signature structs"
    (check-true (abi-signature-spec/c "f(uint256)"))
    (check-true (abi-signature-spec/c (string->abi-signature "f(uint256)")))
    (check-false (abi-signature-spec/c 'f))
    (check-equal? (contract-name abi-signature-spec/c) 'abi-signature-spec/c)))
