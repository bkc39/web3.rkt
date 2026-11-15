#lang racket/base

;; Revert payloads and panic codes, for the decoder's tests and for the
;; same payloads sent through anvil.

(require web3/abi)

(provide panics
         reverts)

(define (w n) (word->bytes n))
(define A "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed")
(define A* "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed")
(define balance "InsufficientBalance(uint256,uint256)")
(define rejected "Rejected(address,string,uint256[])")
(define unauthorized "Unauthorized()")
(define max-word (sub1 (expt 2 256)))

;; payload, errors, name, arguments, string
(define reverts
  `((,(encode-call "Error(string)" "oops!") () "Error" ("oops!") "Error(\"oops!\")")
    (,(encode-call "Error(string)" "") () "Error" ("") "Error(\"\")")
    (,(bytes-append (bytes #x08 #xc3 #x79 #xa0) (w 32) (w 5) #"oops!") () "Error" ("oops!")
     "Error(\"oops!\")")
    (,(encode-call "Panic(uint256)" #x11) () "Panic" (17)
     "Panic(0x11): arithmetic underflow or overflow")
    (,(encode-call "Panic(uint256)" #x99) () "Panic" (153) "Panic(0x99)")
    (,(encode-call "Panic(uint256)" max-word) () "Panic" (,max-word)
     ,(string-append "Panic(0x" (make-string 64 #\f) ")"))
    (,(encode-call balance 10 20) (,balance) "InsufficientBalance" (10 20)
     "InsufficientBalance(10, 20)")
    (,(encode-call rejected A "no" '(1 2)) (,rejected) "Rejected" (,A* "no" (1 2))
     ,(format "Rejected(~a, \"no\", [1, 2])" A*))
    (,(encode-call unauthorized) (,unauthorized) "Unauthorized" () "Unauthorized()")
    (,(encode-call balance 10 20) () #f () "unknown error 0xcf479181 with 64 bytes of data")
    (#"" () #f () "no revert data")
    (#"\1\2" () #f () "revert data 0x0102")
    (,(bytes #xde #xad #xbe #xef) () #f () "unknown error 0xdeadbeef with 0 bytes of data")))

(define panics
  '((#x00 "generic/unspecified error")
    (#x01 "assertion failed")
    (#x11 "arithmetic underflow or overflow")
    (#x12 "division or modulo by zero")
    (#x21 "failed to convert value into enum type")
    (#x22 "storage byte array incorrectly encoded")
    (#x31 "called `.pop()` on an empty array")
    (#x32 "array out-of-bounds access")
    (#x41 "memory allocation error")
    (#x51 "called an invalid internal function")))
