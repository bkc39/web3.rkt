#lang racket/base

;; Revert payloads and panic codes, for the decoder's tests and for the
;; same payloads sent through anvil.

(require (only-in racket/function curry)
         web3/abi
         "util.rkt")

(provide panics
         (struct-out revert-case)
         reverts)

;; display is what abi-revert->string gives for the payload.
(struct revert-case (payload errors name arguments display) #:transparent)

(define rejected "Rejected(address,string,uint256[])")
(define unauthorized "Unauthorized()")

(define reverts
  (map
   (curry apply revert-case)
   `((,(encode-call "Error(string)" "oops!") () "Error" ("oops!") "Error(\"oops!\")")
     (,(encode-call "Error(string)" "") () "Error" ("") "Error(\"\")")
     (,(bytes-append (error-selector "Error(string)") (word 32) (word 5) #"oops!") () "Error" ("oops!")
      "Error(\"oops!\")")
     (,(encode-call "Panic(uint256)" #x11) () "Panic" (17)
      "Panic(0x11): arithmetic underflow or overflow")
     (,(encode-call "Panic(uint256)" #x99) () "Panic" (153) "Panic(0x99)")
     (,(encode-call "Panic(uint256)" max-word) () "Panic" (,max-word)
      ,(string-append "Panic(0x" (make-string 64 #\f) ")"))
     (,(encode-call insufficient-balance 10 20) (,insufficient-balance) "InsufficientBalance" (10 20)
      "InsufficientBalance(10, 20)")
     (,(encode-call rejected alice "no" '(1 2)) (,rejected) "Rejected" (,alice-checksummed "no" (1 2))
      ,(format "Rejected(~a, \"no\", [1, 2])" alice-checksummed))
     (,(encode-call unauthorized) (,unauthorized) "Unauthorized" () "Unauthorized()")
     (,(encode-call insufficient-balance 10 20) () #f () "unknown error 0xcf479181 with 64 bytes of data")
     (#"" () #f () "no revert data")
     (#"\1\2" () #f () "revert data 0x0102")
     (,(bytes #xde #xad #xbe #xef) () #f () "unknown error 0xdeadbeef with 0 bytes of data"))))

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
