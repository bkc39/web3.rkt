#lang racket/base

;; 04: encoding values for the contract ABI.
;;
;; A type is a struct, a string or an s-expression; values are plain Racket
;; data.  Encodings are bytes, shown here one 32-byte word per line.
;;
;;   racket examples/04-abi-encode.rkt
;;   racket -i -e '(enter! "examples/04-abi-encode.rkt")'

(require web3
         "common.rkt")

(provide (all-defined-out))

(section "types, three ways")

(show (abi-type "uint"))
(show (abi-type 'uint))
(show (equal? (abi-type "uint") (abi-uint 256)))

(define pairs (abi-type "(address,uint)[2]"))
(show pairs)
(show (abi-type->string pairs))
(show (abi-type->sexp pairs))

(show (abi-type-dynamic? "string[2]"))
(show (abi-type-static-size "(uint256,bool)[3]"))

(section "signatures and selectors")

(define balance-of (string->abi-signature "balanceOf(address)(uint256)"))
(show balance-of)
(show (abi-signature->string balance-of))
(show (abi-signature->string balance-of #:outputs? #t))
;; Hashed in canonical form, whatever the spacing and aliases.
(show (bytes->hex-string (function-selector "transfer(address, uint)")))

(section "static values")

;; Any case: the checksum is not checked.
(define me "0x00000000000000000000000000000000000A11CE")

(show-words (abi-encode '(uint8 int8 bool address bytes3) (list 255 -1 #t me #"abc")))

(section "dynamic values")

;; The first two words are offsets to the string's and the array's tails.
(show-words (abi-encode '(string (array uint256)) '("hello" (1 2 3))))

(section "a nested tuple array")

(show-words (abi-encode '("(uint256,string)[]") '(((1 "a") (2 "bc")))))

(section "packed")

(show (bytes->hex-string (abi-encode-packed '(uint16 bytes1 string) (list 1 #"A" "hi"))))
(show (bytes-length (abi-encode '(uint16 bytes1 string) (list 1 #"A" "hi"))))

(section "calling a contract")

(define node (start-anvil))
(current-provider (anvil-provider node))

;; mstore(0, add(calldataload(4), calldataload(36))); return(0, 32)
(define contract "0x00000000000000000000000000000000000c0de0")
(anvil-set-code contract "0x602435600435015f5260205ff3")

(define data (encode-call "add(uint256,uint256)" 2 40))
(show-words data)
(show (bytes->words (hex-string->bytes (eth-call (hasheq 'to contract 'data (bytes->hex-string data))
                                                 "latest"))))

;; The identity precompile returns its input.
(define identity "0x0000000000000000000000000000000000000004")
(define submit (encode-call "submit(string,uint256[])" "hello" '(1 2)))
(show-words submit)
(define echoed (eth-call (hasheq 'to identity 'data (bytes->hex-string submit)) "latest"))
(show (equal? (hex-string->bytes echoed) submit))

(section "errors")

(show-error (encode-call "add(uint256,uint256)" 1 -1))
(show-error (encode-call "add(uint256,uint256)" 1))
(show-error (abi-type "uint7"))
(show-error (abi-encode '(bytes3) (list #"ab")))

;; The path is the argument, then the element, then the component.
(show-error (abi-encode '("(uint8,string)[]") '(((1 "a") (256 "b")))) #:as nested)
(show (exn:fail:web3:abi:value-type nested))
(show (exn:fail:web3:abi:value-value nested))
(show (exn:fail:web3:abi:value-path nested))

(show-error (abi-encode-packed '("(uint8,uint8)") '((1 2))))

;; A wrong Racket shape, here a string for the list of types, is a contract
;; violation instead.
(show-error (abi-encode "uint256" '(1)) #:as violation)
(show (exn:fail:web3:abi? violation))
