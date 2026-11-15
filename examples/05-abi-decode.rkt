#lang racket/base

;; 05: decoding values, results, call data and reverts.
;;
;; Decoders read untrusted bytes: anything that is not an encoding raises
;; exn:fail:web3:abi:decode, and addresses come back checksummed.
;;
;;   racket examples/05-abi-decode.rkt
;;   racket -i -e '(enter! "examples/05-abi-decode.rkt")'

(require web3
         "common.rkt")

(provide (all-defined-out))

(section "a round trip")

(define order-type "(address,uint256,(string,bytes)[])")
(define order
  (list "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed" 1000 '(("gold" #"\1\2") ("silver" #""))))

(define encoded (abi-encode (list order-type) (list order)))
(show-words encoded)
(show (abi-decode (list order-type) encoded))
(show (equal? (abi-decode (list order-type) encoded) (list order)))

(section "addresses")

(show (abi-decode '(address) (abi-encode '(address) '("0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed"))))
(show (address->checksum-string "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed"))
(show (checksum-address? "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed"))
(show (checksum-address? "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed"))

(section "results from a contract")

(define node (start-anvil))
(current-provider (anvil-provider node))

;; mstore(0, add(calldataload(4), calldataload(36))); return(0, 32)
(define adder "0x00000000000000000000000000000000000c0de0")
(anvil-set-code adder "0x602435600435015f5260205ff3")

(define sum
  (eth-call (hasheq 'to adder 'data (bytes->hex-string (encode-call "add(uint256,uint256)" 2 40)))
            "latest"))
(show (decode-result "add(uint256,uint256)(uint256)" (hex-string->bytes sum)))

(section "call data")

;; The identity precompile returns its input.
(define identity "0x0000000000000000000000000000000000000004")
(define submit (encode-call "submit(string,uint256[])" "hello" '(1 2)))
(define echoed
  (hex-string->bytes (eth-call (hasheq 'to identity 'data (bytes->hex-string submit)) "latest")))

(show (decode-call "submit(string,uint256[])" echoed))
(show-error (decode-call "transfer(address,uint256)" echoed))

(section "reverts")

;; CALLDATASIZE PUSH0 PUSH0 CALLDATACOPY CALLDATASIZE PUSH0 REVERT: reverts
;; with its own call data, so each payload below comes back as the revert.
(define reverter "0x0000000000000000000000000000000000000bad")
(anvil-set-code reverter "0x365f5f37365ffd")

(define errors '("InsufficientBalance(uint256,uint256)"))

(define funds (encode-call "Error(string)" "insufficient funds"))
(show-error (eth-call (hasheq 'to reverter 'data (bytes->hex-string funds)) "latest") #:as funds-exn)
(define funds-revert (rpc-exn->revert funds-exn))
(show (abi-revert-name funds-revert))
(show (abi-revert-arguments funds-revert))
(show (abi-revert->string funds-revert))

(define overflow (encode-call "Panic(uint256)" #x11))
(show-error (eth-call (hasheq 'to reverter 'data (bytes->hex-string overflow)) "latest")
            #:as overflow-exn)
(show (abi-revert->string (rpc-exn->revert overflow-exn)))
(show (panic-description #x11))

(define balance (encode-call "InsufficientBalance(uint256,uint256)" 10 20))
(show-error (eth-call (hasheq 'to reverter 'data (bytes->hex-string balance)) "latest")
            #:as balance-exn)
(show (exn:fail:web3:rpc-data balance-exn))
(define balance-revert (rpc-exn->revert balance-exn #:errors errors))
(show (abi-revert-name balance-revert))
(show (abi-revert-arguments balance-revert))
(show (abi-revert->string balance-revert))

(show (abi-revert->string (decode-revert balance)))
(show (bytes->hex-string (abi-revert-selector (decode-revert balance))))
(show (bytes->hex-string (error-selector "InsufficientBalance(uint256,uint256)")))

(show-error (eth-call (hasheq 'to reverter 'data "0x") "latest") #:as empty-exn)
(show (abi-revert->string (rpc-exn->revert empty-exn)))

(show (decode-revert-reason funds))
(show (decode-revert-reason balance))

(show (exn-message (rpc-exn->revert-exn balance-exn #:errors errors)))
(show (exn:fail:web3:rpc:revert? (rpc-exn->revert-exn balance-exn #:errors errors)))

(section "malformed data")

(show-error (abi-decode '(uint8) (word->bytes 256)) #:as dirty)
(show (exn:fail:web3:abi:decode-reason dirty))
(show (exn:fail:web3:abi:decode-offset dirty))
(show (exn:fail:web3:abi:decode-type dirty))

(show-error (abi-decode '(bytes) (bytes-append (word->bytes 32) (word->bytes (sub1 (expt 2 256))))))
(show-error (abi-decode '(string) (bytes-append (word->bytes 32) (word->bytes 6) #"oops!")))
(show-error (abi-decode '(string string)
                        (bytes-append (word->bytes 64) (word->bytes 64) (word->bytes 1) #"a"
                                      (make-bytes 31 0))))
(show-error (abi-decode '(string)
                        (bytes-append (word->bytes 32) (word->bytes 1) #"\377" (make-bytes 31 0))))

(show-error (abi-decode "uint256" #""))
