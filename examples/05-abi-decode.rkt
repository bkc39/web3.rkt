#lang racket/base

(require web3)

(define token-info "info()(string,uint256)")
(define transfer "transfer(address,uint256)")
(define errors '("InsufficientBalance(uint256,uint256)"))

(define result (abi-encode '(string uint256) '("Token" 1000)))
(define call-data (encode-call transfer "0x70997970C51812dc3A010C7d01b50e0d17dc79C8" 250))
(define reverts
  (list (encode-call "Error(string)" "not the owner")
        (encode-call "Panic(uint256)" #x11)
        (encode-call "InsufficientBalance(uint256,uint256)" 250 1000)))

(module+ main
  (printf "result ~s\n" (decode-result token-info result))
  (printf "call ~s\n" (decode-call transfer call-data))
  (for ([data (in-list reverts)])
    (printf "revert ~a\n" (abi-revert->string (decode-revert data #:errors errors))))
  (with-handlers ([exn:fail:web3:abi:decode? (lambda (e) (printf "refused: ~a\n" (exn-message e)))])
    (decode-call transfer (subbytes call-data 0 36))))
