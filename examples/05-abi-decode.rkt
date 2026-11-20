#lang racket/base

(require racket/match
         web3
         "common.rkt")

(define balance-of "balanceOf(address)(uint256)")
(define transfer "transfer(address,uint256)")
(define errors '("InsufficientBalance(uint256,uint256)"))

(define (call token from signature . arguments)
  (define data (bytes->hex-string (apply encode-call signature arguments)))
  (hex-string->bytes (eth-call (hasheq 'from from 'to token 'data data) "latest")))

(define (print-revert e)
  (printf "reverted ~a\n" (abi-revert->string (rpc-exn->revert e #:errors errors))))

(module+ main
  (with-anvil ()
    (match-define (list alice bob _ ...) (eth-accounts))
    (define token
      (deploy-contract (artifact-bytecode "Token") "constructor(string,string,uint256)"
                       "Token" "TOK" 1000
                       #:from alice))
    (printf "balance ~s\n" (decode-result balance-of (call token alice balance-of alice)))
    (with-handlers ([exn:fail:web3:rpc? print-revert])
      (call token bob transfer alice 1))))
