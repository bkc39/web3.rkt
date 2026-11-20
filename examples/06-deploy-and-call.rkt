#lang racket/base

(require racket/match
         web3
         "common.rkt")

(define transfer "transfer(address,uint256)")
(define errors '("InsufficientBalance(uint256,uint256)"))

(define (print-refusal e)
  (printf "refused: ~a\n" (abi-revert->string (exn->abi-revert e))))

(module+ main
  (with-anvil ()
    (match-define (list alice bob _ ...) (eth-accounts))
    (parameterize ([current-account alice])
      (define token
        (deploy-contract (artifact-bytecode "Token") "constructor(string,string,uint256)"
                         "Token" "TOK" 1000))
      (printf "~a at ~a\n" (contract-call token "name()(string)") token)
      (define receipt (contract-send token transfer bob 250))
      (printf "transfer succeeded ~a\n" (receipt-succeeded? receipt))
      (printf "balances ~s\n"
              (contract-call token "balancesOf(address[])(uint256[])" (list alice bob)))
      (with-handlers ([exn:fail:web3? print-refusal])
        (contract-send token transfer alice 1000 #:from bob #:errors errors)))))
