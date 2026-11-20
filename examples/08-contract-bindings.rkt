#lang racket/base

(require racket/match
         web3)

(define-contract token #:file "contracts/out/Token.sol/Token.json")

(define (print-refusal e)
  (define revert (exn->abi-revert e))
  (printf "refused ~a ~s\n" (abi-revert-name revert) (abi-revert-arguments revert)))

(module+ main
  (with-anvil ()
    (match-define (list alice bob _ ...) (eth-accounts))
    (parameterize ([current-account alice])
      (define gold (token-deploy "Gold" "GLD" 1000))
      (token-transfer gold bob 250)
      (printf "~a: alice ~a, bob ~a\n"
              (token-symbol gold) (token-balance-of gold alice) (token-balance-of gold bob))
      (with-handlers ([exn:fail:web3? print-refusal])
        (token-transfer gold alice 1000 #:from bob))
      (for ([entry (in-list (contract-logs gold token-transfer-event #:where (hasheq 'to bob)))])
        (printf "transfer to bob ~a\n" (event-log-ref entry 'value))))))
