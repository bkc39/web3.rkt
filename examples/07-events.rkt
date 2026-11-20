#lang racket/base

(require racket/match
         racket/runtime-path
         web3)

(define-runtime-path token-artifact "contracts/out/Token.sol/Token.json")

(define init-code (contract-abi-bytecode (file->contract-abi token-artifact)))
(define transfer "transfer(address,uint256)")
(define transfer-event
  (string->abi-event "event Transfer(address indexed from, address indexed to, uint256 value)"))

(define (print-transfer entry)
  (printf "  ~a -> ~a ~a\n"
          (event-log-ref entry 'from) (event-log-ref entry 'to) (event-log-ref entry 'value)))

(module+ main
  (with-anvil ()
    (match-define (list alice bob carol _ ...) (eth-accounts))
    (parameterize ([current-account alice])
      (define token
        (deploy-contract init-code "constructor(string,string,uint256)" "Token" "TOK" 1000))
      (define receipt (contract-send token transfer bob 250))
      (match-define (list raw) (receipt-logs receipt))
      (printf "receipt\n")
      (print-transfer (decode-log transfer-event raw))
      (contract-send token transfer carol 100)
      (printf "every transfer\n")
      (for ([entry (in-list (get-logs (log-filter #:address token #:event transfer-event)))])
        (print-transfer entry))
      (printf "to bob\n")
      (define to-bob (log-filter #:address token #:event transfer-event #:where (hasheq 'to bob)))
      (for ([entry (in-list (get-logs to-bob))])
        (print-transfer entry)))))
