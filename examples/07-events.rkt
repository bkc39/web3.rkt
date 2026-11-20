#lang racket/base

(require racket/list
         racket/match
         web3
         "common.rkt")

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
        (deploy-contract (artifact-bytecode "Token") "constructor(string,string,uint256)"
                         "Token" "TOK" 1000))
      (define receipt (contract-send token transfer bob 250))
      (printf "receipt\n")
      (print-transfer (decode-log transfer-event (first (receipt-logs receipt))))
      (contract-send token transfer carol 100)
      (printf "every transfer\n")
      (for-each print-transfer (get-logs (log-filter #:address token #:event transfer-event)))
      (printf "to bob\n")
      (for-each print-transfer
                (get-logs (log-filter #:address token #:event transfer-event
                                      #:where (hasheq 'to bob)))))))
