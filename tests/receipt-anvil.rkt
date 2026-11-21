#lang racket/base

(module+ test
  (require racket/match
           rackunit
           web3
           "../examples/common.rkt"
           (only-in "util.rkt" anvil-alice anvil-bob))

  (define transfer "Transfer(address indexed from, address indexed to, uint256 value)")

  (define (check-read-alike receipt)
    (define tx-hash (receipt-transaction-hash receipt))
    (check-equal? (eth-get-transaction-receipt tx-hash) receipt)
    (check-equal? (wait-for-receipt tx-hash) receipt)
    (check-equal? (eth-get-block-receipts (receipt-block-number receipt)) (list receipt))
    (check-equal? (receipt-from receipt) anvil-alice)
    (check-true (receipt-succeeded? receipt))
    (check-equal? (receipt-type receipt) 2)
    (check-pred positive? (receipt-gas-used receipt))
    (check-pred positive? (receipt-effective-gas-price receipt))
    (check-equal? (bytes-length (receipt-logs-bloom receipt)) 256))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping receipt anvil tests: anvil not found (set WEB3_ANVIL or use `nix develop`)")]
    [else
     (with-anvil ()
       (parameterize ([current-account anvil-alice])
         (define nonce (eth-get-transaction-count anvil-alice "latest"))
         (define-values (token deployed)
           (deploy-contract/receipt (artifact-bytecode "Token") "constructor(string,string,uint256)"
                                    "Token" "TOK" 1000))

         (test-case "R14: a deploy's receipt is the node's, and names the new contract"
           (check-read-alike deployed)
           (check-false (receipt-to deployed))
           (check-equal? (receipt-contract-address deployed) token)
           (check-equal? (receipt-contract-address deployed) (create-address anvil-alice nonce)))

         (test-case "R14: a send's receipt is the node's, and its logs decode"
           (define sent (contract-send token "transfer(address,uint256)" anvil-bob 250))
           (check-read-alike sent)
           (check-equal? (receipt-to sent) token)
           (check-false (receipt-contract-address sent))
           (match (receipt-logs sent #:events (list transfer))
             [(list (? event-log? decoded))
              (check-equal? (event-log-arguments decoded) (list anvil-alice anvil-bob 250))
              (check-equal? (log-transaction-hash decoded) (receipt-transaction-hash sent))
              (check-equal? (log-block-hash decoded) (receipt-block-hash sent))]
             [other (fail (format "~a" other))]))))]))
