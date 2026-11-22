#lang racket/base

(module+ test
  (require racket/match
           rackunit
           web3
           "../examples/common.rkt"
           (only-in "util.rkt" anvil-alice anvil-bob))

  (define transfer "Transfer(address indexed from, address indexed to, uint256 value)")

  (cond
    [(not (current-anvil-path))
     (displayln "skipping block anvil tests: anvil not found (set WEB3_ANVIL or use `nix develop`)")]
    [else
     (with-anvil ()
       (parameterize ([current-account anvil-alice])
         (define-values (token _deployed)
           (deploy-contract/receipt (artifact-bytecode "Token") "constructor(string,string,uint256)"
                                    "Token" "TOK" 1000))
         (define sent (contract-send token "transfer(address,uint256)" anvil-bob 250))
         (define tx-hash (receipt-transaction-hash sent))

         (test-case "B9: a mined transfer reads back from its block"
           (define block
             (eth-get-block-by-number (receipt-block-number sent) #:full-transactions? #t))
           (check-equal? (block-number block) (receipt-block-number sent))
           (check-equal? (block-hash block) (receipt-block-hash sent))
           (check-pred exact-nonnegative-integer? (block-base-fee-per-gas block))
           (check-equal? (block-transaction-hashes block) (list tx-hash))
           (match-define (list info) (block-transactions block))
           (check-equal? info (eth-get-transaction-by-hash tx-hash))
           (check-equal? (transaction-info-block-number info) (receipt-block-number sent))
           (check-equal? (transaction-info-from info) anvil-alice)
           (check-equal? (transaction-info-to info) token)
           (check-equal? (transaction-info->signed-transaction info)
                         (bytes->signed-transaction
                          (hex-string->bytes (eth-get-raw-transaction-by-hash tx-hash))))
           (check-equal? (eth-get-block-by-hash (receipt-block-hash sent) #:full-transactions? #t)
                         block)
           (check-equal? (block-transactions (eth-get-block-by-number (block-number block)))
                         (list tx-hash))
           (match (receipt-logs sent #:events (list transfer))
             [(list (? event-log? decoded))
              (check-equal? (event-log-arguments decoded) (list anvil-alice anvil-bob 250))]
             [other (fail (format "~a" other))]))

         (test-case "a transaction by block and index is the block's transaction"
           (check-equal? (eth-get-transaction-by-block-number-and-index (receipt-block-number sent)
                                                                        0)
                         (eth-get-transaction-by-hash tx-hash)))))]))
