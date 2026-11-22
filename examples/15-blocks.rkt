#lang racket/base

(require racket/list
         racket/match
         web3)

(define (print-transaction tx)
  (printf "  type ~a nonce ~a, ~a ether from ~a\n"
          (transaction-info-type tx)
          (transaction-info-nonce tx)
          (wei->string (transaction-info-value tx))
          (transaction-info-from tx)))

(module+ main
  (with-anvil (#:args '("--no-mining"))
    (match-define (list alice bob _ ...) (eth-accounts))
    (send-transaction alice bob #:value (ether->wei 1) #:type 0)
    (send-transaction alice bob #:value (ether->wei 2) #:type 1)
    (send-transaction alice bob #:value (ether->wei 3)
                      #:max-priority-fee-per-gas (gwei->wei 2) #:max-fee-per-gas (gwei->wei 5))
    (anvil-mine)
    (define block (eth-get-block-by-number "latest" #:full-transactions? #t))
    (define transactions (block-transactions block))
    (printf "block ~a, gas used ~a, base fee ~a gwei, ~a transactions\n"
            (block-number block)
            (block-gas-used block)
            (wei->string (block-base-fee-per-gas block) 'gwei)
            (length transactions))
    (for-each print-transaction transactions)
    (match-define (list legacy _ _) transactions)
    (printf "legacy transaction rebuilt, signed by ~a\n"
            (signed-transaction-sender (transaction-info->signed-transaction legacy)))
    (define history (eth-fee-history 2 "latest" '(25 75)))
    (for ([height (in-naturals (fee-history-oldest-block history))]
          [rewards (in-list (fee-history-reward history))])
      (match-define (list low high) rewards)
      (printf "fee history, block ~a: priority fees ~a and ~a gwei\n"
              height (wei->string low 'gwei) (wei->string high 'gwei)))
    (printf "next base fee ~a gwei\n"
            (wei->string (last (fee-history-base-fee-per-gas history)) 'gwei))))
