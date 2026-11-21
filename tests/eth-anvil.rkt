#lang racket/base

(module+ test
  (require (only-in keccak keccak256)
           (only-in racket/match match-define)
           rackunit
           web3
           (only-in "util.rkt" filled-hash word))

  (define emit-log "0x60006000a000")
  (define store-arg "0x5f355f5500")

  (define contract "0x00000000000000000000000000000000000c0de0")
  (define stranger "0x000000000000000000000000000000000000beef")

  (define (first-account)
    (match-define (list me _ ...) (eth-accounts))
    me)

  (define (send-to to data)
    (eth-send-transaction (hasheq 'from (first-account) 'to to 'data data)))

  (define (mined-transfer)
    (define tx-hash (send-to stranger "0x"))
    (list tx-hash (wait-for-receipt tx-hash)))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping eth anvil tests: anvil not found (set WEB3_ANVIL or use `nix develop`)")]
    [else
     (test-case "A1: a raw transaction hashes to its hash"
       (with-anvil ()
         (match-define (list tx-hash _) (mined-transfer))
         (define raw (eth-get-raw-transaction-by-hash tx-hash))
         (check-equal? (bytes->hex-string (keccak256 (hex-string->bytes raw))) tx-hash)
         (check-equal? (eth-get-raw-transaction-by-hash (filled-hash #\a)) 'null)))

     (test-case "A2: a block by its hash is the block by its number"
       (with-anvil ()
         (match-define (list _ receipt) (mined-transfer))
         (for ([full? (in-list '(#f #t))])
           (check-equal? (eth-get-block-by-hash (receipt-block-hash receipt) full?)
                         (eth-get-block-by-number (receipt-block-number receipt) full?)))
         (check-false (eth-get-block-by-hash (filled-hash #\a) #f))))

     (test-case "A3: block receipts are the transactions' receipts"
       (with-anvil ()
         (match-define (list tx-hash receipt) (mined-transfer))
         (check-equal? (eth-get-block-receipts (receipt-block-number receipt))
                       (list (eth-get-transaction-receipt tx-hash)))
         (check-false (eth-get-block-receipts (+ (eth-block-number) 10)))))

     (test-case "A4: a transaction by block and index is the transaction by hash"
       (with-anvil ()
         (match-define (list tx-hash receipt) (mined-transfer))
         (define mined-in (receipt-block-hash receipt))
         (define height (receipt-block-number receipt))
         (define transaction (eth-get-transaction-by-hash tx-hash))
         (check-equal? (transaction-info-hash transaction) tx-hash)
         (check-equal? (transaction-info-block-hash transaction) mined-in)
         (check-equal? (eth-get-transaction-by-block-hash-and-index mined-in 0) transaction)
         (check-equal? (eth-get-transaction-by-block-number-and-index height 0) transaction)
         (check-false (eth-get-transaction-by-block-hash-and-index mined-in 1))
         (check-false (eth-get-transaction-by-block-number-and-index height 1))
         (check-false (eth-get-transaction-by-hash (filled-hash #\a)))
         (check-equal? (eth-get-block-transaction-count-by-hash mined-in) 1)))

     (test-case "A5: fee history covers the requested blocks"
       (with-anvil ()
         (for ([_ (in-range 3)])
           (anvil-mine))
         (define head (eth-block-number))
         (define history (eth-fee-history 2 "latest" '(25 75)))
         (check-equal? (fee-history-oldest-block history) (sub1 head))
         (check-equal? (length (fee-history-base-fee-per-gas history)) 3)
         (check-equal? (length (fee-history-gas-used-ratio history)) 2)
         (check-equal? (map length (fee-history-reward history)) '(2 2))))

     (test-case "A6: the blob base fee of a chain without blobs is 1"
       (with-anvil ()
         (check-equal? (eth-blob-base-fee) 1)))

     (test-case "A7: a proof carries the slot's value"
       (with-anvil ()
         (anvil-set-storage-at contract 3 (bytes->hex-string (word 99)))
         (define proof (eth-get-proof contract '(3) "latest"))
         (match-define (list (hash-table ('key key) ('value value) _ ...))
           (hash-ref proof 'storageProof))
         (check-equal? (list (hex-string->number key) (hex-string->number value)) '(3 99))
         (check-pred pair? (hash-ref proof 'accountProof))))

     (test-case "A8: an access list names the slot a call writes"
       (with-anvil ()
         (anvil-set-code contract store-arg)
         (define access
           (eth-create-access-list
            (hasheq 'from (first-account) 'to contract 'data (bytes->hex-string (word 7)))
            "latest"))
         (check-not-false (member (hasheq 'address (string-downcase contract)
                                          'storageKeys (list (filled-hash #\0)))
                                  (hash-ref access 'accessList)))))

     (test-case "A9: a log filter sees new logs once"
       (with-anvil ()
         (anvil-set-code contract emit-log)
         (define filter-id (eth-new-filter (log-filter->jsexpr (log-filter #:address contract))))
         (define tx-hash (send-to contract "0x"))
         (wait-for-receipt tx-hash)
         (check-equal? (map (lambda (entry) (hash-ref entry 'transactionHash))
                            (eth-get-filter-changes filter-id))
                       (list tx-hash))
         (check-equal? (eth-get-filter-changes filter-id) '())
         (check-equal? (length (eth-get-filter-logs filter-id)) 1)))

     (test-case "A10: a block filter sees mined blocks"
       (with-anvil ()
         (define filter-id (eth-new-block-filter))
         (anvil-mine)
         (check-equal? (eth-get-filter-changes filter-id)
                       (list (block-hash (eth-get-block-by-number "latest" #f))))))

     (test-case "A11: a pending filter sees pending transactions"
       (with-anvil (#:args '("--no-mining"))
         (define filter-id (eth-new-pending-transaction-filter))
         (define tx-hash (send-to stranger "0x"))
         (check-equal? (eth-get-filter-changes filter-id) (list tx-hash))))

     (test-case "A12: uninstalling a filter twice gives #t then #f"
       (with-anvil ()
         (define filter-id (eth-new-block-filter))
         (check-true (eth-uninstall-filter filter-id))
         (check-false (eth-uninstall-filter filter-id))))]))
