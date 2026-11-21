#lang racket/base

(module+ test
  (require racket/format
           (only-in racket/file file->string)
           (only-in racket/match match match-define)
           rackunit
           (only-in web3/eth eth-get-block-by-hash eth-get-block-by-number)
           web3/eth/block
           (only-in (submod web3/eth/block internal) block-fields fee-history-fields withdrawal-fields)
           (only-in web3/eth/transaction-info jsexpr->transaction-info transaction-info->jsexpr)
           (only-in web3/private/record field-row-field field-row-provided?)
           (only-in web3/rpc exn:fail:web3? mock-provider)
           (only-in "helpers/results.rkt"
                    block-json
                    fee-history-json
                    full-block-json
                    pending-block-json
                    transaction-info-json/0
                    withdrawal-json)
           "modules.rkt"
           "util.rkt")

  (define mined-hash (hash-ref block-json 'hash))
  (define transaction-hashes (hash-ref block-json 'transactions))
  (match-define (list first-hash _ ...) transaction-hashes)
  (define zero-hash (filled-hash #\0))

  (define (message-of thunk)
    (define e (raised thunk))
    (check-pred exn:fail:web3? e)
    (check-false (exn:fail:contract? e))
    (and e (exn-message e)))

  (define expected-block
    `([hash ,mined-hash]
      [parent-hash "0xcf3edcafefb348d27cb299d18f73cbcc4e93a912168b6291369a3fc8e0580e5b"]
      [sha3-uncles "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347"]
      [miner "0x0000000000000000000000000000000000000000"]
      [state-root "0xb1b8d88a4b0a2acaaa60e14245dc7d794bcce5a13d918af30890c565db667608"]
      [transactions-root "0x681c8e89df028e0c0d0fb8c3c3dc8a4185b9ea50aab92f381704fd7cfa5a1f52"]
      [receipts-root "0xdfeff9dad83b6f7eb62afe83ba8adff1d0951e31f085553500598d2a77725114"]
      [logs-bloom ,(make-bytes 256 0)]
      [difficulty 0]
      [number 1]
      [gas-limit 30000000]
      [gas-used #x106e4]
      [timestamp #x6b02118f]
      [extra-data #""]
      [mix-hash "0x924439e964bc6aae3d7b1c037a9119caf7652e53ed5582cfe279dcae20bab02b"]
      [nonce ,(make-bytes 8 0)]
      [size #x3ec]
      [base-fee-per-gas 1000000000]
      [withdrawals-root "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"]
      [withdrawals ()]
      [blob-gas-used 0]
      [excess-blob-gas 0]
      [parent-beacon-block-root ,zero-hash]
      [requests-hash "0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"]
      [uncles ()]
      [transactions ,transaction-hashes]))

  (define expected-withdrawal
    '([index 7]
      [validator-index 42]
      [address "0x70997970C51812dc3A010C7d01b50e0d17dc79C8"]
      [amount 1000000000]))

  (define (check-fields record prefix rows expected)
    (for ([row (in-list (filter field-row-provided? rows))])
      (define name (string->symbol (~a prefix "-" (field-row-field row))))
      (match (assq (field-row-field row) expected)
        [(list _ value)
         (check-equal? ((dynamic-require 'web3 name) record) value (~a name))]
        [#f (fail (~a "no expected value for " name))])))

  (test-case "B1: a node's block and withdrawal read into their fields"
    (define block (jsexpr->block block-json))
    (check-fields block "block" block-fields expected-block)
    (check-eq? (block->jsexpr block) block-json)
    (define entry (jsexpr->withdrawal withdrawal-json))
    (check-fields entry "withdrawal" withdrawal-fields expected-withdrawal)
    (check-eq? (withdrawal->jsexpr entry) withdrawal-json)
    (check-equal? (block-withdrawals (jsexpr->block (hash-set block-json 'withdrawals
                                                              (list withdrawal-json))))
                  (list entry)))

  (test-case "B4: a pending block has no hash, miner or nonce"
    (define pending (jsexpr->block pending-block-json))
    (check-equal? (map (lambda (field) (field pending)) (list block-hash block-miner block-nonce))
                  '(#f #f #f))
    (check-equal? (~a pending) "#<block 1 pending 3 transactions>"))

  (test-case "B5: a block's transactions are hashes or transactions, never both"
    (define hashes (jsexpr->block block-json))
    (define full (jsexpr->block full-block-json))
    (check-equal? (block-transactions hashes) transaction-hashes)
    (check-equal? (block-transaction-hashes hashes) transaction-hashes)
    (check-equal? (block-transaction-hashes full) transaction-hashes)
    (for ([entry (in-list (block-transactions full))]
          [json (in-list (hash-ref full-block-json 'transactions))])
      (check-eq? (transaction-info->jsexpr entry) json))
    (define no-transactions (jsexpr->block (hash-set block-json 'transactions '())))
    (check-equal? (block-transaction-hashes no-transactions) '())
    (check-regexp-match #rx"^jsexpr->block: malformed block transactions from the node: "
                        (message-of (lambda ()
                                      (jsexpr->block
                                       (hash-set block-json 'transactions
                                                 (list first-hash
                                                       transaction-info-json/0)))))))

  (define (answering method result)
    (mock-provider (hash method result)))

  (test-case "B6: a block's transactions agree with the form asked for"
    (check-equal? (eth-get-block-by-number 1 #f
                                           #:provider (answering "eth_getBlockByNumber" block-json))
                  (jsexpr->block block-json))
    (check-equal? (eth-get-block-by-hash mined-hash #t
                                         #:provider (answering "eth_getBlockByHash" full-block-json))
                  (jsexpr->block full-block-json))
    (check-false (eth-get-block-by-number 9 #t #:provider (answering "eth_getBlockByNumber" 'null)))
    (for ([row (in-list `([#t ,block-json] [#f ,full-block-json]))])
      (match-define (list full? json) row)
      (check-regexp-match #rx"^eth_getBlockByNumber: malformed block transactions from the node: "
                          (message-of (lambda ()
                                        (eth-get-block-by-number
                                         1 full? #:provider (answering "eth_getBlockByNumber" json)))))
      (check-regexp-match #rx"^eth_getBlockByHash: malformed block transactions from the node: "
                          (message-of (lambda ()
                                        (eth-get-block-by-hash
                                         mined-hash full?
                                         #:provider (answering "eth_getBlockByHash" json))))))
    (check-equal? (eth-get-block-by-number 1 #t
                                           #:provider (answering "eth_getBlockByNumber"
                                                                 (hash-set block-json 'transactions '())))
                  (jsexpr->block (hash-set block-json 'transactions '()))))

  (test-case "B7: a bad withdrawal names the withdrawal"
    (check-equal? (message-of (lambda ()
                                (jsexpr->block
                                 (hash-set block-json 'withdrawals
                                           (list withdrawal-json
                                                 (hash-set withdrawal-json 'amount "zz"))))))
                  "jsexpr->block: malformed withdrawal amount in block withdrawals 1 from the node: \"zz\""))

  (test-case "T6: blocks and withdrawals print in every mode"
    (for ([row (in-list `([,(jsexpr->block block-json)
                           ,(~a "#<block 1 " mined-hash " 3 transactions>")]
                          [,(jsexpr->block (hash-set block-json 'transactions (list zero-hash)))
                           ,(~a "#<block 1 " mined-hash " 1 transaction>")]
                          [,(jsexpr->withdrawal withdrawal-json)
                           "#<withdrawal 7 validator 42 0x70997970C51812dc3A010C7d01b50e0d17dc79C8 1000000000 gwei>"]
                          [,(jsexpr->fee-history fee-history-json) "#<fee-history blocks 0 to 1>"]
                          [,(jsexpr->fee-history (hasheq 'oldestBlock "0x5" 'baseFeePerGas '("0x7")
                                                         'gasUsedRatio '()))
                           "#<fee-history no blocks>"]))])
      (match-define (list value text) row)
      (for ([mode (in-list (list ~a ~s ~v))])
        (check-equal? (mode value) text)
        (check-equal? (mode (list value)) (if (eq? mode ~v) (~a "(list " text ")") (~a "(" text ")"))))))

  (test-case "T6: block procedures blame their caller"
    (check-blames-here (block-number 42))
    (check-blames-here (block->jsexpr (hasheq)))
    (check-blames-here (block-transaction-hashes block-json))
    (check-blames-here (withdrawal-amount withdrawal-json))
    (check-blames-here (fee-history-reward fee-history-json)))

  (test-case "fee history reads into its fields"
    (define history (jsexpr->fee-history fee-history-json))
    (check-equal? (list (fee-history-oldest-block history)
                        (fee-history-base-fee-per-gas history)
                        (fee-history-gas-used-ratio history)
                        (fee-history-reward history)
                        (fee-history-base-fee-per-blob-gas history)
                        (fee-history-blob-gas-used-ratio history))
                  '(0 (1000000000 1000000000 875560834) (0.0 0.0022433333333333333)
                      ((0 0) (2000000000 29000000000)) (1 1 1) (0.0 0.0)))
    (check-eq? (fee-history->jsexpr history) fee-history-json)
    (check-false (fee-history-reward (jsexpr->fee-history (hash-remove fee-history-json 'reward)))))

  (test-case "B8: a fee history has one more base fee than it has blocks"
    (for ([base-fees (in-list '(("0x1" "0x1") ("0x1" "0x1" "0x1" "0x1")))])
      (check-equal? (message-of (lambda ()
                                  (jsexpr->fee-history
                                   (hash-set fee-history-json 'baseFeePerGas base-fees))))
                    (~a "jsexpr->fee-history: malformed fee history base fee per gas from the node: "
                        (~e base-fees))))
    (for ([ratio (in-list '(1.5 -0.1))])
      (check-regexp-match #rx"^jsexpr->fee-history: malformed fee history gas used ratio 1 from"
                          (message-of (lambda ()
                                        (jsexpr->fee-history
                                         (hash-set fee-history-json 'gasUsedRatio
                                                   (list 0.0 ratio))))))))

  (test-case "every accessor the field tables provide is in web3 and the manual"
    (define manual (file->string (collection-file-path "scribblings/web3.scrbl" "web3")))
    (define names (exported-names 'web3))
    (define (accessors prefix rows)
      (for/list ([row (in-list (filter field-row-provided? rows))])
        (string->symbol (~a prefix "-" (field-row-field row)))))
    (for ([name (in-list (append (accessors "block" block-fields)
                                 (accessors "withdrawal" withdrawal-fields)
                                 (accessors "fee-history" fee-history-fields)
                                 '(block? jsexpr->block block->jsexpr block-transaction-hashes
                                   withdrawal? jsexpr->withdrawal withdrawal->jsexpr
                                   fee-history? jsexpr->fee-history fee-history->jsexpr)))])
      (check-not-false (memq name names) (~a name))
      (check-not-false (regexp-match? (regexp (~a "@defproc\\[\\(" (regexp-quote (~a name)) " "))
                                      manual)
                       (~a name)))
    (for ([name (in-list (exported-names 'web3/eth/block))])
      (check-not-false (memq name names) (~a name)))))
