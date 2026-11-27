#lang racket/base

(require racket/contract/base
         (only-in racket/match match match-define)
         (only-in "../private/field-contracts.rkt" hash-string/c)
         (only-in "../private/node.rkt" raise-node-malformed)
         "../private/record.rkt"
         (only-in "transaction-info.rkt" transaction-info-hash transaction-info?)
         (only-in (submod "transaction-info.rkt" internal) jsexpr->transaction-info/who))

(provide (contract-out [block-transaction-hashes (-> block? (listof hash-string/c))]))

(define (write-withdrawal entry port _mode)
  (fprintf port "#<withdrawal ~a validator ~a ~a ~a gwei>"
           (withdrawal-index entry)
           (withdrawal-validator-index entry)
           (withdrawal-address entry)
           (withdrawal-amount entry)))

(define-node-record withdrawal #:noun "withdrawal" #:printer write-withdrawal
  [index uint64-field]
  [validator-index uint64-field #:key validatorIndex]
  [address address-field]
  [amount quantity-field])

(define read-hashes (field-kind-reader (list-field hash-field)))

(define read-transaction-infos
  (field-kind-reader (list-field (record-field jsexpr->transaction-info/who transaction-info?))))

(define (hash-strings? v)
  (and (list? v) (andmap string? v)))

(define (transaction-objects? v)
  (and (list? v) (andmap hash? v)))

(define (read-block-transactions who what here v)
  (cond
    [(hash-strings? v) (read-hashes who what here v)]
    [(transaction-objects? v) (read-transaction-infos who what here v)]
    [else (raise-node-malformed who (located what here) v)]))

(define block-transactions-field
  (field-kind 'block-transactions
              read-block-transactions
              (or/c (listof hash-string/c) (listof transaction-info?))))

(define (write-block record port _mode)
  (define total (length (block-transactions record)))
  (fprintf port "#<block ~a ~a ~a transaction~a>"
           (block-number record)
           (or (block-hash record) "pending")
           total
           (if (= total 1) "" "s")))

(define-node-record block #:noun "block" #:printer write-block
  [hash hash-field #:optional]
  [parent-hash hash-field #:key parentHash]
  [sha3-uncles hash-field #:key sha3Uncles #:optional]
  [miner address-field #:optional]
  [state-root hash-field #:key stateRoot]
  [transactions-root hash-field #:key transactionsRoot]
  [receipts-root hash-field #:key receiptsRoot]
  [logs-bloom (fixed-data-field 256) #:key logsBloom #:optional]
  [difficulty quantity-field #:optional]
  [number uint64-field]
  [gas-limit uint64-field #:key gasLimit]
  [gas-used uint64-field #:key gasUsed]
  [timestamp uint64-field]
  [extra-data data-field #:key extraData #:optional]
  [mix-hash hash-field #:key mixHash #:optional]
  [nonce (fixed-data-field 8) #:optional]
  [size uint64-field #:optional]
  [base-fee-per-gas quantity-field #:key baseFeePerGas #:optional]
  [withdrawals-root hash-field #:key withdrawalsRoot #:optional]
  [withdrawals (list-field (record-field jsexpr->withdrawal/who withdrawal?)) #:optional]
  [blob-gas-used uint64-field #:key blobGasUsed #:optional]
  [excess-blob-gas uint64-field #:key excessBlobGas #:optional]
  [parent-beacon-block-root hash-field #:key parentBeaconBlockRoot #:optional]
  [requests-hash hash-field #:key requestsHash #:optional]
  [uncles (list-field hash-field) #:optional]
  [transactions block-transactions-field])

(define (write-fee-history history port _mode)
  (define oldest (fee-history-oldest-block history))
  (match (length (fee-history-gas-used-ratio history))
    [0 (write-string "#<fee-history no blocks>" port)]
    [blocks (fprintf port "#<fee-history blocks ~a to ~a>" oldest (+ oldest blocks -1))]))

(define (entries-fit? entries blocks extra empty-allowed?)
  (or (not entries)
      (= (length entries) (+ blocks extra))
      (and empty-allowed? (null? entries))))

(define (check-fee-history who detail history)
  (define blocks (length (fee-history-gas-used-ratio history)))
  (for ([row (in-list fee-history-lengths)])
    (match-define (list entries-of key what extra empty-allowed?) row)
    (unless (entries-fit? (entries-of history) blocks extra empty-allowed?)
      (raise-node-malformed who (string-append "fee history " what detail)
                            (hash-ref (fee-history->jsexpr history) key)))))

(define-node-record fee-history #:noun "fee history" #:printer write-fee-history
  #:check check-fee-history
  [oldest-block uint64-field #:key oldestBlock]
  [base-fee-per-gas (list-field quantity-field) #:key baseFeePerGas]
  [gas-used-ratio (list-field ratio-field) #:key gasUsedRatio]
  [reward (list-field (list-field quantity-field)) #:optional]
  [base-fee-per-blob-gas (list-field quantity-field) #:key baseFeePerBlobGas #:optional]
  [blob-gas-used-ratio (list-field ratio-field) #:key blobGasUsedRatio #:optional])

(define fee-history-lengths
  `([,fee-history-base-fee-per-gas baseFeePerGas "base fee per gas" 1 #f]
    [,fee-history-reward reward "reward" 0 #t]
    [,fee-history-base-fee-per-blob-gas baseFeePerBlobGas "base fee per blob gas" 1 #f]
    [,fee-history-blob-gas-used-ratio blobGasUsedRatio "blob gas used ratio" 0 #f]))

(define (block-transaction-hashes record)
  (for/list ([entry (in-list (block-transactions record))])
    (if (string? entry) entry (transaction-info-hash entry))))

(define (jsexpr->block-of who v asked full?)
  (define answer (jsexpr->block/who who v))
  (check-block-asked who "block" asked (block-number answer) (block-hash answer))
  (if (andmap (if full? transaction-info? string?) (block-transactions answer))
      answer
      (raise-node-malformed who "block transactions" (hash-ref v 'transactions))))

(define (rewards-fit? reward blocks percentiles)
  (if (null? percentiles)
      (or (not reward) (andmap null? reward))
      (and reward
           (= (length reward) blocks)
           (andmap (lambda (row) (= (length row) (length percentiles))) reward))))

(define (jsexpr->fee-history-of who v _blocks _newest percentiles)
  (define history (jsexpr->fee-history/who who v))
  (define reward (fee-history-reward history))
  (if (rewards-fit? reward (length (fee-history-gas-used-ratio history)) percentiles)
      history
      (raise-node-malformed who "fee history reward" (hash-ref v 'reward 'null))))

(module* internal #f
  (provide block-fields
           block-noun
           fee-history-fields
           fee-history-noun
           jsexpr->block-of
           jsexpr->block/who
           jsexpr->fee-history-of
           withdrawal-fields
           withdrawal-noun))
