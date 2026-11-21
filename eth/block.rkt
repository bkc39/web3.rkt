#lang racket/base

(require racket/contract/base
         (only-in racket/match match)
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

(define (read-block-transactions who what detail v)
  (cond
    [(and (list? v) (andmap string? v)) (read-hashes who what detail v)]
    [(and (list? v) (andmap hash? v)) (read-transaction-infos who what detail v)]
    [else (raise-node-malformed who (string-append what detail) v)]))

(define block-transactions-field
  (field-kind 'block-transactions
              read-block-transactions
              (or/c (listof hash-string/c) (listof transaction-info?))))

(define (write-block block port _mode)
  (define total (length (block-transactions block)))
  (fprintf port "#<block ~a ~a ~a transaction~a>"
           (block-number block)
           (or (block-hash block) "pending")
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
  [logs-bloom (fixed-data-field 256) #:key logsBloom]
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

(define (check-fee-history who detail history)
  (define base-fees (fee-history-base-fee-per-gas history))
  (unless (= (length base-fees) (add1 (length (fee-history-gas-used-ratio history))))
    (raise-node-malformed who (string-append "fee history base fee per gas" detail)
                          (hash-ref (fee-history->jsexpr history) 'baseFeePerGas))))

(define-node-record fee-history #:noun "fee history" #:printer write-fee-history
  #:check check-fee-history
  [oldest-block uint64-field #:key oldestBlock]
  [base-fee-per-gas (list-field quantity-field) #:key baseFeePerGas]
  [gas-used-ratio (list-field ratio-field) #:key gasUsedRatio]
  [reward (list-field (list-field quantity-field)) #:optional]
  [base-fee-per-blob-gas (list-field quantity-field) #:key baseFeePerBlobGas #:optional]
  [blob-gas-used-ratio (list-field ratio-field) #:key blobGasUsedRatio #:optional])

(define (block-transaction-hashes block)
  (for/list ([entry (in-list (block-transactions block))])
    (if (string? entry) entry (transaction-info-hash entry))))

(define (jsexpr->block-of who _block full? v)
  (define block (jsexpr->block/who who v))
  (define entries (block-transactions block))
  (if (andmap (if full? transaction-info? string?) entries)
      block
      (raise-node-malformed who "block transactions" (hash-ref v 'transactions))))

(module* internal #f
  (provide block-fields
           fee-history-fields
           jsexpr->block-of
           jsexpr->block/who
           jsexpr->fee-history/who
           withdrawal-fields))
