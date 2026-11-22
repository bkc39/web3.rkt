#lang racket/base

(require racket/contract/base
         (only-in racket/match match)
         (only-in "../account.rkt" signature)
         (only-in "../private/exn.rkt" raise-web3-error)
         (only-in "../private/node.rkt" raise-node-malformed raise-node-missing)
         "../private/record.rkt"
         "../private/who.rkt"
         (only-in "../transaction.rkt"
                  signed-transaction-hash
                  signed-transaction-sender
                  signed-transaction?)
         (only-in (submod "../transaction.rkt" internal)
                  curve-scalar?
                  fee-caps-ordered?
                  fields->transaction
                  legacy-v?
                  signed-transaction/who
                  v-chain-id
                  v-y-parity
                  y-parity?))

(provide (contract-out
          [transaction-info->signed-transaction (-> transaction-info? signed-transaction?)]))

(define (write-transaction-info info port _mode)
  (define type (transaction-info-type info))
  (fprintf port "#<transaction-info ~a~a nonce ~a block ~a index ~a>"
           (transaction-info-hash info)
           (if type (format " type ~a" type) "")
           (transaction-info-nonce info)
           (or (transaction-info-block-number info) "pending")
           (or (transaction-info-transaction-index info) "pending")))

(define-node-record transaction-info #:noun "transaction" #:printer write-transaction-info
  [hash hash-field]
  [block-hash hash-field #:key blockHash #:optional]
  [block-number uint64-field #:key blockNumber #:optional]
  [block-timestamp lenient-uint64-field #:key blockTimestamp #:optional]
  [transaction-index uint64-field #:key transactionIndex #:optional]
  [from address-field]
  [type byte-field #:optional]
  [chain-id uint64-field #:key chainId #:optional]
  [nonce uint64-field]
  [to address-field #:optional]
  [value quantity-field]
  [data data-field #:key input]
  [gas-limit uint64-field #:key gas]
  [gas-price quantity-field #:key gasPrice #:optional]
  [max-priority-fee-per-gas quantity-field #:key maxPriorityFeePerGas #:optional]
  [max-fee-per-gas quantity-field #:key maxFeePerGas #:optional]
  [max-fee-per-blob-gas quantity-field #:key maxFeePerBlobGas #:optional]
  [access-list access-list-field #:key accessList #:optional]
  [blob-versioned-hashes (list-field hash-field) #:key blobVersionedHashes #:optional]
  [v quantity-field #:optional]
  [y-parity bit-field #:key yParity #:optional]
  [r quantity-field #:optional]
  [s quantity-field #:optional])

(define (required-field who value what)
  (or value (raise-node-missing who what)))

(define (typed-only-field? info)
  (for/or ([field (in-list (list transaction-info-max-fee-per-gas
                                 transaction-info-max-priority-fee-per-gas
                                 transaction-info-access-list
                                 transaction-info-y-parity))])
    (field info)))

(define (supported-type who info)
  (match (transaction-info-type info)
    [(and type (or 0 1 2)) type]
    [#f #:when (typed-only-field? info) (raise-node-missing who "transaction type")]
    [#f 0]
    [type (raise-web3-error who "type ~a is not supported" type)]))

(define (v-parity who type info)
  (define legacy-v (transaction-info-v info))
  (cond
    [(not legacy-v) #f]
    [(and (zero? type) (legacy-v? legacy-v)) (v-y-parity legacy-v)]
    [(and (positive? type) (y-parity? legacy-v)) legacy-v]
    [else (raise-node-malformed who "transaction v" legacy-v)]))

(define (y-parity-of who type info)
  (define parity (transaction-info-y-parity info))
  (define from-v (v-parity who type info))
  (if (and parity from-v (not (= parity from-v)))
      (raise-node-malformed who "transaction y parity" parity)
      (required-field who (or parity from-v) "transaction y parity")))

(define (chain-id-of who type info)
  (define chain-id (transaction-info-chain-id info))
  (define legacy-v (transaction-info-v info))
  (cond
    [(or (positive? type) (not legacy-v)) chain-id]
    [(and chain-id (not (eqv? chain-id (v-chain-id legacy-v))))
     (raise-node-malformed who "transaction chain id" chain-id)]
    [else (v-chain-id legacy-v)]))

(define (required-chain-id who chain-id)
  (required-field who chain-id "transaction chain id"))

(define (gas-price-of who info)
  (required-field who (transaction-info-gas-price info) "transaction gas price"))

(define (access-list-of who info)
  (required-field who (transaction-info-access-list info) "transaction access list"))

(define (fee-caps-of who info)
  (define priority (required-field who (transaction-info-max-priority-fee-per-gas info)
                                   "transaction max priority fee per gas"))
  (define max-fee
    (required-field who (transaction-info-max-fee-per-gas info) "transaction max fee per gas"))
  (if (fee-caps-ordered? priority max-fee)
      (hasheq 'max-priority-fee-per-gas priority 'max-fee-per-gas max-fee)
      (raise-node-malformed who "transaction max priority fee per gas" priority)))

(define (type-fields who type chain-id info)
  (match type
    [0 (hasheq 'chain-id chain-id 'gas-price (gas-price-of who info))]
    [1 (hasheq 'chain-id (required-chain-id who chain-id)
               'gas-price (gas-price-of who info)
               'access-list (access-list-of who info))]
    [2 (hash-set* (fee-caps-of who info)
                  'chain-id (required-chain-id who chain-id)
                  'access-list (access-list-of who info))]))

(define (unsigned-transaction who type chain-id info)
  (fields->transaction type
                       (hash-set* (type-fields who type chain-id info)
                                  'nonce (transaction-info-nonce info)
                                  'gas-limit (transaction-info-gas-limit info)
                                  'to (transaction-info-to info)
                                  'value (transaction-info-value info)
                                  'data (transaction-info-data info))))

(define (curve-scalar who value what)
  (if (curve-scalar? (required-field who value what))
      value
      (raise-node-malformed who what value)))

(define (check-claim who claim claimed source actual)
  (unless (equal? claimed actual)
    (raise-web3-error who "the node's ~a ~a is not ~a ~a" claim claimed source actual)))

(define/who (transaction-info->signed-transaction who info)
  (define type (supported-type who info))
  (define y-parity (y-parity-of who type info))
  (define transaction (unsigned-transaction who type (chain-id-of who type info) info))
  (define signature-r (curve-scalar who (transaction-info-r info) "transaction r"))
  (define signature-s (curve-scalar who (transaction-info-s info) "transaction s"))
  (define signed
    (signed-transaction/who who transaction (signature signature-r signature-s y-parity)))
  (check-claim who "hash" (transaction-info-hash info) "the transaction's"
               (signed-transaction-hash signed))
  (check-claim who "from" (transaction-info-from info) "the signature's sender"
               (signed-transaction-sender signed))
  signed)

(define jsexpr->transaction-info-of
  (asked-reader jsexpr->transaction-info/who transaction-info-hash "transaction hash"))

(define (jsexpr->transaction-info-in who v block index)
  (define info (jsexpr->transaction-info/who who v))
  (check-block-asked who "transaction block" block
                     (transaction-info-block-number info) (transaction-info-block-hash info))
  (check-asked who "transaction transaction index" index (transaction-info-transaction-index info))
  info)

(module* internal #f
  (provide jsexpr->transaction-info-in
           jsexpr->transaction-info-of
           jsexpr->transaction-info/who
           transaction-info-fields
           transaction-info-noun))
