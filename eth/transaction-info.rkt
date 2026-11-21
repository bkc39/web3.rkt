#lang racket/base

(require racket/contract/base
         (only-in racket/match match)
         (only-in "../account.rkt" signature)
         "../private/exn.rkt"
         (only-in "../private/node.rkt" raise-node-malformed raise-node-missing)
         "../private/record.rkt"
         (only-in "../transaction.rkt"
                  eip1559-transaction
                  eip2930-transaction
                  legacy-transaction
                  signed-transaction
                  signed-transaction-hash
                  signed-transaction-sender
                  signed-transaction?)
         (only-in (submod "../transaction.rkt" internal)
                  curve-scalar?
                  legacy-v?
                  v-chain-id
                  v-y-parity))

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

(define converter 'transaction-info->signed-transaction)

(define (raise-converter fmt . arguments)
  (raise (exn:fail:web3 (apply format (string-append "~a: " fmt) converter arguments)
                        (current-continuation-marks))))

(define (needed value what)
  (or value (raise-node-missing converter what)))

(define (supported-type info)
  (match (transaction-info-type info)
    [(and type (or 0 1 2)) type]
    [#f (raise-node-missing converter "transaction type")]
    [type (raise-converter "type ~a is not supported" type)]))

(define (chain-id-of type info)
  (define chain-id (transaction-info-chain-id info))
  (define v (transaction-info-v info))
  (cond
    [(or (positive? type) (not v)) chain-id]
    [(and chain-id (not (eqv? chain-id (v-chain-id v))))
     (raise-node-malformed converter "transaction chain id" chain-id)]
    [else (v-chain-id v)]))

(define (v-parity type v)
  (cond
    [(zero? type) (and (legacy-v? v) (v-y-parity v))]
    [(<= v 1) v]
    [else #f]))

(define (y-parity-of type info)
  (define parity (transaction-info-y-parity info))
  (define v (transaction-info-v info))
  (define from-v (and v (v-parity type v)))
  (cond
    [(and v (not from-v)) (raise-node-malformed converter "transaction y parity" v)]
    [(and parity from-v (not (= parity from-v)))
     (raise-node-malformed converter "transaction y parity" parity)]
    [else (needed (or parity from-v) "transaction y parity")]))

(define (fee-caps info)
  (define priority (needed (transaction-info-max-priority-fee-per-gas info)
                           "transaction max priority fee per gas"))
  (define max-fee (needed (transaction-info-max-fee-per-gas info) "transaction max fee per gas"))
  (if (> priority max-fee)
      (raise-node-malformed converter "transaction max priority fee per gas" priority)
      (values priority max-fee)))

(define (unsigned-transaction type chain-id info)
  (define nonce (transaction-info-nonce info))
  (define gas-limit (transaction-info-gas-limit info))
  (define to (transaction-info-to info))
  (define value (transaction-info-value info))
  (define data (transaction-info-data info))
  (match type
    [0
     (legacy-transaction #:chain-id chain-id #:nonce nonce
                         #:gas-price (needed (transaction-info-gas-price info)
                                             "transaction gas price")
                         #:gas-limit gas-limit #:to to #:value value #:data data)]
    [1
     (eip2930-transaction #:chain-id (needed chain-id "transaction chain id") #:nonce nonce
                          #:gas-price (needed (transaction-info-gas-price info)
                                              "transaction gas price")
                          #:gas-limit gas-limit #:to to #:value value #:data data
                          #:access-list (needed (transaction-info-access-list info)
                                                "transaction access list"))]
    [2
     (define required-chain-id (needed chain-id "transaction chain id"))
     (define-values (priority max-fee) (fee-caps info))
     (eip1559-transaction #:chain-id required-chain-id #:nonce nonce
                          #:max-priority-fee-per-gas priority #:max-fee-per-gas max-fee
                          #:gas-limit gas-limit #:to to #:value value #:data data
                          #:access-list (needed (transaction-info-access-list info)
                                                "transaction access list"))]))

(define (curve-scalar value what)
  (cond
    [(curve-scalar? (needed value what)) value]
    [else (raise-node-malformed converter what value)]))

(define (check-claim claim claimed source actual)
  (unless (equal? claimed actual)
    (raise-converter "the node's ~a ~a is not ~a ~a" claim claimed source actual)))

(define (transaction-info->signed-transaction info)
  (define type (supported-type info))
  (define chain-id (chain-id-of type info))
  (define y-parity (y-parity-of type info))
  (define tx (unsigned-transaction type chain-id info))
  (define r (curve-scalar (transaction-info-r info) "transaction r"))
  (define s (curve-scalar (transaction-info-s info) "transaction s"))
  (define stx (signed-transaction tx (signature r s y-parity)))
  (check-claim "hash" (transaction-info-hash info) "the transaction's"
               (signed-transaction-hash stx))
  (check-claim "from" (transaction-info-from info) "the signature's sender"
               (signed-transaction-sender stx))
  stx)

(define jsexpr->transaction-info-of
  (asked-reader jsexpr->transaction-info/who transaction-info-hash "transaction hash"))

(module* internal #f
  (provide jsexpr->transaction-info-of
           jsexpr->transaction-info/who
           transaction-info-fields))
