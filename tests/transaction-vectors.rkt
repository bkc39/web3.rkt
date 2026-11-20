#lang racket/base

(module+ test
  (require racket/list
           racket/match
           racket/port
           racket/runtime-path
           (only-in racket/string string-split)
           rackunit
           (only-in web3/rlp exn:fail:web3:rlp?)
           (only-in web3/rpc hex-string->bytes)
           web3/transaction)

  (define-runtime-path vectors-path "vectors/transaction.rktd")

  (define-values (header rows)
    (match (call-with-input-file vectors-path (lambda (in) (port->list read in)))
      [(cons header rows) (values header rows)]))

  (define manifest
    '(("ttEIP1559" 9) ("ttEIP2930" 7) ("ttSignature" 34) ("ttVValue" 28) ("ttRSValue" 29)
      ("ttWrongRLP" 59) ("ttNonce" 10) ("ttGasLimit" 10) ("ttGasPrice" 4) ("ttValue" 3)
      ("ttAddress" 4) ("ttData" 7)))

  (test-case "V0: the vectors are ethereum/tests at ff69b74, one test per file"
    (match header
      [`(source (repo "ethereum/tests") (commit ,commit) (path "TransactionTests")
                (licence ,licence) (generated-by ,_))
       (check-equal? commit "ff69b74f2eb44fbe1bf09627f06ebf69cc72ea67")
       (check-regexp-match #rx"^MIT" licence)])
    (check-equal? (length rows) (apply + (map second manifest)))
    (for ([entry (in-list manifest)])
      (match-define (list directory count) entry)
      (check-equal? (length (remove-duplicates
                             (for/list ([row (in-list rows)]
                                        #:when (equal? (first row) directory))
                               (second row))))
                    count
                    directory)))

  (define outcomes
    '((INTRINSIC_GAS_TOO_LOW accepted)
      (GASLIMIT_PRICE_PRODUCT_OVERFLOW accepted)
      (NONCE_TOO_BIG accepted)
      (INVALID_CHAINID other-chain)
      (RLP_ERROR_SIZE rlp-or-type)
      (RLP_ERROR_EOF rlp-or-type)
      (RLP_ERROR_SIZE_LEADING_ZEROS rlp-or-type)
      (RLP_LEADING_ZEROS_DATA_SIZE rlp-or-type)
      (RLP_LEADING_ZEROS_NONCE_SIZE rlp-or-type)
      (RLP_INVALID_HEADER rlp-or-type)
      (TYPE_NOT_SUPPORTED (raw (type) any))
      (RLP_TOO_FEW_ELEMENTS (raw (shape) (#f)))
      (RLP_TOO_MANY_ELEMENTS (raw (shape) (#f)))
      (RLP_INVALID_NONCE (rlp-or (raw (shape) (nonce))))
      (RLP_INVALID_GASLIMIT (rlp-or (raw (shape) (gas-limit))))
      (RLP_INVALID_DATA (rlp-or (raw (shape) (data))))
      (RLP_INVALID_TO (rlp-or (raw (shape) (to))))
      (RLP_INVALID_SIGNATURE_R (rlp-or (raw (shape) (r))))
      (RLP_INVALID_SIGNATURE_S (rlp-or (raw (shape) (s))))
      (RLP_LEADING_ZEROS_NONCE (raw (leading-zero) (nonce)))
      (RLP_LEADING_ZEROS_GASLIMIT (raw (leading-zero) (gas-limit)))
      (RLP_LEADING_ZEROS_GASPRICE (raw (leading-zero) (gas-price)))
      (RLP_LEADING_ZEROS_VALUE (raw (leading-zero) (value)))
      (RLP_LEADING_ZEROS_R (raw (leading-zero) (r)))
      (RLP_LEADING_ZEROS_S (raw (leading-zero) (s)))
      (RLP_LEADING_ZEROS_V (raw (leading-zero) (v)))
      (RLP_LEADING_ZEROS_BASEFEE (raw (leading-zero) (max-fee-per-gas)))
      (RLP_LEADING_ZEROS_PRIORITY_FEE (raw (leading-zero) (max-priority-fee-per-gas)))
      (NONCE_OVERFLOW (raw (range) (nonce)))
      (GASLIMIT_OVERFLOW (raw (range leading-zero) (gas-limit)))
      (VALUE_OVERFLOW (raw (range) (value)))
      (GASPRICE_OVERFLOW (raw (range) (gas-price max-fee-per-gas)))
      (PRIORITY_OVERFLOW (raw (range) (max-priority-fee-per-gas)))
      (PRIORITY_GREATER_THAN_MAX_FEE_PER_GAS (raw (range) (max-priority-fee-per-gas)))
      (PRIORITY_GREATER_THAN_MAX_FEE_PER_GAS_2 (raw (range) (max-priority-fee-per-gas)))
      (ADDRESS_TOO_SHORT (raw (length) (to)))
      (ADDRESS_TOO_LONG (raw (length) (to)))
      (RLP_INVALID_ACCESS_LIST_ADDRESS_TOO_SHORT (raw (length) (access-list)))
      (RLP_INVALID_ACCESS_LIST_ADDRESS_TOO_LONG (raw (length) (access-list)))
      (RLP_INVALID_ACCESS_LIST_STORAGE_TOO_SHORT (raw (length) (access-list)))
      (RLP_INVALID_ACCESS_LIST_STORAGE_TOO_LONG (raw (length) (access-list)))
      (INVALID_SIGNATURE_VRS (raw (range high-s) any))
      (EC_RECOVERY_FAIL (raw (no-sender) (#f)))))

  (define (parse bs)
    (with-handlers ([exn:fail:web3:rlp? values]
                    [exn:fail:web3:raw-transaction? values])
      (bytes->signed-transaction bs)))

  (define (check-accepted result bs name)
    (check-pred signed-transaction? result name)
    (when (signed-transaction? result)
      (check-equal? (signed-transaction->bytes result) bs name)))

  (define (expected-outcome kind outcome result bs name)
    (match* (outcome result)
      [('accepted _) (check-accepted result bs name)]
      [('other-chain (? signed-transaction?))
       (check-accepted result bs name)
       (check-false (memv (transaction-chain-id (signed-transaction-transaction result)) '(#f 1))
                    name)]
      [('other-chain _) (check-pred exn:fail:web3:raw-transaction? result name)]
      [('rlp-or-type (? exn:fail:web3:rlp?)) (void)]
      [('rlp-or-type _) (expected-outcome kind '(raw (type) any) result bs name)]
      [(`(rlp-or ,_) (? exn:fail:web3:rlp?)) (void)]
      [(`(rlp-or ,raw-outcome) _) (expected-outcome kind raw-outcome result bs name)]
      [(`(raw ,reasons ,fields) _)
       (check-pred exn:fail:web3:raw-transaction? result name)
       (when (exn:fail:web3:raw-transaction? result)
         (check-not-false (memq (exn:fail:web3:raw-transaction-reason result) reasons) name)
         (unless (eq? fields 'any)
           (check-not-false (memq (exn:fail:web3:raw-transaction-field result) fields) name)))]))

  (test-case "V1: every transaction upstream accepts parses to its sender and hash"
    (for ([row (in-list rows)])
      (match row
        [(list directory file test hex (list 'sender sender 'hash hash))
         (define bs (hex-string->bytes hex))
         (define result (parse bs))
         (define name (format "~a/~a ~a" directory file test))
         (check-accepted result bs name)
         (when (signed-transaction? result)
           (check-equal? (string-downcase (signed-transaction-sender result)) sender name)
           (check-equal? (signed-transaction-hash result) hash name))]
        [_ (void)])))

  (test-case "V2: every transaction upstream refuses is refused for the same reason or a node's"
    (for ([row (in-list rows)])
      (match row
        [(list directory file test hex (list 'exception text))
         (define kind (string->symbol (first (string-split text "|"))))
         (define name (format "~a/~a ~a: ~a" directory file test kind))
         (define bs (hex-string->bytes hex))
         (match (assq kind outcomes)
           [(list _ outcome) (expected-outcome kind outcome (parse bs) bs name)]
           [#f (fail (format "~a: no outcome for ~a" name kind))])]
        [_ (void)]))))
