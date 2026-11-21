#lang racket/base

(module+ test
  (require racket/list
           racket/match
           racket/port
           racket/runtime-path
           (only-in racket/string string-split)
           rackunit
           (only-in web3/rlp exn:fail:web3:rlp-offset exn:fail:web3:rlp-reason exn:fail:web3:rlp?)
           (only-in web3/rpc hex-string->bytes)
           web3/transaction)

  (define-runtime-path vectors-path "vectors/transaction.rktd")
  (define-runtime-path outcomes-path "vectors/transaction-outcomes.rktd")

  (define-values (header rows)
    (match (call-with-input-file vectors-path (lambda (in) (port->list read in)))
      [(cons header rows) (values header rows)]))

  (define pinned-entries
    (append* (for/list ([group (in-list (call-with-input-file outcomes-path read))])
               (match-define (cons outcome names) group)
               (for/list ([name (in-list names)])
                 (cons name outcome)))))

  (define pinned (make-immutable-hasheq pinned-entries))

  (define (row-name row)
    (match row
      [(list _ file _ _ _) (string->symbol (regexp-replace #rx"[.]json$" file ""))]))

  (define manifest
    '(("ttEIP1559" 9) ("ttEIP2930" 7) ("ttSignature" 34) ("ttVValue" 28) ("ttRSValue" 29)
      ("ttWrongRLP" 59) ("ttNonce" 10) ("ttGasLimit" 10) ("ttGasPrice" 4) ("ttValue" 3)
      ("ttAddress" 4) ("ttData" 7)))

  (test-case "V0: the vectors are ethereum/tests at ff69b74, one test per file, each pinned once"
    (match header
      [`(source (repo "ethereum/tests") (commit ,commit) (path "TransactionTests")
                (licence ,licence) (generated-by ,_))
       (check-equal? commit "ff69b74f2eb44fbe1bf09627f06ebf69cc72ea67")
       (check-regexp-match #rx"^MIT" licence)])
    (check-equal? (length rows)
                  (for/sum ([entry (in-list manifest)])
                    (match entry
                      [(list _ count) count])))
    (for ([entry (in-list manifest)])
      (match-define (list directory count) entry)
      (check-equal? (for/sum ([row (in-list rows)])
                      (match row
                        [(list (== directory) _ _ _ _) 1]
                        [_ 0]))
                    count
                    directory))
    (define names (map row-name rows))
    (check-equal? (check-duplicates names) #f)
    (check-equal? (hash-count pinned) (length pinned-entries) "a vector pinned twice")
    (check-equal? (remove* names (hash-keys pinned)) '() "pinned, but no such vector")
    (check-equal? (remove* (hash-keys pinned) names) '() "vectors with no pinned outcome"))

  (define (outcome-of bs)
    (with-handlers ([exn:fail:web3:rlp?
                     (lambda (e)
                       `(rlp ,(exn:fail:web3:rlp-reason e) ,(exn:fail:web3:rlp-offset e)))]
                    [exn:fail:web3:raw-transaction?
                     (lambda (e)
                       `(raw ,(exn:fail:web3:raw-transaction-reason e)
                             ,(exn:fail:web3:raw-transaction-field e)))])
      (define stx (bytes->signed-transaction bs))
      (check-equal? (signed-transaction->bytes stx) bs)
      `(accepted ,(transaction-chain-id (signed-transaction-transaction stx)))))

  (test-case "V1: every transaction upstream accepts parses to its sender and hash"
    (for ([row (in-list rows)])
      (match row
        [(list _ _ _ hex (list 'sender sender 'hash hash))
         (define stx (bytes->signed-transaction (hex-string->bytes hex)))
         (define name (symbol->string (row-name row)))
         (check-equal? (signed-transaction->bytes stx) (hex-string->bytes hex) name)
         (check-equal? (string-downcase (signed-transaction-sender stx)) sender name)
         (check-equal? (signed-transaction-hash stx) hash name)]
        [_ (void)])))

  (test-case "V2: every vector parses to its pinned outcome"
    (for ([row (in-list rows)])
      (match-define (list _ _ _ hex _) row)
      (define name (row-name row))
      (check-equal? (outcome-of (hex-string->bytes hex)) (hash-ref pinned name #f)
                    (symbol->string name))))

  (define kinds
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

  (define (agrees? kind outcome)
    (match* (kind outcome)
      [('accepted `(accepted ,_)) #t]
      [('other-chain `(accepted ,chain-id)) (not (memv chain-id '(#f 1)))]
      [('other-chain `(raw ,_ ,_)) #t]
      [('rlp-or-type `(rlp ,_ ,_)) #t]
      [('rlp-or-type _) (agrees? '(raw (type) any) outcome)]
      [(`(rlp-or ,_) `(rlp ,_ ,_)) #t]
      [(`(rlp-or ,raw-kind) _) (agrees? raw-kind outcome)]
      [(`(raw ,reasons any) `(raw ,reason ,_)) (and (memq reason reasons) #t)]
      [(`(raw ,reasons ,fields) `(raw ,reason ,field))
       (and (memq reason reasons) (memq field fields) #t)]
      [(_ _) #f]))

  (test-case "V3: every pinned refusal is upstream's reason or a node's"
    (for ([row (in-list rows)])
      (match row
        [(list _ _ _ _ (list 'exception text))
         (match-define (cons kind-text _) (string-split text "|"))
         (define kind (string->symbol kind-text))
         (define name (row-name row))
         (match (assq kind kinds)
           [(list _ allowed)
            (check-true (agrees? allowed (hash-ref pinned name))
                        (format "~a: ~a" name kind))]
           [#f (fail (format "~a: no outcome for ~a" name kind))])]
        [_ (void)]))))
