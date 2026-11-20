#lang racket/base

(module+ test
  (require (only-in json jsexpr->string string->jsexpr)
           racket/list
           racket/match
           rackunit
           (only-in libsecp256k1 secret-key->bytes)
           (only-in web3/account secret-key->address signature)
           (only-in web3/rpc bytes->hex-string)
           web3/transaction
           "cast.rkt"
           (only-in "helpers/accounts.rkt" random-key)
           "helpers/transactions.rkt"
           (only-in "util.rkt" seeded))

  (define (decimal n)
    (number->string n))

  (define (access-list-json tx)
    (jsexpr->string
     (for/list ([entry (in-list (transaction-access-list tx))])
       (match-define (cons address keys) entry)
       (hasheq 'address address 'storageKeys (map bytes->hex-string keys)))))

  (define (fee-arguments tx)
    (match (transaction-kind tx)
      ['legacy (list "--legacy" "--gas-price" (decimal (transaction-gas-price tx)))]
      ['eip2930 (list "--legacy" "--access-list" (access-list-json tx)
                      "--gas-price" (decimal (transaction-gas-price tx)))]
      ['eip1559 (list "--gas-price" (decimal (transaction-max-fee-per-gas tx))
                      "--priority-gas-price"
                      (decimal (transaction-max-priority-fee-per-gas tx))
                      "--access-list" (access-list-json tx))]))

  (define (mktx-arguments tx . options)
    (define data (bytes->hex-string (transaction-data tx)))
    (append (list "mktx"
                  "--nonce" (decimal (transaction-nonce tx))
                  "--gas-limit" (decimal (transaction-gas-limit tx))
                  "--chain" (decimal (transaction-chain-id tx))
                  "--value" (decimal (transaction-value tx)))
            (fee-arguments tx)
            options
            (match (transaction-to tx)
              [#f (list "--create" data)]
              [to (list to data)])))

  (define (key-hex key)
    (bytes->hex-string (secret-key->bytes key)))

  (define (cast-signed key tx)
    (apply run-cast (mktx-arguments tx "--private-key" (key-hex key))))

  (define (cast-unsigned key tx)
    (apply run-cast (mktx-arguments tx "--raw-unsigned" "--from" (secret-key->address key))))

  (define (cast-cases count . seed)
    (define prng (apply seeded seed))
    (for*/list ([kind (in-list '(legacy eip2930 eip1559))]
                [_ (in-range count)])
      (cons (random-key prng) (random-transaction prng kind #:fee-bytes 16))))

  (define (quantity text)
    (string->number (substring text 2) 16))

  (define (decoded raw)
    (string->jsexpr (string->jsexpr (run-cast "decode-tx" (bytes->hex-string raw)))))

  (define (check-decoded stx json)
    (define tx (signed-transaction-transaction stx))
    (define name (format "~s" stx))
    (match-define (signature r s y-parity) (signed-transaction-signature stx))
    (define (field key) (hash-ref json key))
    (check-equal? (field 'signer) (string-downcase (signed-transaction-sender stx)) name)
    (check-equal? (field 'hash) (signed-transaction-hash stx) name)
    (check-equal? (quantity (field 'type)) (transaction-type tx) name)
    (check-equal? (quantity (field 'chainId)) (transaction-chain-id tx) name)
    (check-equal? (quantity (field 'nonce)) (transaction-nonce tx) name)
    (check-equal? (quantity (field 'gas)) (transaction-gas-limit tx) name)
    (check-equal? (field 'to) (match (transaction-to tx) [#f 'null] [to (string-downcase to)]) name)
    (check-equal? (quantity (field 'value)) (transaction-value tx) name)
    (check-equal? (field 'input) (bytes->hex-string (transaction-data tx)) name)
    (check-equal? (quantity (field 'r)) r name)
    (check-equal? (quantity (field 's)) s name)
    (match (transaction-type tx)
      [0
       (check-equal? (quantity (field 'gasPrice)) (transaction-gas-price tx) name)
       (check-equal? (quantity (field 'v)) (+ 35 (* 2 (transaction-chain-id tx)) y-parity) name)]
      [type
       (check-equal? (quantity (field 'yParity)) y-parity name)
       (check-equal? (field 'accessList)
                     (for/list ([entry (in-list (transaction-access-list tx))])
                       (match-define (cons address keys) entry)
                       (hasheq 'address (string-downcase address)
                               'storageKeys (map bytes->hex-string keys)))
                     name)
       (if (= type 1)
           (check-equal? (quantity (field 'gasPrice)) (transaction-gas-price tx) name)
           (check-equal? (list (quantity (field 'maxFeePerGas))
                               (quantity (field 'maxPriorityFeePerGas)))
                         (list (transaction-max-fee-per-gas tx)
                               (transaction-max-priority-fee-per-gas tx))
                         name))]))

  (cond
    [(not cast-path)
     (displayln "skipping transaction differential: cast not found")]
    [else
     (define signed-cases (cast-cases 30 2 7 1 8 2 8))

     (define signed
       (for/list ([pair (in-list signed-cases)])
         (match-define (cons key tx) pair)
         (sign-transaction key tx)))

     (test-case "C1: sign-transaction agrees with cast mktx"
       (for ([pair (in-list signed-cases)]
             [stx (in-list signed)])
         (match-define (cons key tx) pair)
         (check-equal? (bytes->hex-string (signed-transaction->bytes stx)) (cast-signed key tx)
                       (format "~s" tx))))

     (test-case "C2: transaction->bytes agrees with cast mktx --raw-unsigned"
       (for ([pair (in-list (cast-cases 20 3 1 4 1 5 9))])
         (match-define (cons key tx) pair)
         (check-equal? (bytes->hex-string (transaction->bytes tx)) (cast-unsigned key tx)
                       (format "~s" tx))))

     (test-case "C3: cast decode-tx reads our raw bytes as the fields we signed"
       (for ([stx (in-list signed)]
             [i (in-naturals)]
             #:when (< (remainder i 30) 20))
         (check-decoded stx (decoded (signed-transaction->bytes stx)))))

     (test-case "C4: where cast and web3/transaction differ"
       (define twin (edit-raw (third bases) '(high-s)))
       (check-equal? (hash-ref (decoded twin) 'signer) "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266")
       (check-exn exn:fail:web3:raw-transaction? (lambda () (bytes->signed-transaction twin)))
       (define tx (third example-transactions))
       (define arguments (mktx-arguments tx "--private-key" (key-hex example-key)))
       (define priority-above-max
         (list-set arguments
                   (add1 (index-of arguments "--priority-gas-price"))
                   (decimal (add1 (transaction-max-fee-per-gas tx)))))
       (match (apply cast-outcome priority-above-max)
         [(list 'failed stderr) (check-regexp-match #rx"cannot exceed max fee per gas" stderr)]
         [other (fail (format "cast accepted ~s" other))])
       (define wide-fee (transaction-of 'eip1559 (hasheq 'max-fee-per-gas (expt 2 128))))
       (check-pred signed-transaction? (sign-transaction example-key wide-fee))
       (check-equal? (first (apply cast-outcome
                                   (mktx-arguments wide-fee "--private-key" (key-hex example-key))))
                     'failed))]))
