#lang racket/base

(module+ test
  (require (only-in json string->jsexpr)
           racket/list
           racket/match
           (only-in racket/string string-split string-trim)
           rackunit
           web3
           (only-in "abi-gen.rkt" random-address)
           (only-in "helpers/accounts.rkt" anvil-keys)
           (only-in "helpers/mock.rkt" call-with-fields)
           (only-in "helpers/transactions.rkt" slot-1)
           (only-in "cast.rkt" cast-outcome cast-path run-cast)
           (only-in "util.rkt" anvil-bob seeded))

  (match-define (list _ key1 _ ...) (map hex-string->secret-key anvil-keys))
  (define signer (local-signer key1))
  (define prng (seeded 3 1 4 1 5 9))
  (define init-code (hex-string->bytes "0x602a60005260206000f3"))

  (define (fresh-address)
    (address->checksum-string (random-address prng)))

  (define (json-number v)
    (match v
      [(? exact-integer?) v]
      [(? string?) (hex-string->number v)]))

  (define (expected-fields tx)
    (append
     `((type ,(transaction-type tx)) (nonce ,(transaction-nonce tx))
       (gas ,(transaction-gas-limit tx)) (value ,(transaction-value tx))
       (chainId ,(transaction-chain-id tx))
       (to ,(match (transaction-to tx) [#f #f] [to (string-downcase to)]))
       (input ,(bytes->hex-string (transaction-data tx)))
       (signer ,(string-downcase (signer-address signer))))
     (match (transaction-type tx)
       [2 `((maxFeePerGas ,(transaction-max-fee-per-gas tx))
            (maxPriorityFeePerGas ,(transaction-max-priority-fee-per-gas tx)))]
       [_ `((gasPrice ,(transaction-gas-price tx)))])))

  (define (given-fields tx)
    (append
     `(#:nonce ,(transaction-nonce tx) #:gas-limit ,(transaction-gas-limit tx)
       #:type ,(transaction-type tx) #:value ,(transaction-value tx) #:data ,(transaction-data tx))
     (match (transaction-type tx)
       [0 `(#:gas-price ,(transaction-gas-price tx))]
       [1 `(#:gas-price ,(transaction-gas-price tx)
            #:access-list ,(transaction-access-list tx))]
       [2 `(#:max-fee-per-gas ,(transaction-max-fee-per-gas tx)
            #:max-priority-fee-per-gas ,(transaction-max-priority-fee-per-gas tx)
            #:access-list ,(transaction-access-list tx))])))

  (define (cast-fields json keys)
    (for/list ([key (in-list keys)])
      (define v (hash-ref json key 'null))
      (list key
            (match* (key v)
              [(_ 'null) #f]
              [((or 'to 'input 'signer) _) (string-downcase v)]
              [(_ _) (json-number v)]))))

  (cond
    [(not cast-path) (displayln "skipping send cast tests: cast not found")]
    [(not (current-anvil-path)) (displayln "skipping send cast tests: anvil not found")]
    [else
     (define node (start-anvil))
     (define url (format "http://127.0.0.1:~a" (anvil-port node)))
     (parameterize ([current-provider (anvil-provider node)])
       (test-case "C1: cast reads the transactions sent as they were filled"
         (for ([fields (list `(#:type 0 #:value 1) `(#:type 1 #:value 2)
                             `(#:access-list ((,anvil-bob ,slot-1)) #:value 3) `(#:data ,init-code))])
           (define to (and (not (memq '#:data fields)) (fresh-address)))
           (define given (apply hasheq fields))
           (define tx
             (fill-transaction signer to
                               #:type (hash-ref given '#:type #f)
                               #:value (hash-ref given '#:value 0)
                               #:data (hash-ref given '#:data #"")
                               #:access-list (hash-ref given '#:access-list #f)))
           (define tx-hash (call-with-fields send-signed-transaction (given-fields tx) signer to))
           (define name (format "~s" fields))
           (check-true (receipt-succeeded? (wait-for-receipt tx-hash)) name)
           (define raw (rpc-call "eth_getRawTransactionByHash" (list tx-hash)))
           (define decoded (string->jsexpr (run-cast "decode-transaction" "--json" raw)))
           (define json (string->jsexpr (hash-ref decoded 'data)))
           (define expected (expected-fields tx))
           (check-equal? (cast-fields json (map first expected)) expected name)
           (check-equal? (hash-ref json 'hash) tx-hash name)))

       (match (cast-outcome "chain-id" "--rpc-url" url)
         [(list 'failed stderr)
          (printf "skipping C2: cast cannot reach the node: ~a\n"
                  (last (string-split (string-trim stderr) "\n")))]
         [(list 'ok _)
          (test-case "C2: the gas limit filled is cast's estimate"
            (for ([value (list 0 1 (ether->wei 1) 0 (gwei->wei 3))]
                  [to (list (fresh-address) anvil-bob (fresh-address) anvil-bob (fresh-address))])
              (define tx (fill-transaction signer to #:value value))
              (check-equal? (string->number
                             (run-cast "estimate" "--from" (signer-address signer) to
                                       "--value" (number->string value) "--rpc-url" url))
                            (transaction-gas-limit tx))))]))
     (stop-anvil node)]))
