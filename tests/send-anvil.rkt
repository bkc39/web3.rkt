#lang racket/base

(module+ test
  (require racket/list
           racket/match
           rackunit
           web3
           (only-in "helpers/accounts.rkt" anvil-addresses anvil-keys random-key)
           (only-in "helpers/mock.rkt" call-with-fields recording recording-calls)
           (only-in "helpers/transactions.rkt"
                    check-node-fields quantity random-checksummed-address slot-1)
           (only-in "util.rkt" anvil-bob seeded))

  (define-contract token #:file "../examples/contracts/out/Token.sol/Token.json")
  (define-contract counter #:file "../examples/contracts/out/Counter.sol/Counter.json")

  (match-define (list _ key1 key2 key3 key4 _ ...) (map hex-string->secret-key anvil-keys))
  (match-define (list _ account1 _ account3 _ account5 _ ...) anvil-addresses)
  (define token-abi
    (file->contract-abi (collection-file-path "examples/contracts/out/Token.sol/Token.json" "web3")))
  (define counter-abi
    (file->contract-abi
     (collection-file-path "examples/contracts/out/Counter.sol/Counter.json" "web3")))

  (define prng (seeded 1 8 7 5 4 2))
  (define one-and-a-half (ether->wei "1.5"))

  (define (fresh-address)
    (random-checksummed-address prng))

  (define (balance address)
    (eth-get-balance address "latest"))

  (define (pending address)
    (eth-get-transaction-count address "pending"))

  (define (fee receipt)
    (* (receipt-gas-used receipt) (receipt-effective-gas-price receipt)))

  (define (recorded calls method)
    (for/list ([call (in-list calls)]
               #:when (equal? (first call) method))
      call))

  (define (raw-param calls)
    (match (recorded calls "eth_sendRawTransaction")
      [(list (list _ (list raw) _)) raw]))

  (define (raised-rpc thunk)
    (with-handlers ([exn:fail:web3:rpc? values])
      (thunk)
      #f))

  (define (deploy-token deploy from)
    (deploy (contract-abi-bytecode token-abi) "constructor(string,string,uint256)" "Token" "TOK"
            1000 #:from from))

  (define (sends-from from)
    (define-values (address deployed) (deploy-token deploy-contract/receipt from))
    (define transferred (token-transfer address anvil-bob 10 #:from from))
    (define deposited (token-deposit address "memo" #:from from #:value 7))
    (values address (list deployed transferred deposited)))

  (define (chain-view address receipts sender)
    (list (for/list ([receipt (in-list receipts)])
            (list (receipt-status receipt) (receipt-gas-used receipt)
                  (for/list ([l (in-list (receipt-logs receipt))])
                    (list (log-address l) (log-topics l) (log-data l)))
                  (receipt-contract-address receipt)
                  (let ([tx (eth-get-transaction-by-hash (receipt-transaction-hash receipt))])
                    (map (lambda (field) (field tx))
                         (list transaction-info-to transaction-info-data transaction-info-value
                               transaction-info-gas-limit)))))
          (token-balance-of address anvil-bob)
          (token-deposits address sender)
          (token-total-supply address)
          (balance anvil-bob)
          (pending sender)))

  (define (decoded receipts)
    (for*/list ([receipt (in-list receipts)]
                [l (in-list (receipt-logs receipt #:events (contract-abi-events token-abi)))])
      (list (event-log-name l) (event-log-arguments l))))

  (define (ether-cases)
    (test-case "N1, N11: an ether send from a local key is mined as filled and signed"
      (define signer (local-signer key2))
      (define sender (signer-address signer))
      (define recipient (fresh-address))
      (define sender-before (balance sender))
      (define nonce-before (pending sender))
      (define node (recording (current-provider)))
      (define receipt
        (send-signed-transaction/receipt signer recipient #:value one-and-a-half #:provider node))
      (define calls (recording-calls node))
      (check-equal? (list (receipt-status receipt) (receipt-type receipt)) '(1 2))
      (check-equal? (balance recipient) one-and-a-half)
      (check-equal? (- sender-before (balance sender)) (+ one-and-a-half (fee receipt)))
      (check-equal? (pending sender) (add1 nonce-before))
      (define tx-hash (receipt-transaction-hash receipt))
      (define raw (rpc-call "eth_getRawTransactionByHash" (list tx-hash)))
      (check-equal? raw (raw-param calls))
      (define stx (bytes->signed-transaction (hex-string->bytes raw)))
      (check-equal? (signed-transaction-sender stx) sender)
      (check-equal? (signed-transaction-hash stx) tx-hash)
      (check-node-fields stx (eth-get-transaction-by-hash tx-hash))
      (match-define (list (list _ _ block)) (recorded calls "eth_getBlockByNumber"))
      (match-define (list (list _ _ priority)) (recorded calls "eth_maxPriorityFeePerGas"))
      (check-equal? (transaction-max-fee-per-gas (signed-transaction-transaction stx))
                    (+ (* 2 (quantity (hash-ref block 'baseFeePerGas))) (quantity priority))))

    (test-case "N2: sends in a row get consecutive nonces"
      (define signer (local-signer key3))
      (define first-nonce (pending (signer-address signer)))
      (define hashes
        (for/list ([_ (in-range 5)])
          (send-signed-transaction signer (fresh-address) #:value 1)))
      (for ([tx-hash (in-list hashes)]
            [nonce (in-naturals first-nonce)])
        (check-equal? (receipt-status (wait-for-receipt tx-hash)) 1)
        (check-equal? (transaction-info-nonce (eth-get-transaction-by-hash tx-hash)) nonce)))

    (test-case "S9: the node signs sends from its accounts, filling their nonces"
      (define recipient (fresh-address))
      (define first-nonce (pending account5))
      (define receipt (send-transaction/receipt account5 recipient #:value (ether->wei 1)))
      (check-equal? (list (receipt-status receipt) (receipt-from receipt) (receipt-to receipt))
                    (list 1 account5 recipient))
      (check-equal? (balance recipient) (ether->wei 1))
      (define hashes
        (for/list ([_ (in-range 2)])
          (send-transaction account5 recipient #:value 1)))
      (check-equal? (for/list ([tx-hash (in-list hashes)])
                      (receipt-status (wait-for-receipt tx-hash)))
                    '(1 1))
      (check-equal? (map (lambda (tx-hash)
                           (transaction-info-nonce (eth-get-transaction-by-hash tx-hash)))
                         hashes)
                    (list (+ first-nonce 1) (+ first-nonce 2)))
      (check-equal? (balance recipient) (+ (ether->wei 1) 2))
      (define created
        (send-transaction/receipt account5 #f #:data (contract-abi-bytecode counter-abi)))
      (define address (receipt-contract-address created))
      (check-equal? address (create-address account5 (+ first-nonce 3)))
      (check-equal? (counter-count address) 0))

    (test-case "S11: a send the node signs has the type fill-transaction gives"
      (define price (eth-gas-price))
      (for ([row (in-list `((0 (#:type 0))
                            (1 (#:type 1))
                            (1 (#:type 1 #:gas-price ,price))
                            (1 (#:gas-price ,price #:access-list ()))
                            (2 (#:type 2))
                            (2 ())))])
        (match-define (list type fields) row)
        (define name (format "~s" fields))
        (define recipient (fresh-address))
        (check-equal? (transaction-type
                       (call-with-fields fill-transaction fields account5 recipient))
                      type name)
        (check-equal? (receipt-type (call-with-fields send-transaction/receipt fields
                                                      account5 recipient))
                      type name)))

    (test-case "S12: a send the node signs takes a tip alone, or a fee cap alone"
      (for ([row (in-list `(((#:max-priority-fee-per-gas ,(gwei->wei 1))
                             ,transaction-info-max-priority-fee-per-gas)
                            ((#:max-fee-per-gas ,(gwei->wei 100))
                             ,transaction-info-max-fee-per-gas)))])
        (match-define (list (and fields (list _ amount)) mined-fee) row)
        (define name (format "~s" fields))
        (define receipt (call-with-fields send-transaction/receipt `(#:value 1 ,@fields)
                                          account5 (fresh-address)))
        (check-equal? (list (receipt-status receipt) (receipt-type receipt)) '(1 2) name)
        (check-equal? (mined-fee (eth-get-transaction-by-hash (receipt-transaction-hash receipt)))
                      amount name))))

  (define (contract-cases)
    (test-case "N4, N5: a contract's sends from an address and from its key leave the same chain"
      (define snapshot (evm-snapshot))
      (define before (balance account1))
      (define-values (by-address address-receipts) (sends-from account1))
      (define address-view (chain-view by-address address-receipts account1))
      (define address-spent (- before (balance account1)))
      (define address-logs (decoded address-receipts))
      (evm-revert snapshot)
      (define-values (by-signer signer-receipts) (sends-from (local-signer key1)))
      (check-equal? by-signer by-address)
      (check-equal? (chain-view by-signer signer-receipts account1) address-view)
      (check-equal? (decoded signer-receipts) address-logs)
      (check-equal? address-spent (+ 7 (apply + (map fee address-receipts))))
      (check-equal? (- before (balance account1)) (+ 7 (apply + (map fee signer-receipts)))))

    (test-case "N7: a reverting send raises the decoded revert and spends no nonce"
      (define signer (local-signer key1))
      (define address (deploy-token deploy-contract signer))
      (define nonce (pending account1))
      (define e
        (with-handlers ([exn:fail:web3:rpc:revert? values])
          (token-transfer address anvil-bob 1000000 #:from signer)))
      (check-pred exn:fail:web3:rpc:revert? e)
      (check-equal? (abi-signature-name (abi-revert-signature (exn:fail:web3:rpc:revert-revert e)))
                    "InsufficientBalance")
      (check-equal? (pending account1) nonce))

    (test-case "N10: a node signer as #:from sends from its account"
      (define address (deploy-token deploy-contract (local-signer key1)))
      (define receipt
        (contract-send address "deposit(string)" "node" #:value 1 #:from (node-signer account3)))
      (check-equal? (receipt-status receipt) 1)
      (check-equal? (string-downcase (transaction-info-from (eth-get-transaction-by-hash
                                                             (receipt-transaction-hash receipt))))
                    (string-downcase account3))))

  (define refusals
    `((,(lambda (signer _poor) (send-signed-transaction signer anvil-bob #:nonce 0))
       #rx"nonce too low")
      (,(lambda (signer _poor) (send-signed-transaction signer anvil-bob #:max-fee-per-gas 1
                                                        #:max-priority-fee-per-gas 0))
       #rx"base fee")
      (,(lambda (_signer poor) (send-signed-transaction poor anvil-bob #:value 1
                                                        #:gas-limit 21000))
       #rx"(?i:insufficient funds)")))

  (define (account-state signer poor)
    (define sender (signer-address signer))
    (list (pending sender) (balance sender) (pending (signer-address poor))))

  (define (refusal-cases)
    (test-case "N8: the node's refusals leave nonces and balances as they were"
      (define signer (local-signer key4))
      (wait-for-receipt (send-signed-transaction signer (fresh-address) #:value 1))
      (define poor (local-signer (random-key prng)))
      (define before (account-state signer poor))
      (for ([row (in-list refusals)])
        (match-define (list send pattern) row)
        (define e (raised-rpc (lambda () (send signer poor))))
        (check-pred exn:fail:web3:rpc? e (format "~a" pattern))
        (when e
          (check-equal? (exn:fail:web3:rpc-method e) "eth_sendRawTransaction")
          (check-regexp-match pattern (exn:fail:web3:rpc-rpc-message e))))
      (define wrong-chain
        (with-handlers ([exn:fail:web3:fill? values])
          (send-signed-transaction signer anvil-bob #:chain-id 1)))
      (check-eq? (exn:fail:web3:fill-reason wrong-chain) 'chain-id)
      (check-equal? (account-state signer poor) before))

    (test-case "N9: a transaction that fails on chain raises with its receipt"
      (define reverter (fresh-address))
      (anvil-set-code reverter "0x60006000fd")
      (define e
        (with-handlers ([exn:fail:web3:transaction? values])
          (send-signed-transaction/receipt (local-signer key2) reverter #:gas-limit 50000)))
      (check-pred exn:fail:web3:transaction? e)
      (check-equal? (receipt-status (exn:fail:web3:transaction-receipt e)) 0)))

  (define echo-runtime
    (hex-string->bytes "0x6000358060005260f81c60ff14601c57602a60206000a160206000f35b60206000fd"))
  (define echo-init (bytes-append (hex-string->bytes "0x602280600b6000396000f3") echo-runtime))

  (define (call-cases)
    (define-values (echo _deployed) (deploy-contract/receipt echo-init #:from account1))

    (test-case "T1: call-transaction calls a contract with no ABI, bytes in and bytes out"
      (define calldata (word->bytes 42))
      (check-equal? (call-transaction #f echo #:data calldata) calldata)
      (check-equal? (call-transaction account3 echo #:data calldata) calldata)
      (check-equal? (call-transaction (local-signer key1) echo #:data calldata #:block "pending")
                    calldata)
      (check-equal? (call-transaction #f echo) (make-bytes 32 0)))

    (test-case "T2: a call that reverts raises exn:fail:web3:rpc:revert carrying the revert data"
      (define calldata (bytes-append (bytes #xff) (make-bytes 31 7)))
      (define e
        (with-handlers ([exn:fail:web3:rpc:revert? values])
          (call-transaction account3 echo #:data calldata)))
      (check-pred exn:fail:web3:rpc:revert? e)
      (when (exn:fail:web3:rpc:revert? e)
        (check-equal? (exn:fail:web3:rpc-method e) "eth_call")
        (check-equal? (abi-revert-data (exn->abi-revert e)) calldata)))

    (test-case "T3: a call to #f runs its data as creation code"
      (check-equal? (call-transaction #f #f #:data echo-init) echo-runtime)
      (check-equal? (call-transaction account3 #f #:data echo-init) echo-runtime))

    (test-case "T4: send-transaction/receipt from a local signer is mined from its address"
      (define signer (local-signer key3))
      (define nonce (pending (signer-address signer)))
      (define receipt (send-transaction/receipt signer echo #:data (word->bytes 7)))
      (check-equal? (receipt-status receipt) 1)
      (check-equal? (receipt-from receipt) (signer-address signer))
      (check-equal? (pending (signer-address signer)) (add1 nonce))
      (match-define (list l) (receipt-logs receipt))
      (check-equal? (log-address l) echo)
      (check-equal? (log-topics l) (list (bytes->hex-string (word->bytes 42))))
      (check-equal? (log-data l) (word->bytes 7))
      (define tx-hash (send-transaction signer echo #:data (word->bytes 8)))
      (check-equal? (receipt-from (wait-for-receipt tx-hash)) (signer-address signer))))

  (define (berlin-cases)
    (test-case "N6: before London a fill is legacy, or EIP-2930 with an access list"
      (define signer (local-signer key2))
      (define sender (signer-address signer))
      (define price (eth-gas-price))
      (define legacy (fill-transaction signer (fresh-address) #:value 1))
      (check-equal? (list (transaction-type legacy) (transaction-gas-price legacy)) (list 0 price))
      (define listed
        (fill-transaction signer anvil-bob #:access-list (list (list anvil-bob slot-1))))
      (check-equal? (transaction-type listed) 1)
      (check-equal?
       (for/list ([access-list (list #f (list (list anvil-bob slot-1)))])
         (receipt-type (send-signed-transaction/receipt signer (fresh-address) #:value 1
                                                        #:access-list access-list)))
       '(0 1))
      (define nonce (pending sender))
      (define e
        (with-handlers ([exn:fail:web3:fill? values])
          (send-signed-transaction signer anvil-bob #:type 2)))
      (check-eq? (exn:fail:web3:fill-reason e) 'no-base-fee)
      (check-equal? (pending sender) nonce)))

  (define (accountless-cases)
    (test-case "N3: a key the node does not hold deploys and sends; its address alone cannot"
      (define signer (local-signer key2))
      (define sender (signer-address signer))
      (anvil-set-balance sender (ether->wei 10))
      (parameterize ([current-account signer])
        (define address (counter-deploy))
        (check-equal? address (create-address sender 0))
        (counter-increment address)
        (check-equal? (counter-count address) 1)
        (define e (raised-rpc (lambda () (counter-increment address #:from sender))))
        (check-equal? (exn:fail:web3:rpc-method e) "eth_sendTransaction")
        (check-equal? (counter-count address) 1))))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping send node tests: anvil not found")]
    [else
     (with-anvil ()
       (ether-cases)
       (contract-cases)
       (refusal-cases)
       (call-cases))
     (with-anvil (#:hardfork "berlin")
       (berlin-cases))
     (with-anvil (#:args '("--accounts" "0"))
       (accountless-cases))]))
