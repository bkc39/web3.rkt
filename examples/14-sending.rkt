#lang racket/base

(require web3)

(define-contract counter #:file "contracts/out/Counter.sol/Counter.json")

(define key
  (hex-string->secret-key "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))
(define signer (local-signer key))
(define recipient "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")

(define (refused thunk)
  (with-handlers ([exn:fail:web3:rpc? (lambda (_) 'refused)])
    (thunk)))

(define (raw-sender tx-hash)
  (signed-transaction-sender
   (bytes->signed-transaction
    (hex-string->bytes (rpc-call "eth_getRawTransactionByHash" (list tx-hash))))))

(module+ main
  (with-anvil (#:args '("--accounts" "0"))
    (anvil-set-balance (signer-address signer) (ether->wei 10))
    (printf "node accounts ~a\n" (eth-accounts))
    (parameterize ([current-account signer])
      (define address (counter-deploy))
      (counter-increment address)
      (printf "counter ~a\n  count ~a\n" address (counter-count address))
      (printf "  from the address ~a\n"
              (refused (lambda () (counter-increment address #:from (signer-address signer)))))
      (printf "  count ~a\n" (counter-count address)))
    (define tx (fill-transaction signer recipient #:value (ether->wei "1.5")))
    (printf "transfer type ~a nonce ~a gas ~a\n"
            (transaction-type tx) (transaction-nonce tx) (transaction-gas-limit tx))
    (define receipt (send-signed-transaction/receipt signer recipient #:value (ether->wei "1.5")))
    (printf "  sender ~a\n  status ~a\n" (raw-sender (hash-ref receipt 'transactionHash))
            (receipt-succeeded? receipt))
    (printf "  balance ~a ether\n  nonce ~a\n"
            (wei->string (eth-get-balance recipient "latest"))
            (eth-get-transaction-count (signer-address signer) "latest"))))
