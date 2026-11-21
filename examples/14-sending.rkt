#lang racket/base

(require web3)

(define-contract counter #:file "contracts/out/Counter.sol/Counter.json")

(define key
  (hex-string->secret-key "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))
(define signer (local-signer key))
(define recipient "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")

(module+ main
  (with-anvil (#:args '("--accounts" "0"))
    (anvil-set-balance (signer-address signer) (ether->wei 10))
    (parameterize ([current-account signer])
      (define address (counter-deploy))
      (counter-increment address)
      (printf "counter ~a count ~a\n" address (counter-count address)))
    (define tx (fill-transaction signer recipient #:value (ether->wei "1.5")))
    (printf "transfer type ~a nonce ~a gas ~a\n"
            (transaction-type tx) (transaction-nonce tx) (transaction-gas-limit tx))
    (define receipt (send-signed-transaction/receipt signer recipient #:value (ether->wei "1.5")))
    (define raw (rpc-call "eth_getRawTransactionByHash" (list (hash-ref receipt 'transactionHash))))
    (printf "  sender ~a\n"
            (signed-transaction-sender (bytes->signed-transaction (hex-string->bytes raw))))
    (printf "  balance ~a ether\n" (wei->string (eth-get-balance recipient "latest")))))
