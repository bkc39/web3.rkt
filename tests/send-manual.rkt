#lang racket/base

(module+ test
  (require rackunit
           "helpers/manual.rkt")

  (define blocks
    `(([(require racket/list) (void)]
       [(define key
          (hex-string->secret-key
           "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))
        (void)]
       [(define signer (local-signer key)) (void)]
       [(define recipient "0x70997970C51812dc3A010C7d01b50e0d17dc79C8") (void)]
       [(define answers
          (hash "eth_chainId" "0x7a69"
                "eth_estimateGas" "0x5208"
                "eth_getTransactionCount" "0x0"
                "eth_getBlockByNumber" (hasheq 'number "0x1" 'baseFeePerGas "0x3b9aca00")
                "eth_maxPriorityFeePerGas" "0x3b9aca00"
                "eth_gasPrice" "0x77359400"
                "eth_sendRawTransaction"
                (lambda (params)
                  (signed-transaction-hash
                   (bytes->signed-transaction (hex-string->bytes (first params)))))))
        (void)]
       [(define node (mock-provider answers)) (void)]
       [(define tx (fill-transaction signer recipient #:value (ether->wei "1.5") #:provider node))
        (void)]
       [tx (prints "#<eip1559-transaction chain-id 31337 nonce 0>")]
       [(transaction-max-fee-per-gas tx) (value 3000000000)]
       [(map first (mock-provider-calls node))
        (value ("eth_chainId" "eth_estimateGas" "eth_getTransactionCount" "eth_getBlockByNumber"
                "eth_maxPriorityFeePerGas"))]
       [(define legacy-node
          (mock-provider (hash-set answers "eth_getBlockByNumber" (hasheq 'number "0x1"))))
        (void)]
       [(transaction-type (fill-transaction signer recipient #:provider legacy-node)) (value 0)])
      ([(transaction-nonce (fill-transaction signer recipient #:nonce 7 #:provider node)) (value 7)]
       [(fill-transaction signer recipient #:chain-id 1 #:provider node)
        (raises "fill-transaction: wrong chain: #:chain-id is 1, but the node's chain is 31337")])
      ([(send-signed-transaction signer recipient #:value (ether->wei "1.5")
                                 #:provider (mock-provider answers))
        (value "0x353911be23e52e6b62f32b2323be63d06541c4ab17382cde9ff87b3174c3c9f6")])
      ([(fill-transaction signer recipient #:type 0 #:access-list '() #:provider node)
        (raises "fill-transaction: #:type 0 takes no #:access-list")]
       [(fill-transaction signer recipient #:type 2 #:provider legacy-node)
        (raises ,(string-append "fill-transaction: no base fee: the latest block has no base fee, "
                                "so a type 2 transaction needs #:max-fee-per-gas"))])))

  (test-case "M1: the manual's sending examples give what the manual shows"
    (check-manual-blocks blocks)))
