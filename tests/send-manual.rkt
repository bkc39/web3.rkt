#lang racket/base

(module+ test
  (require (only-in racket/file file->string)
           (only-in racket/match match-define)
           (only-in racket/string string-split)
           rackunit
           (only-in web3/private/send fill-reasons)
           "helpers/manual.rkt")

  (define blocks
    `(([(define key
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
                "eth_sendRawTransaction"
                "0x353911be23e52e6b62f32b2323be63d06541c4ab17382cde9ff87b3174c3c9f6"))
        (void)]
       [(define node (mock-provider answers)) (void)]
       [(define tx (fill-transaction signer recipient #:value (ether->wei "1.5") #:provider node))
        (void)]
       [tx (prints "#<eip1559-transaction chain-id 31337 nonce 0>")]
       [(transaction-max-fee-per-gas tx) (value 3000000000)])
      ([(send-signed-transaction signer recipient #:value (ether->wei "1.5") #:provider node)
        (value "0x353911be23e52e6b62f32b2323be63d06541c4ab17382cde9ff87b3174c3c9f6")])
      ([(fill-transaction signer recipient #:chain-id 1 #:provider node)
        (raises "fill-transaction: wrong chain: #:chain-id is 1, but the node's chain is 31337")]
       [(fill-transaction signer recipient #:type 0 #:access-list '() #:provider node)
        (raises "fill-transaction: #:type 0 takes no #:access-list")]
       [(define legacy-node
          (mock-provider (hash-set answers "eth_getBlockByNumber" (hasheq 'number "0x1"))))
        (void)]
       [(fill-transaction signer recipient #:type 2 #:provider legacy-node)
        (raises ,(string-append "fill-transaction: no base fee: the latest block has no base fee, "
                                "so a type 2 transaction needs #:max-fee-per-gas"))])))

  (test-case "M1: the manual's sending examples give what the manual shows"
    (check-manual-blocks blocks))

  (test-case "M2: the manual's exn:fail:web3:fill reasons are the ones raised"
    (define manual
      (file->string (collection-file-path "scribblings/web3.scrbl" "web3")))
    (match-define (list _ documented)
      (regexp-match #px"exn:fail:web3:fill exn:fail:web3\\) \\(\\[reason \\(or/c ([^)]*)\\)" manual))
    (check-equal? (sort (map (lambda (s) (string->symbol (substring s 1)))
                             (string-split documented))
                        symbol<?)
                  (sort fill-reasons symbol<?))))
