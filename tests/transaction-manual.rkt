#lang racket/base

(module+ test
  (require rackunit
           "helpers/manual.rkt")

  (define blocks
    `(([(define key
          (hex-string->secret-key
           "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))
        (void)]
       [(define recipient "0x70997970C51812dc3A010C7d01b50e0d17dc79C8") (void)]
       [(define tx
          (eip1559-transaction #:chain-id 31337
                               #:nonce 0
                               #:max-priority-fee-per-gas (gwei->wei 2)
                               #:max-fee-per-gas (gwei->wei 30)
                               #:gas-limit 21000
                               #:to recipient
                               #:value (ether->wei "1.5")))
        (void)]
       [(define signed (sign-transaction key tx)) (void)]
       [(bytes->hex-string (signed-transaction->bytes signed))
        (value ,(string-append
                 "0x02f875827a698084773594008506fc23ac008252089470997970c51812dc3a010c7d01b50e0d"
                 "17dc79c88814d1120d7b16000080c001a0e4283c98410a9ee00d296ef6d31b47e6326541bf89e1"
                 "4640e3ff09b145517165a070e1e6a5e2a7b71af2ff6278becbe8de276a30f350ea156b43867875"
                 "1338171f"))]
       [(signed-transaction-hash signed)
        (value "0x6e6196f107bb02ceffd0db99a2c3be3053f8d1917061e67f88c7843aa77833e2")])
      ([(define parsed (bytes->signed-transaction (signed-transaction->bytes signed))) (void)]
       [(signed-transaction-sender parsed) (value "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")]
       [(equal? parsed signed) (value #t)]
       [(transaction-value (signed-transaction-transaction parsed)) (value 1500000000000000000)]
       [(bytes->signed-transaction (bytes 3))
        (raises "bytes->signed-transaction: not a transaction: type 3 is not supported")])
      ([(define slot-1 (bytes-append (make-bytes 31 0) (bytes 1))) (void)]
       [(signed-transaction-hash
         (sign-transaction key
                           (eip2930-transaction #:chain-id 31337
                                                #:nonce 0
                                                #:gas-price (gwei->wei 30)
                                                #:gas-limit 30000
                                                #:to recipient
                                                #:value (ether->wei "1.5")
                                                #:access-list (list (list recipient slot-1)))))
        (value "0x9e7b669c47958eaa50ebec28a6941dfc397986688e124eb1cacc1d290a693326")])
      ([(define eip-155
          (legacy-transaction #:chain-id 1
                              #:nonce 9
                              #:gas-price (gwei->wei 20)
                              #:gas-limit 21000
                              #:to "0x3535353535353535353535353535353535353535"
                              #:value (ether->wei 1)))
        (void)]
       [eip-155 (prints "#<legacy-transaction chain-id 1 nonce 9>")])
      ([(eip1559-transaction #:chain-id 1 #:nonce 0
                             #:max-priority-fee-per-gas 2 #:max-fee-per-gas 1
                             #:gas-limit 21000 #:to #f)
        (blames eip1559-transaction)])
      ([(transaction-to eip-155) (value "0x3535353535353535353535353535353535353535")]
       [(transaction-chain-id eip-155) (value 1)])
      ([(bytes->hex-string (transaction->bytes eip-155))
        (value ,(string-append
                 "0xec098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a7"
                 "64000080018080"))])
      ([(bytes->hex-string (transaction-signing-hash eip-155))
        (value "0xdaf5a779ae972f972197303d7b574746c7ef83eadac0f2791ad23db92e4c8e53")])
      ([(bytes->hex-string
         (signed-transaction->bytes
          (sign-transaction
           (hex-string->secret-key
            "0x4646464646464646464646464646464646464646464646464646464646464646")
           eip-155)))
        (value ,(string-append
                 "0xf86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a7"
                 "6400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a0"
                 "67cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83"))])
      ([(equal? (signed-transaction tx (sign-digest key (transaction-signing-hash tx)))
                (sign-transaction key tx))
        (value #t)])
      ([(define deployer
          (bytes->signed-transaction
           (hex-string->bytes
            (string-append
             "0xf8a58085174876e800830186a08080b853604580600e600039806000f350fe7fffffffffff"
             "ffffffffffffffffffffffffffffffffffffffffffffffffffffe03601600081602082378035"
             "828234f58015156039578182fd5b8082525050506014600cf31ba0222222222222222222222222"
             "2222222222222222222222222222222222222222a0222222222222222222222222222222222222"
             "2222222222222222222222222222"))))
        (void)]
       [(signed-transaction-sender deployer) (value "0x3fAB184622Dc19b6109349B94811493BF2a45362")]
       [(transaction-chain-id (signed-transaction-transaction deployer)) (value #f)]
       [(create-address (signed-transaction-sender deployer) 0)
        (value "0x4e59b44847b379578588920cA78FbF26c0B4956C")]
       [(bytes->signed-transaction (bytes))
        (raises "bytes->signed-transaction: not a transaction: no bytes")])))

  (test-case "M1: the manual's transaction examples give what the manual shows"
    (check-manual-blocks blocks)))
