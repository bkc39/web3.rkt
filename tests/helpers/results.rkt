#lang racket/base

(provide block-json
         fee-history-json
         full-block-json
         log-json
         pending-block-json
         receipt-json
         root-receipt-json
         transaction-info-json/0
         transaction-info-json/1
         transaction-info-json/2
         withdrawal-json)

(define transaction-info-json/0
  '#hasheq((blockHash . "0x56268c1258891dd669aacbe1c854f83204063dd979b3207267179897f8e45d7c")
           (blockNumber . "0x1")
           (blockTimestamp . "0x6b02118f")
           (chainId . "0x7a69")
           (from . "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266")
           (gas . "0x5208")
           (gasPrice . "0x6fc23ac00")
           (hash . "0x4a02a85a61dbc92fec29ffa220781b27b2c6bc91527b22a04af286791e41c28a")
           (input . "0x")
           (nonce . "0x0")
           (r . "0xbbfa8af4971395f4254befd93c53c020fb096fc451369a7dc6496814972c5948")
           (s . "0x17e6be4dd1cdb05aa9f72da7d69aa2af9d5c619ad2d29b538f7cf16e40a21411")
           (to . "0x70997970c51812dc3a010c7d01b50e0d17dc79c8")
           (transactionIndex . "0x0")
           (type . "0x0")
           (v . "0xf4f6")
           (value . "0x1")))

(define transaction-info-json/1
  '#hasheq((accessList
            . (#hasheq((address . "0x70997970c51812dc3a010c7d01b50e0d17dc79c8")
                       (storageKeys
                        . ("0x0101010101010101010101010101010101010101010101010101010101010101")))))
           (blockHash . "0x56268c1258891dd669aacbe1c854f83204063dd979b3207267179897f8e45d7c")
           (blockNumber . "0x1")
           (blockTimestamp . "0x6b02118f")
           (chainId . "0x7a69")
           (from . "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266")
           (gas . "0x7530")
           (gasPrice . "0x6fc23ac00")
           (hash . "0xfd310be38374fb2da856236f50c317183f9ae310ac64acc67b0ea7504b4a65bf")
           (input . "0x")
           (nonce . "0x1")
           (r . "0x660a4ca3fee9e2a2c61be0a00ed1d7742e011768c6afb1b236b962d05dbcfeb")
           (s . "0x511662b9422a7f352b401e6b4344f24b97cf0c748d26456e10d194916204ce1e")
           (to . "0x70997970c51812dc3a010c7d01b50e0d17dc79c8")
           (transactionIndex . "0x1")
           (type . "0x1")
           (v . "0x1")
           (value . "0x1")
           (yParity . "0x1")))

(define transaction-info-json/2
  '#hasheq((accessList . ())
           (blockHash . "0x56268c1258891dd669aacbe1c854f83204063dd979b3207267179897f8e45d7c")
           (blockNumber . "0x1")
           (blockTimestamp . "0x6b02118f")
           (chainId . "0x7a69")
           (from . "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266")
           (gas . "0x5208")
           (gasPrice . "0xb2d05e00")
           (hash . "0xaa9f2a6bd2236a45a29502b1d9d6e60123d1b38a4d61b9e396503dc07ee4ab12")
           (input . "0x")
           (maxFeePerGas . "0x6fc23ac00")
           (maxPriorityFeePerGas . "0x77359400")
           (nonce . "0x2")
           (r . "0x96135088ca7d769bbbea307fb768b7442fced7a944758a6b49300eed825bd84f")
           (s . "0x459cd18259c47d8ddab2882b3e58151a1bc91067810d2fbfa9689dffea41089")
           (to . "0x70997970c51812dc3a010c7d01b50e0d17dc79c8")
           (transactionIndex . "0x2")
           (type . "0x2")
           (v . "0x0")
           (value . "0x1")
           (yParity . "0x0")))

(define withdrawal-json
  '#hasheq((index . "0x7")
           (validatorIndex . "0x2a")
           (address . "0x70997970c51812dc3a010c7d01b50e0d17dc79c8")
           (amount . "0x3b9aca00")))

(define block-json
  '#hasheq((baseFeePerGas . "0x3b9aca00")
           (blobGasUsed . "0x0")
           (difficulty . "0x0")
           (excessBlobGas . "0x0")
           (extraData . "0x")
           (gasLimit . "0x1c9c380")
           (gasUsed . "0x106e4")
           (hash . "0x56268c1258891dd669aacbe1c854f83204063dd979b3207267179897f8e45d7c")
           (logsBloom
            . "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
           (miner . "0x0000000000000000000000000000000000000000")
           (mixHash . "0x924439e964bc6aae3d7b1c037a9119caf7652e53ed5582cfe279dcae20bab02b")
           (nonce . "0x0000000000000000")
           (number . "0x1")
           (parentBeaconBlockRoot
            . "0x0000000000000000000000000000000000000000000000000000000000000000")
           (parentHash . "0xcf3edcafefb348d27cb299d18f73cbcc4e93a912168b6291369a3fc8e0580e5b")
           (receiptsRoot . "0xdfeff9dad83b6f7eb62afe83ba8adff1d0951e31f085553500598d2a77725114")
           (requestsHash . "0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
           (sha3Uncles . "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")
           (size . "0x3ec")
           (stateRoot . "0xb1b8d88a4b0a2acaaa60e14245dc7d794bcce5a13d918af30890c565db667608")
           (timestamp . "0x6b02118f")
           (totalDifficulty . "0x0")
           (transactions
            . ("0x4a02a85a61dbc92fec29ffa220781b27b2c6bc91527b22a04af286791e41c28a"
               "0xfd310be38374fb2da856236f50c317183f9ae310ac64acc67b0ea7504b4a65bf"
               "0xaa9f2a6bd2236a45a29502b1d9d6e60123d1b38a4d61b9e396503dc07ee4ab12"))
           (transactionsRoot
            . "0x681c8e89df028e0c0d0fb8c3c3dc8a4185b9ea50aab92f381704fd7cfa5a1f52")
           (uncles . ())
           (withdrawals . ())
           (withdrawalsRoot
            . "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")))

(define full-block-json
  (hash-set block-json 'transactions
            (list transaction-info-json/0 transaction-info-json/1 transaction-info-json/2)))

(define pending-block-json
  (hash-set* block-json 'hash 'null 'miner 'null 'nonce 'null))

(define fee-history-json
  '#hasheq((baseFeePerBlobGas . ("0x1" "0x1" "0x1"))
           (baseFeePerGas . ("0x3b9aca00" "0x3b9aca00" "0x342fff82"))
           (blobGasUsedRatio . (0.0 0.0))
           (gasUsedRatio . (0.0 0.0022433333333333333))
           (oldestBlock . "0x0")
           (reward . (("0x0" "0x0") ("0x77359400" "0x6c088e200")))))

(define log-json
  '#hasheq((address . "0x5fbdb2315678afecb367f032d93f642f64180aa3")
           (blockHash . "0x78e3cb19177d24232982259d6717f8ea45cfae5f369be21280af7b8e535146b5")
           (blockNumber . "0x2")
           (blockTimestamp . "0x6b01fb55")
           (data . "0x00000000000000000000000000000000000000000000000000000000000000fa")
           (logIndex . "0x0")
           (removed . #f)
           (topics . ("0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"
                      "0x000000000000000000000000f39fd6e51aad88f6f4ce6ab8827279cfffb92266"
                      "0x00000000000000000000000070997970c51812dc3a010c7d01b50e0d17dc79c8"))
           (transactionHash . "0xf6530f5992a7b2b0ff8b8014dc54ad20e0f16ae89343cfb98640c6a7d94cf5d4")
           (transactionIndex . "0x0")))

(define receipt-json
  (hash-set
   '#hasheq((blockHash . "0x78e3cb19177d24232982259d6717f8ea45cfae5f369be21280af7b8e535146b5")
            (blockNumber . "0x2")
            (blockTimestamp . 1795291989)
            (contractAddress . null)
            (cumulativeGasUsed . "0xcbf6")
            (effectiveGasPrice . "0x705f9991")
            (from . "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266")
            (gasUsed . "0xcbf6")
            (logsBloom
             . "0x00000000000000000002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000000000000000000000000000000840000000000000000100000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000000000000000000042000000200000000000000000000000002000000000000000000000000000000000000000000000000000000001000000000000000000000000000000")
            (status . "0x1")
            (to . "0x5fbdb2315678afecb367f032d93f642f64180aa3")
            (transactionHash . "0xf6530f5992a7b2b0ff8b8014dc54ad20e0f16ae89343cfb98640c6a7d94cf5d4")
            (transactionIndex . "0x0")
            (type . "0x2"))
   'logs (list log-json)))

(define root-receipt-json
  (hash-set (hash-remove receipt-json 'status)
            'root "0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef"))
