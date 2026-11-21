#lang racket/base

(provide log-json
         receipt-json
         root-receipt-json)

;; Captured from the pinned anvil: a Token transfer and its one log.  The
;; receipt's blockTimestamp is a JSON number, its log's a hex quantity.
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
