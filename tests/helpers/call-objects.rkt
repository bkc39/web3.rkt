#lang racket/base

(require (only-in web3 bytes->hex-string)
         (only-in "../util.rkt" anvil-alice anvil-bob anvil-carol))

(provide call-object-rows)

(define storage-key (make-bytes 32 1))

(define call-object-rows
  `((() ,anvil-alice ,anvil-bob #hasheq((from . ,anvil-alice) (to . ,anvil-bob)))
    (() #f ,anvil-bob #hasheq((to . ,anvil-bob)))
    (() ,(string-downcase anvil-alice) ,(string-downcase anvil-bob)
        #hasheq((from . ,anvil-alice) (to . ,anvil-bob)))
    ((#:data #"\x60\x80") ,anvil-alice #f #hasheq((from . ,anvil-alice) (data . "0x6080")))
    ((#:data #"" #:value 0) ,anvil-alice ,anvil-bob
                            #hasheq((from . ,anvil-alice) (to . ,anvil-bob)))
    ((#:value 5 #:gas-limit 30000) #f ,anvil-bob
     #hasheq((to . ,anvil-bob) (value . "0x5") (gas . "0x7530")))
    ((#:type 0 #:gas-price 7 #:nonce 0) ,anvil-alice ,anvil-bob
     #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (type . "0x0") (gasPrice . "0x7")
             (nonce . "0x0")))
    ((#:access-list ()) ,anvil-alice ,anvil-bob
     #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (accessList . ())))
    ((#:value 1 #:data #"\1\2" #:nonce 3 #:gas-limit 30000 #:chain-id 31337 #:type 2
      #:max-priority-fee-per-gas 1 #:max-fee-per-gas 2
      #:access-list ((,(string-downcase anvil-carol) ,storage-key)))
     ,anvil-alice ,anvil-bob
     #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (value . "0x1") (data . "0x0102")
             (nonce . "0x3") (gas . "0x7530") (chainId . "0x7a69") (type . "0x2")
             (maxPriorityFeePerGas . "0x1") (maxFeePerGas . "0x2")
             (accessList . (#hasheq((address . ,anvil-carol)
                                    (storageKeys . (,(bytes->hex-string storage-key))))))))))
