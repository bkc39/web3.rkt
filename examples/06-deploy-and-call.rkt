#lang racket/base

;; 06: deploying contracts, calling them, and sending them transactions.
;;
;; The contracts are examples/contracts/Token.sol and Counter.sol, deployed
;; from their compiled artifacts in examples/contracts/out/.
;;
;;   racket examples/06-deploy-and-call.rkt
;;   racket -i -e '(enter! "examples/06-deploy-and-call.rkt")'

(require (only-in racket/list first take)
         web3
         "common.rkt")

(provide (all-defined-out))

(section "the node and three accounts")

(define node (start-anvil))
(current-provider (anvil-provider node))

(define-values (alice bob carol)
  (apply values (map address->checksum-string (take (eth-accounts) 3))))
(show (list alice bob carol))
(current-account alice)

(section "deploying with constructor arguments")

(define token-code (artifact-bytecode "Token"))
(show (bytes-length (hex-string->bytes token-code)))

(show (create-address alice (eth-get-transaction-count alice "latest")))
(define token (deploy-contract token-code "constructor(string,string,uint256)" "Token" "TOK" 1000000))
(show token)

(section "reading")

(show (contract-call token "name()(string)"))
(show (contract-call token "owner()(address)"))
(show (contract-call token "balanceOf(address)(uint256)" alice))
(show (contract-call token "info()(string,string,uint8,uint256)"))
(define-values (name symbol decimals supply) (contract-call token "info()(string,string,uint8,uint256)"))
(show (list name symbol decimals supply))
(show (contract-call token "balancesOf(address[])(uint256[])" (list alice bob)))

(section "sending")

(define transfer "transfer(address,uint256)")
(define transfer/bool "transfer(address,uint256)(bool)")

(define transfer-tx
  (hasheq 'from alice 'to token 'data (bytes->hex-string (encode-call transfer bob 250))))
(show (eth-estimate-gas transfer-tx))
(define receipt (contract-send token transfer bob 250))
(show (receipt-succeeded? receipt))
(show (hash-ref receipt 'blockNumber))
(show (hex-string->number (hash-ref receipt 'gasUsed)))
(show (length (hash-ref receipt 'logs)))
(show (hash-ref (first (hash-ref receipt 'logs)) 'topics))
(show (contract-call token "balancesOf(address[])(uint256[])" (list alice bob)))

(show (contract-call token transfer/bool bob 1))
(show (contract-call token "balancesOf(address[])(uint256[])" (list alice bob)))

(section "custom errors")

(define errors '("InsufficientBalance(uint256,uint256)" "Unauthorized(address)"))

(show-error (contract-call token transfer/bool alice 1 #:from carol #:errors errors) #:as refused)
(show (abi-revert-name (exn:fail:web3:rpc:revert-revert refused)))
(show (abi-revert-arguments (exn:fail:web3:rpc:revert-revert refused)))

(show (eth-get-transaction-count carol "latest"))
(show-error (contract-send token transfer alice 1 #:from carol #:errors errors) #:as refused-estimate)
(show (exn:fail:web3:rpc-method refused-estimate))
(show (eth-get-transaction-count carol "latest"))

(show-error (contract-call token transfer/bool alice 1 #:from carol))

(section "a transaction that is mined and fails")

(show-error (contract-send token transfer bob 1 #:from carol #:gas 90000 #:errors errors) #:as failed)
(show (receipt-succeeded? (exn:fail:web3:transaction-receipt failed)))
(show (hex-string->number (hash-ref (exn:fail:web3:transaction-receipt failed) 'gasUsed)))
(show (abi-revert-arguments (exn:fail:web3:transaction-revert failed)))
(show (eth-get-transaction-count carol "latest"))

(show-error (contract-send token transfer bob 1 #:gas 30000) #:as out-of-gas)
(show (exn:fail:web3:transaction-revert out-of-gas))

(section "require and panic")

(show-error (contract-send token "deposit(string)" "hi"))
(show-error (contract-send token "burn(uint256)" 1 #:from carol))
(define mint "mint(address,uint256)")
(show-error (contract-send token mint bob 1000 #:from bob #:errors errors))

(section "paying")

(show (eth-get-balance token "latest"))
(show (receipt-succeeded? (contract-send token "deposit(string)" "first" #:value 1000)))
(show (contract-call token "deposits(address)(uint256)" alice))
(show (eth-get-balance token "latest"))

(section "another sender")

(define bob-receipt
  (parameterize ([current-account bob])
    (contract-send token transfer carol 50)))
(show (hash-ref bob-receipt 'from))
(show (hash-ref (contract-send token transfer carol 50 #:from bob) 'from))
(show (contract-call token "balancesOf(address[])(uint256[])" (list alice bob carol)))

(section "a failing constructor")

(show-error (deploy-contract token-code "constructor(string,string,uint256)" "" "X" 1))

(section "contract addresses")

(define counter-code (artifact-bytecode "Counter"))
(define deployer "0x4e59b44847b379578588920cA78FbF26c0B4956C")
(define salt 42)
(define predicted (create2-address deployer salt counter-code))
(show predicted)
(show (eth-get-code predicted "latest"))
(define create2-tx
  (hasheq 'from alice
          'to deployer
          'data (bytes->hex-string (bytes-append (word->bytes salt) (hex-string->bytes counter-code)))))
(show (eth-call create2-tx "latest"))
(show (receipt-succeeded? (send-transaction/receipt create2-tx)))
(show (receipt-succeeded? (contract-send predicted "increment()")))
(show (contract-call predicted "count()(uint256)"))

(show (create-address alice (eth-get-transaction-count alice "latest")))
(define-values (counter counter-receipt) (deploy-contract/receipt counter-code))
(show counter)
(show (hash-ref counter-receipt 'contractAddress))
(show (hex-string->number (hash-ref counter-receipt 'gasUsed)))
(show (contract-call counter "count()(uint256)"))
