#lang racket/base

;; 06: deploying contracts, calling them, and sending them transactions.
;;
;; The contracts are examples/contracts/Token.sol and Counter.sol, deployed
;; from their compiled artifacts in examples/contracts/out/.
;;
;;   racket examples/06-deploy-and-call.rkt
;;   racket -i -e '(enter! "examples/06-deploy-and-call.rkt")'

(require racket/match
         web3
         "common.rkt")

(provide (all-defined-out))

(section "the node and three accounts")

(define node (start-anvil))
(current-provider (anvil-provider node))

(match-define (list alice bob carol _ ...) (map address->checksum-string (eth-accounts)))
(show (list alice bob carol))
(current-account alice)

(section "deploying with constructor arguments")

(define token-code (artifact-bytecode "Token"))
(define predicted-token (create-address alice (eth-get-transaction-count alice "latest")))
(show predicted-token)
(define token (deploy-contract token-code "constructor(string,string,uint256)" "Token" "TOK" 1000000))
(show token)
(show (equal? token predicted-token))

(section "reading")

(show (contract-call token "name()(string)"))
(show (contract-call token "owner()(address)"))
(show (contract-call token "balanceOf(address)(uint256)" alice))
(show (contract-call token "balancesOf(address[])(uint256[])" (list alice bob)))

(section "several outputs are several values")

(show (contract-call token "info()(string,string,uint8,uint256)"))
(define-values (name symbol decimals supply) (contract-call token "info()(string,string,uint8,uint256)"))
(show symbol)

(section "sending")

(show (contract-call token "balancesOf(address[])(uint256[])" (list alice bob)))
(define receipt (contract-send token "transfer(address,uint256)" bob 250))
(show (receipt-succeeded? receipt))
(show (hash-ref receipt 'blockNumber))
(show (hash-ref receipt 'gasUsed))
(show (length (hash-ref receipt 'logs)))
(show (contract-call token "balancesOf(address[])(uint256[])" (list alice bob)))

(show (contract-call token "transfer(address,uint256)(bool)" bob 1))
(show (contract-call token "balancesOf(address[])(uint256[])" (list alice bob)))

(section "reverts")

(define errors '("InsufficientBalance(uint256,uint256)" "Unauthorized(address)"))

(show-error (contract-call token "transfer(address,uint256)(bool)" alice 1 #:from carol #:errors errors)
            #:as refused)
(show (abi-revert-name (exn->abi-revert refused)))
(show (abi-revert-arguments (exn->abi-revert refused)))

(show-error (contract-call token "transfer(address,uint256)(bool)" alice 1 #:from carol))
(show-error (contract-send token "deposit(string)" "hi"))
(show-error (contract-send token "burn(uint256)" 1 #:from carol))

(section "a revert caught before it is mined sends nothing")

(define carol-before-refusal (eth-get-transaction-count carol "latest"))
(show-error (contract-send token "transfer(address,uint256)" alice 1 #:from carol #:errors errors))
(show (list carol-before-refusal (eth-get-transaction-count carol "latest")))

(section "a transaction that is mined and fails")

(define carol-before-failure (eth-get-transaction-count carol "latest"))
(show-error (contract-send token "transfer(address,uint256)" bob 1 #:from carol #:gas 90000 #:errors errors)
            #:as failed)
(show (list carol-before-failure (eth-get-transaction-count carol "latest")))
(show (receipt-succeeded? (exn:fail:web3:transaction-receipt failed)))
(show (abi-revert->string (exn->abi-revert failed)))

(show-error (contract-send token "transfer(address,uint256)" bob 1 #:gas 30000) #:as out-of-gas)
(show (exn->abi-revert out-of-gas))

(section "paying")

(show (eth-get-balance token "latest"))
(show (receipt-succeeded? (contract-send token "deposit(string)" "first" #:value 1000)))
(show (contract-call token "deposits(address)(uint256)" alice))
(show (eth-get-balance token "latest"))

(section "another sender")

(define bob-receipt
  (parameterize ([current-account bob])
    (contract-send token "transfer(address,uint256)" carol 50)))
(show (hash-ref bob-receipt 'from))
(show (hash-ref (contract-send token "transfer(address,uint256)" carol 50 #:from bob) 'from))
(show (contract-call token "balancesOf(address[])(uint256[])" (list alice bob carol)))

(section "a failing constructor")

(show-error (deploy-contract token-code "constructor(string,string,uint256)" "" "X" 1))

(section "contract addresses")

(define counter-code (artifact-bytecode "Counter"))
(show (create2-address "0x4e59b44847b379578588920cA78FbF26c0B4956C" 42 counter-code))

(define predicted-counter (create-address alice (eth-get-transaction-count alice "latest")))
(show predicted-counter)
(define-values (counter counter-receipt) (deploy-contract/receipt counter-code))
(show (equal? counter predicted-counter))
(show (hash-ref counter-receipt 'contractAddress))
(show (receipt-succeeded? (contract-send counter "increment()")))
(show (contract-call counter "count()(uint256)"))
