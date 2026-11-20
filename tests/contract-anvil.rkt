#lang racket/base

;; Deploying, calling and sending to the fixture contracts on anvil.
;; Skipped when anvil is not available.

(module+ test
  (require racket/list
           rackunit
           web3
           "../examples/common.rkt"
           "util.rkt")

  (define token-code (artifact-bytecode "Token"))
  (define counter-code (artifact-bytecode "Counter"))
  (define create2-deployer "0x4e59b44847b379578588920cA78FbF26c0B4956C")
  (define errors (list insufficient-balance "Unauthorized(address)"))
  (define transfer "transfer(address,uint256)")

  (define (nonce account)
    (eth-get-transaction-count account "latest"))

  (define (balance-of token account)
    (contract-call token "balanceOf(address)(uint256)" account))

  (define (revert-string e)
    (abi-revert->string
     (if (exn:fail:web3:transaction? e)
         (exn:fail:web3:transaction-revert e)
         (exn:fail:web3:rpc:revert-revert e))))

  (define (deploy-token)
    (deploy-contract token-code "constructor(string,string,uint256)" "Token" "TOK" 1000000))

  (define (check-deploy-and-read)
    (define before (nonce anvil-alice))
    (define token (deploy-token))
    (check-equal? token (create-address anvil-alice before))
    (check-equal? (string-downcase (eth-get-code token "latest")) (artifact-deployed-bytecode "Token"))
    (define-values (again receipt)
      (deploy-contract/receipt token-code "constructor(string,string,uint256)" "Token" "TOK" 1000000))
    (check-equal? (address->checksum-string (hash-ref receipt 'contractAddress)) again)
    (check-equal? (contract-call token "name()(string)") "Token")
    (check-equal? (call-with-values (lambda () (contract-call token "info()(string,string,uint8,uint256)"))
                                    list)
                  '("Token" "TOK" 18 1000000))
    (check-equal? (call-with-values
                   (lambda () (contract-call token "balancesOf(address[])(uint256[])"
                                             (list anvil-alice anvil-bob)))
                   list)
                  '((1000000 0)))
    (check-equal? (contract-call token "owner()(address)") anvil-alice))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping contract anvil tests: anvil not found (set WEB3_ANVIL or use `nix develop`)")]
    [else
     (with-anvil ()
       (parameterize ([current-account anvil-alice])
         (test-case "a deployed token is at its CREATE address and reads back its state"
           (check-deploy-and-read))

         (define token (deploy-token))

         (test-case "a send returns its receipt once mined"
           (define receipt (contract-send token transfer anvil-bob 250))
           (check-true (receipt-succeeded? receipt))
           (check-equal? (map (lambda (log) (first (hash-ref log 'topics))) (hash-ref receipt 'logs))
                         '("0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"))
           (check-equal? (balance-of token anvil-alice) 999750)
           (check-equal? (balance-of token anvil-bob) 250)
           (check-true (contract-call token "transfer(address,uint256)(bool)" anvil-bob 1))
           (check-equal? (balance-of token anvil-bob) 250))

         (test-case "a custom error is the same revert on call, on estimate and when mined"
           (define before (nonce anvil-carol))
           (define on-call
             (raised (lambda () (contract-call token "transfer(address,uint256)(bool)" anvil-alice 1
                                               #:from anvil-carol #:errors errors))))
           (check-pred exn:fail:web3:rpc:revert? on-call)
           (check-equal? (nonce anvil-carol) before)
           (define on-estimate
             (raised (lambda () (contract-send token transfer anvil-alice 1 #:from anvil-carol #:errors errors))))
           (check-pred exn:fail:web3:rpc:revert? on-estimate)
           (check-equal? (exn:fail:web3:rpc-method on-estimate) "eth_estimateGas")
           (check-equal? (nonce anvil-carol) before)
           (define on-chain
             (raised (lambda () (contract-send token transfer anvil-alice 1
                                               #:from anvil-carol #:gas 100000 #:errors errors))))
           (check-pred exn:fail:web3:transaction? on-chain)
           (check-equal? (hash-ref (exn:fail:web3:transaction-receipt on-chain) 'status) "0x0")
           (check-equal? (nonce anvil-carol) (add1 before))
           (define reverts
             (list (exn:fail:web3:rpc:revert-revert on-call)
                   (exn:fail:web3:rpc:revert-revert on-estimate)
                   (exn:fail:web3:transaction-revert on-chain)))
           (for ([r (in-list reverts)])
             (check-equal? r (first reverts))
             (check-equal? (abi-revert-name r) "InsufficientBalance")
             (check-equal? (abi-revert-arguments r) '(0 1))))

         (test-case "require, panics and custom errors on send decode"
           (check-equal? (revert-string (raised (lambda () (contract-send token "deposit(string)" "hi"))))
                         "Error(\"no value\")")
           (check-equal? (revert-string (raised (lambda () (contract-send token "burn(uint256)" 1
                                                                          #:from anvil-carol))))
                         "Panic(0x11): arithmetic underflow or overflow")
           (check-equal? (revert-string (raised (lambda () (contract-send token "mint(address,uint256)"
                                                                          anvil-bob 1
                                                                          #:from anvil-bob
                                                                          #:errors '("Unauthorized(address)")))))
                         (format "Unauthorized(~a)" anvil-bob))
           (define unnamed
             (raised (lambda () (contract-call token "transfer(address,uint256)(bool)" anvil-alice 1
                                               #:from anvil-carol))))
           (check-equal? (revert-string unnamed) "unknown error 0xcf479181 with 64 bytes of data"))

         (test-case "#:value pays a payable function"
           (contract-send token "deposit(string)" "first" #:value 1000)
           (check-equal? (contract-call token "deposits(address)(uint256)" anvil-alice) 1000)
           (check-equal? (eth-get-balance token "latest") 1000))

         (test-case "a reverting constructor raises its reason, estimated or mined"
           (define refused
             (raised (lambda () (deploy-contract token-code "constructor(string,string,uint256)" "" "X" 1))))
           (check-pred exn:fail:web3:rpc:revert? refused)
           (check-equal? (revert-string refused) "Error(\"empty name\")")
           (define mined (raised (lambda ()
                                   (deploy-contract token-code "constructor(string,string,uint256)" "" "X" 1
                                                    #:gas 3000000))))
           (check-pred exn:fail:web3:transaction? mined)
           (check-equal? (revert-string mined) "Error(\"empty name\")"))

         (test-case "a send that runs out of gas raises with no revert"
           (define e (raised (lambda () (contract-send token transfer anvil-bob 1 #:gas 30000))))
           (check-pred exn:fail:web3:transaction? e)
           (check-false (exn:fail:web3:transaction-revert e))
           (check-equal? (hash-ref (exn:fail:web3:transaction-receipt e) 'status) "0x0"))

         (test-case "a sender the node does not manage is the node's RPC error"
           (define e (raised (lambda () (contract-send token transfer anvil-bob 0
                                                       #:from "0x000000000000000000000000000000000000bEEF"))))
           (check-pred exn:fail:web3:rpc? e)
           (check-false (exn:fail:web3:rpc:revert? e))
           (check-equal? (exn:fail:web3:rpc-code e) -32602)
           (check-equal? (exn:fail:web3:rpc-method e) "eth_sendTransaction"))

         (test-case "contracts deploy at their create-address for every nonce width"
           (define accounts (eth-accounts))
           (define (check-deploy account name)
             (define expected (create-address account (nonce account)))
             (check-equal? (deploy-contract counter-code #:from account) expected name))
           (define fresh (list-ref accounts 5))
           (check-deploy fresh "nonce 0")
           (check-deploy fresh "nonce 1")
           (define (deploy-at account n)
             (rpc-call "anvil_setNonce" (list account (number->hex-string n)))
             (check-deploy account (format "nonce ~a" n)))
           (for ([n (in-list (list 127 128 255 256 65535 65536 (expt 2 32)))])
             (deploy-at fresh n))
           (deploy-at (list-ref accounts 6) (- (expt 2 64) 2)))

         (test-case "a CREATE2 deployment lands at create2-address"
           (check-not-equal? (eth-get-code create2-deployer "latest") "0x")
           (define salt 42)
           (define data
             (bytes->hex-string (bytes-append (word salt) (hex-string->bytes counter-code))))
           (define predicted (create2-address create2-deployer salt counter-code))
           (define returned (eth-call (hasheq 'to create2-deployer 'data data) "latest"))
           (check-equal? (address->checksum-string returned) predicted)
           (check-true (receipt-succeeded?
                        (send-transaction/receipt (hasheq 'from anvil-alice 'to create2-deployer 'data data))))
           (check-equal? (string-downcase (eth-get-code predicted "latest")) (artifact-deployed-bytecode "Counter"))
           (contract-send predicted "increment()")
           (check-equal? (contract-call predicted "count()(uint256)") 1))

         (test-case "calling an account with no code is short return data"
           (define e (raised (lambda () (contract-call anvil-bob "balanceOf(address)(uint256)" anvil-bob))))
           (check-pred exn:fail:web3:abi:decode? e)
           (check-equal? (exn:fail:web3:abi:decode-reason e) 'short))))

     (with-anvil (#:hardfork "cancun")
       (parameterize ([current-account anvil-alice])
         (test-case "the token deploys and reads back on cancun, the artifacts' target"
           (check-deploy-and-read))))]))
