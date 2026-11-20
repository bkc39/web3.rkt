#lang racket/base

;; Bound contracts on anvil, end to end: deploy, call, send, events and
;; custom errors by name, against the committed solc artifacts.  Skipped
;; when anvil is not available.

(module+ test
  (require json
           racket/runtime-path
           rackunit
           web3
           "util.rkt")

  (define-runtime-path token-file "../examples/contracts/out/Token.sol/Token.json")

  (define-contract token #:file "../examples/contracts/out/Token.sol/Token.json")
  (define-contract erc20
    "function balanceOf(address owner) view returns (uint256)"
    "function transfer(address to, uint256 amount) returns (bool)"
    "error InsufficientBalance(uint256 have, uint256 want)")
  (define-contract counter #:file "../examples/contracts/out/Counter.sol/Counter.json")
  (define-contract events #:file "../examples/contracts/out/Events.sol/Events.json")
  (define-contract sexp-counter
    (function count () #:view #:returns ([#f uint256]))
    (function increment ())
    #:bytecode (contract-abi-bytecode counter))

  (define bezzle-items
    '((function balanceOf ([#f address]) #:view #:returns ([#f uint256]))
      (function transfer ([#f address] [#f uint256]) #:returns ([#f bool]))
      (error BezzleError ([#f uint256]))))

  (define (check-reverts thunk pred name arguments)
    (define e (raised thunk))
    (check-pred pred e)
    (when (pred e)
      (check-equal? (abi-revert-name (exn->abi-revert e)) name)
      (check-equal? (abi-revert-arguments (exn->abi-revert e)) arguments)))

  (define (nonce account)
    (eth-get-transaction-count account "latest"))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping contract binding tests: anvil not found (set WEB3_ANVIL or use `nix develop`)")]
    [else
     (with-anvil ()
       (parameterize ([current-account anvil-alice])
         (define before (nonce anvil-alice))
         (define gold (token-deploy "Gold" "GLD" 1000000))

         (test-case "a bound deploy creates the contract at its CREATE address"
           (check-equal? gold (create-address anvil-alice before))
           (check-equal? (string-downcase (eth-get-code gold "latest"))
                         (hash-ref (hash-ref (call-with-input-file token-file read-json) 'deployedBytecode)
                                   'object)))

         (test-case "bound calls return the contract's outputs"
           (check-equal? (token-balance-of gold anvil-alice) 1000000)
           (check-equal? (call-with-values (lambda () (token-info gold)) list) '("Gold" "GLD" 18 1000000))
           (check-equal? (token-balances-of gold (list anvil-alice anvil-bob)) '(1000000 0)))

         (test-case "a bound send transfers, and its event decodes by name from the receipt and the chain"
           (define sent (token-transfer gold anvil-bob 250))
           (check-true (receipt-succeeded? sent))
           (check-equal? (token-balance-of gold anvil-bob) 250)
           (define logs (receipt-logs sent #:events (contract-abi-events token)))
           (check-equal? (map event-log-name logs) '("Transfer"))
           (check-equal? (event-log-ref (car logs) 'value) 250)
           (check-equal? (contract-logs gold token-transfer-event #:where (hasheq 'to anvil-bob)) logs)
           (check-equal? (map (lambda (l) (event-log-ref l 'value))
                              (contract-logs (contract-instance token gold) 'Transfer))
                         '(1000000 250)))

         (test-case "a bound event filters get-logs, and contract-logs takes any of several values"
           (define to-bob (get-logs (log-filter #:address gold #:event token-transfer-event
                                                #:where (hasheq 'to anvil-bob))))
           (check-equal? (map (lambda (l) (event-log-ref l 'value)) to-bob) '(250))
           (check-equal? to-bob
                         (contract-logs gold token-transfer-event #:where (hasheq 'to anvil-bob)))
           (check-equal? (map (lambda (l) (event-log-ref l 'to))
                              (contract-logs gold token-transfer-event
                                             #:where (hasheq 'to (any-of anvil-alice anvil-bob))))
                         (list anvil-alice anvil-bob)))

         (test-case "custom errors decode by name with no #:errors"
           (define carol-nonce (nonce anvil-carol))
           (check-reverts (lambda () (token-transfer gold anvil-alice 1 #:from anvil-carol))
                          exn:fail:web3:rpc:revert? "InsufficientBalance" '(0 1))
           (check-equal? (nonce anvil-carol) carol-nonce)
           (check-reverts (lambda () (token-transfer gold anvil-alice 1 #:from anvil-carol #:gas 100000))
                          exn:fail:web3:transaction? "InsufficientBalance" '(0 1))
           (check-reverts (lambda () (token-mint gold anvil-carol 5 #:from anvil-bob))
                          exn:fail:web3:rpc:revert? "Unauthorized" (list anvil-bob))
           (define panic (raised (lambda () (token-burn gold 1 #:from anvil-carol))))
           (check-equal? (abi-revert->string (exn->abi-revert panic)) "Panic(0x11): arithmetic underflow or overflow"))

         (test-case "an inline binding works against the same contract"
           (check-equal? (erc20-balance-of gold anvil-bob) 250)
           (check-reverts (lambda () (erc20-transfer gold anvil-alice 1 #:from anvil-carol))
                          exn:fail:web3:rpc:revert? "InsufficientBalance" '(0 1)))

         (test-case "descriptions and instances at run time agree with the binding"
           (check-equal? (file->contract-abi token-file) token)
           (define silver (deploy-contract-instance token "Silver" "SLV" 500))
           (check-equal? (contract-call silver 'balanceOf anvil-alice) 500)
           (check-equal? (contract-call (contract-instance token gold) 'transfer anvil-bob 1) #t)
           (check-equal? (token-balance-of gold anvil-bob) 250)
           (check-true (exact-positive-integer? (contract-estimate-gas silver 'transfer anvil-bob 5))))

         (test-case "payable functions, Counter and Events through bindings"
           (token-deposit gold "hi" #:value 1000)
           (check-equal? (token-deposits gold anvil-alice) 1000)
           (define tally (counter-deploy))
           (counter-increment tally)
           (check-equal? (counter-count tally) 1)
           (define sexp-tally (sexp-counter-deploy))
           (sexp-counter-increment sexp-tally)
           (check-equal? (sexp-counter-count sexp-tally) 1)
           (define e (events-deploy))
           (define flagged (events-flags e #t -1 #"abc"))
           (check-equal? (map event-log-arguments (receipt-logs flagged #:events (list events-flags-event)))
                         '((#t -1 #"abc"))))

         (test-case "a compiler's item data reads the deployed token"
           (check-equal? (contract-call (contract-instance (contract-abi bezzle-items) gold) 'balanceOf anvil-bob)
                         250))))]))
