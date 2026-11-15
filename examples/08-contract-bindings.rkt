#lang racket/base

;; 08: binding contracts.
;;
;; define-contract reads a contract's ABI when this file is compiled and binds a
;; procedure per function, named token-balance-of for balanceOf.  Calls return
;; values, sends return receipts, and reverts come back decoded by name.
;;
;;   racket examples/08-contract-bindings.rkt
;;   racket -i -e '(enter! "examples/08-contract-bindings.rkt")'

(require racket/match
         web3
         "common.rkt")

(provide (all-defined-out))

(section "the node and three accounts")

(define node (start-anvil))
(current-provider (anvil-provider node))
(match-define (list alice bob carol _ ...) (map address->checksum-string (eth-accounts)))
(current-account alice)
(show (list alice bob carol))

(section "a binding from a compiled artifact")

(define-contract token #:file "contracts/out/Token.sol/Token.json")
(define transfer-function (contract-abi-function token 'transfer))
(show token)
(show (abi-item->string transfer-function))
(show token-transfer)
(show token-transfer-event)

(section "deploying: the constructor's arguments, checked against its inputs")

(define gold (token-deploy "Gold" "GLD" 1000000))
(show gold)

(section "reading: one value per output")

(show (token-name gold))
(show (token-balance-of gold alice))
(show (token-info gold))
(show (token-balances-of gold (list alice bob)))

(section "sending: a receipt, and the events it holds")

(define sent (token-transfer gold bob 250))
(show (receipt-succeeded? sent))
(show (receipt-logs sent #:events (contract-abi-events token)))
(show (token-balance-of gold bob))
(show (receipt-succeeded? (token-deposit gold "first" #:value 1000)))
(show (token-deposits gold alice))

(section "custom errors, decoded by name; a refused estimate sends nothing")

(show-error (token-transfer gold alice 1 #:from carol) #:as refused)
(show (abi-revert-name (exn->abi-revert refused)))
(show (abi-revert-arguments (exn->abi-revert refused)))
(show-error (token-mint gold carol 5 #:from bob))
(show-error (token-burn gold 1 #:from carol))

(section "events by name")

(define from-alice-or-bob (hasheq 'from (any-of alice bob)))
(show (contract-logs gold token-transfer-event #:where (hasheq 'to bob)))
(show (contract-logs gold token-transfer-event #:where from-alice-or-bob))
(show (get-logs (log-filter #:address gold #:event token-transfer-event #:where from-alice-or-bob)))

(section "three functions and no artifact")

(define-contract erc20
  "function balanceOf(address owner) view returns (uint256)"
  "function transfer(address to, uint256 amount) returns (bool)"
  "event Transfer(address indexed from, address indexed to, uint256 value)"
  "error InsufficientBalance(uint256 have, uint256 want)")
(show erc20)
(show (erc20-balance-of gold bob))
(show (receipt-succeeded? (erc20-transfer gold carol 5)))
(show-error (erc20-transfer gold alice 10 #:from carol))

(section "s-expressions, with bytecode from elsewhere")

(define-contract counter
  (function count () #:view #:returns ([#f uint256]))
  (function increment () #:returns ([#f uint256]))
  #:bytecode (contract-abi-bytecode (file->contract-abi (artifact-path "Counter"))))
(show counter)
(define tally (counter-deploy))
(show (receipt-succeeded? (counter-increment tally)))
(show (counter-count tally))

(section "call data without sending")

(show-words (encode-call transfer-function bob 250))
(show (decode-call transfer-function (encode-call transfer-function bob 250)))

(section "a contract instance from JSON at run time")

(define token-abi (file->contract-abi (artifact-path "Token")))
(show (equal? token-abi token))
(define silver (deploy-contract-instance token-abi "Silver" "SLV" 500))
(show silver)
(show (contract-call silver 'balanceOf alice))
(show (receipt-succeeded? (contract-send silver 'transfer bob 5)))
(show (contract-estimate-gas silver 'transfer bob 5))
(show (contract-logs silver))

(section "a state-changing function called, not sent")

(define gold-token (contract-instance token gold))
(show (contract-call gold-token 'transfer bob 1))
(show (token-balance-of gold bob))

(section "plain data, as a compiler would emit it")

(define items
  '((function balanceOf ([owner address]) #:view #:returns ([#f uint256]))
    (error InsufficientBalance ([have uint256] [want uint256]))))
(define tiny (contract-instance (contract-abi items) gold))
(show tiny)
(show (contract-call tiny 'balanceOf bob))
(show (map abi-item->string (contract-abi-items (contract-abi items))))

(section "overloads: one procedure, picked by the number of arguments")

(define-contract nft
  "function safeTransferFrom(address from, address to, uint256 id)"
  "function safeTransferFrom(address from, address to, uint256 id, bytes data)")
(show nft-safe-transfer-from)
(show (abi-item->string (contract-abi-function nft 'safeTransferFrom #:arity 4)))
(show (bytes->hex-string
       (function-selector (contract-abi-function nft 'safeTransferFrom #:arity 3))))

(section "mistakes, caught before anything is sent")

(show-error (contract-abi-function nft 'safeTransferFrom))
(show-error (contract-call gold-token 'transfr bob 1))
(show-error (token-balance-of gold))
(show-error (token-transfer gold bob 1 #:value 5))
(show-error (token-transfer gold bob -1))
(show-error (define-contract broken "function f() veiw"))
