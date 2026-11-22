#lang racket/base

(module+ test
  (require (only-in racket/contract exn:fail:contract:blame?)
           racket/list
           racket/match
           rackunit
           (only-in libsecp256k1 integer->secret-key)
           web3
           (only-in web3/private/binding-rules all-keywords)
           "helpers/mock.rkt"
           (only-in "helpers/secrets.rkt" check-no-leak marker marker-hex marker-integer)
           "helpers/signers.rkt"
           (only-in "util.rkt" anvil-alice anvil-bob anvil-carol))

  (define-contract token-binding #:file "../examples/contracts/out/Token.sol/Token.json")
  (define-contract counter #:file "../examples/contracts/out/Counter.sol/Counter.json")

  (define token-abi
    (file->contract-abi (collection-file-path "examples/contracts/out/Token.sol/Token.json" "web3")))
  (define counter-abi
    (file->contract-abi
     (collection-file-path "examples/contracts/out/Counter.sol/Counter.json" "web3")))
  (define payable-abi (contract-abi '("constructor() payable") #:bytecode #"\x60\x80"))
  (define deployed (hash-set mined 'contractAddress token))
  (define node (hash-set (filling #:receipt deployed) "eth_sendTransaction" tx-hash))
  (define transfer-data (bytes->hex-string (encode-call "transfer(address,uint256)" anvil-bob 10)))
  (define transfer-tx (hasheq 'from anvil-alice 'to token 'data transfer-data))

  (define (call-param calls method)
    (match (assoc method calls)
      [(list _ param _ ...) param]
      [#f #f]))

  (test-case "K2: a contract send from a signer is filled, signed and sent raw"
    (define-values (result calls)
      (run-mock node (lambda () (contract-send token "transfer(address,uint256)" anvil-bob 10
                                               #:from signer0))))
    (check-equal? (receipt->jsexpr (first result))
                  (hash-set deployed 'transactionHash (call-param calls "eth_getTransactionReceipt")))
    (check-equal? (method-names calls)
                  '("eth_chainId" "eth_estimateGas" "eth_getTransactionCount"
                    "eth_getBlockByNumber" "eth_maxPriorityFeePerGas" "eth_sendRawTransaction"
                    "eth_getTransactionReceipt"))
    (check-equal? (call-param calls "eth_estimateGas") transfer-tx)
    (match-define (list tx) (raw-transactions calls))
    (check-equal? (list (transaction-to tx) (bytes->hex-string (transaction-data tx))
                        (transaction-value tx) (transaction-gas-limit tx))
                  (list token transfer-data 0 21000)))

  (define entry-points
    `((contract-send
       ,(lambda (from) (contract-send token "transfer(address,uint256)" anvil-bob 10 #:from from)))
      (deploy-contract ,(lambda (from) (deploy-contract #"\x60\x80" #:from from)))
      (deploy-contract/receipt ,(lambda (from) (deploy-contract/receipt #"\x60\x80" #:from from)))
      (deploy-contract-instance ,(lambda (from) (deploy-contract-instance counter-abi #:from from)))
      (bound-send ,(lambda (from) (token-binding-transfer token anvil-bob 10 #:from from)))
      (bound-deploy ,(lambda (from) (counter-deploy #:from from)))
      (instance-send
       ,(lambda (from) (contract-send (contract-instance token-abi token) 'transfer anvil-bob 10
                                      #:from from)))))

  (define (sent-fields object)
    (list (hash-ref object 'to #f)
          (hash-ref object 'data)
          (hex-string->number (hash-ref object 'value "0x0"))
          (hex-string->number (hash-ref object 'gas))))

  (define (raw-fields tx)
    (list (transaction-to tx) (bytes->hex-string (transaction-data tx)) (transaction-value tx)
          (transaction-gas-limit tx)))

  (test-case "K3: every entry point sends the same transaction from an address and a signer"
    (for ([entry (in-list entry-points)])
      (match-define (list name send) entry)
      (define-values (by-address address-calls) (run-mock node (lambda () (send anvil-alice))))
      (define-values (by-signer signer-calls) (run-mock node (lambda () (send signer0))))
      (check-false (exn? (first by-address)) (format "~a ~a" name by-address))
      (check-false (exn? (first by-signer)) (format "~a ~a" name by-signer))
      (check-equal? (call-param signer-calls "eth_estimateGas")
                    (call-param address-calls "eth_estimateGas")
                    (symbol->string name))
      (check-equal? (map raw-fields (raw-transactions signer-calls))
                    (list (sent-fields (call-param address-calls "eth_sendTransaction")))
                    (symbol->string name))))

  (test-case "K4, K5: #:gas is the gas limit with no estimate, and #:value reaches the raw"
    (define-values (_given given-calls)
      (run-mock node (lambda () (contract-send token "transfer(address,uint256)" anvil-bob 10
                                               #:from signer0 #:gas 50000))))
    (check-false (member "eth_estimateGas" (method-names given-calls)))
    (check-equal? (map transaction-gas-limit (raw-transactions given-calls)) '(50000))
    (for ([send (list (lambda () (token-binding-deposit token "memo" #:value 7 #:from signer0))
                      (lambda () (deploy-contract-instance payable-abi #:value 7 #:from signer0)))])
      (define-values (result calls) (run-mock node send))
      (check-equal? (map transaction-value (raw-transactions calls)) '(7) (format "~a" result))))

  (define width-rows
    `((#:gas ,(expt 2 64) "#:gas is more than a transaction's 64 bits hold")
      (#:value ,(expt 2 256) "#:value is more than a transaction's 256 bits hold")))

  (test-case "K14: a send refuses what a transaction cannot hold, before any request"
    (for* ([from (list signer0 anvil-alice)]
           [row (in-list width-rows)])
      (match-define (list kw v text) row)
      (define name (format "~a ~a" from text))
      (define-values (e calls)
        (run-mock node (lambda ()
                         (call-with-fields contract-send `(#:from ,from ,kw ,v)
                                           token "transfer(address,uint256)" anvil-bob 1))))
      (check-true (and (exn:fail:contract? e) (not (exn:fail:contract:blame? e))) name)
      (check-equal? (and (exn? e) (exn-message e)) (string-append "contract-send: " text) name)
      (check-equal? calls '() name)))

  (test-case "K6: calls and estimates from a signer use its address and sign nothing"
    (for ([call (list (lambda (from) (contract-call token "balanceOf(address)(uint256)" anvil-bob
                                                    #:from from))
                      (lambda (from) (contract-estimate-gas token "transfer(address,uint256)"
                                                            anvil-bob 1 #:from from))
                      (lambda (from) (token-binding-balance-of token anvil-bob #:from from)))])
      (define signer (watched))
      (define-values (result calls)
        (run-mock (filling #:replay (hex-word 5)) (lambda () (call signer))))
      (check-false (exn? (first result)) (format "~a" result))
      (match-define (list (list _ object _ ...)) calls)
      (check-equal? (hash-ref object 'from) anvil-alice)
      (check-equal? (remove-duplicates (watched-calls signer)) '(address))))

  (define precedence-rows
    '((- - S S) (A - S A) (S - A S) (- S A S) (- A S A) (A S - A)))

  (define (sender-of symbol)
    (match symbol
      ['- #f]
      ['S signer0]
      ['A anvil-carol]))

  (test-case "K7: #:from, then the instance's sender, then current-account"
    (for ([row (in-list precedence-rows)])
      (match-define (list (app sender-of from) (app sender-of instance-from)
                          (app sender-of account) expected)
        row)
      (define instance (contract-instance token-abi token #:from instance-from))
      (define-values (_result calls)
        (parameterize ([current-account account])
          (run-mock node (lambda ()
                           (call-with-fields contract-send (if from `(#:from ,from) '())
                                             instance 'transfer anvil-bob 1)))))
      (check-equal? (if (member "eth_sendRawTransaction" (method-names calls)) 'S 'A)
                    expected
                    (format "~s" row))))

  (test-case "K8: reverts on the signer path are decoded with the contract's errors"
    (for ([send (list (lambda () (contract-send (contract-instance token-abi token) 'transfer
                                                anvil-bob 10 #:from signer0))
                      (lambda () (token-binding-transfer token anvil-bob 10 #:from signer0)))])
      (define-values (e calls)
        (run-mock (filling #:estimate (reverting "eth_estimateGas")) send))
      (check-pred exn:fail:web3:rpc:revert? e)
      (check-equal? (abi-revert-arguments (exn:fail:web3:rpc:revert-revert e)) '(0 5000))
      (check-false (member "eth_sendRawTransaction" (method-names calls)))
      (define-values (failure failure-calls)
        (run-mock (filling #:receipt failed #:replay (reverting "eth_call")) send))
      (check-pred exn:fail:web3:transaction? failure)
      (check-equal? (abi-revert-arguments (exn:fail:web3:transaction-revert failure)) '(0 5000))
      (check-equal? (last failure-calls)
                    `("eth_call" ,(hash-set transfer-tx 'gas "0x5208") "0x5"))
      (define-values (refused _refused-calls)
        (run-mock (filling #:raw (reverting "eth_sendRawTransaction")) send))
      (check-pred exn:fail:web3:rpc:revert? refused)
      (when (exn:fail:web3:rpc:revert? refused)
        (check-equal? (abi-revert-arguments (exn:fail:web3:rpc:revert-revert refused)) '(0 5000)))))

  (test-case "K15: a signer whose address changes after the fill sends nothing"
    (for ([entry (in-list entry-points)])
      (match-define (list name send) entry)
      (define-values (e calls) (run-mock node (lambda () (send (flipper)))))
      (check-pred exn:fail:web3:signer? e (symbol->string name))
      (when (exn:fail:web3:signer? e)
        (check-eq? (exn:fail:web3:signer-reason e) 'wrong-signer)
        (check-regexp-match
         (regexp-quote (format (string-append "wrong signer: the transaction's sender is ~a, "
                                              "not the address it was filled for ~a")
                               anvil-bob anvil-alice))
         (exn-message e)))
      (check-false (member "eth_sendRawTransaction" (method-names calls)) (symbol->string name))))

  (test-case "K9, K10: no sender, a lying signer and malformed counts name the entry point"
    (define-values (none none-calls)
      (parameterize ([current-account #f])
        (run-mock node (lambda () (contract-send token "transfer(address,uint256)" anvil-bob 1)))))
    (check-equal? (exn-message none) "contract-send: no sender; pass #:from or set current-account")
    (check-equal? none-calls '())
    (for ([row (list (list "contract-send"
                           (lambda (from) (contract-send token "transfer(address,uint256)"
                                                         anvil-bob 1 #:from from)))
                     (list "token-binding-transfer"
                           (lambda (from) (token-binding-transfer token anvil-bob 1 #:from from)))
                     (list "deploy-contract" (lambda (from) (deploy-contract #"\x60" #:from from))))])
      (match-define (list who send) row)
      (define-values (e calls) (run-mock node (lambda () (send (liar marker key0)))))
      (check-pred exn:fail:web3:signer? e who)
      (check-regexp-match (format "^~a: wrong signer: " who) (exn-message e))
      (check-no-leak (exn-message e))
      (check-false (member "eth_sendRawTransaction" (method-names calls)) who))
    (define-values (malformed _calls)
      (run-mock (filling #:count "0x")
                (lambda () (contract-send token "transfer(address,uint256)" anvil-bob 1
                                          #:from signer0))))
    (check-equal? (exn-message malformed)
                  "contract-send: malformed transaction count from the node: \"0x\""))

  (define marker-key (integer->secret-key marker-integer))

  (define from-takers
    (list (lambda (from) (contract-call token "balanceOf(address)(uint256)" anvil-bob #:from from))
          (lambda (from) (contract-send token "transfer(address,uint256)" anvil-bob 1 #:from from))
          (lambda (from) (contract-estimate-gas token "transfer(address,uint256)" anvil-bob 1
                                                #:from from))
          (lambda (from) (deploy-contract #"\x60" #:from from))
          (lambda (from) (deploy-contract/receipt #"\x60" #:from from))
          (lambda (from) (deploy-contract-instance counter-abi #:from from))
          (lambda (from) (contract-instance token-abi token #:from from))
          (lambda (from) (current-account from))
          (lambda (from) (token-binding-transfer token anvil-bob 1 #:from from))
          (lambda (from) (counter-deploy #:from from))))

  (define misapplied
    `((contract-call ,contract-call (,token "balanceOf(address)(uint256)" ,anvil-bob) ())
      (contract-send ,contract-send (,token "transfer(address,uint256)" ,anvil-bob 1) ())
      (contract-estimate-gas ,contract-estimate-gas (,token "transfer(address,uint256)" ,anvil-bob 1)
                             ())
      (deploy-contract ,deploy-contract (#"\x60") ())
      (deploy-contract/receipt ,deploy-contract/receipt (#"\x60") ())
      (deploy-contract-instance ,deploy-contract-instance (,counter-abi) ())
      (contract-instance ,contract-instance (,token-abi ,token) (,token-abi))
      (token-binding-transfer ,token-binding-transfer (,token ,anvil-bob 1) (,token))
      (counter-deploy ,counter-deploy () (1 2 3))))

  (define (misapplications row from)
    (match-define (list _ procedure positional wrong-arity) row)
    (list (lambda () (keyword-apply procedure '(#:bogus #:from) (list 1 from) positional))
          (lambda () (keyword-apply procedure '(#:from) (list from) wrong-arity))))

  (test-case "K11: a sender of another kind blames the caller and shows no key"
    (parameterize ([current-provider (mock-provider node)])
      (for* ([take (in-list from-takers)]
             [bad (list marker-hex marker-key (make-bytes 20 1) 's)])
        (define e (with-handlers ([exn:fail? values]) (take bad)))
        (check-pred exn:fail:contract:blame? e)
        (when (exn? e)
          (check-regexp-match #rx"expected: #f, an address or a signer" (exn-message e))
          (check-no-leak (exn-message e))))
      (for* ([row (in-list misapplied)]
             [call (in-list (misapplications row (local-signer marker-key)))])
        (define e (with-handlers ([exn:fail? values]) (call)))
        (check-pred exn:fail? e (format "~a" (first row)))
        (when (exn? e)
          (check-no-leak (exn-message e) (format "~a" (first row)))))))

  (test-case "K12, K13: bound keywords are unchanged, and an instance keeps its signer"
    (check-equal? all-keywords '(#:from #:value #:block #:gas #:errors #:provider))
    (check-eq? (contract-instance-from (contract-instance token-abi token #:from signer0))
               signer0)))
