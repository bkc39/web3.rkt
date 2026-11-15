#lang racket/base

;; contract-call, contract-send and the deploys against a mock provider:
;; the exact traffic, results and failures.

(module+ test
  (require racket/list
           racket/match
           rackunit
           web3
           (submod web3/contract internal)
           "abi-gen.rkt"
           "modules.rkt"
           "util.rkt")

  (define-namespace-anchor anchor)

  (define token "0x5FbDB2315678afecb367f032d93F642f64180aa3")
  (define tx-hash (string-append "0x" (make-string 64 #\a)))
  (define payload (encode-call insufficient-balance 0 5000))
  (define insufficient-revert
    (abi-revert (string->abi-signature insufficient-balance) '(0 5000) payload))
  (define mined (hasheq 'status "0x1" 'blockNumber "0x2" 'transactionHash tx-hash 'logs '()))
  (define transfer-data (bytes->hex-string (encode-call "transfer(address,uint256)" anvil-bob 10)))
  (define transfer-tx (hasheq 'from anvil-alice 'to token 'data transfer-data))

  (define (hex-word n)
    (bytes->hex-string (word n)))

  (define ((reverting method [data (bytes->hex-string payload)]) _params)
    (raise-rpc-error method 3 "execution reverted: custom error" data))

  ;; What thunk returned, as a list, or what it raised; and the calls it made.
  (define (run-mock handler thunk)
    (define node (mock-provider handler))
    (define result
      (parameterize ([current-provider node])
        (with-handlers ([(lambda (_) #t) values])
          (call-with-values thunk list))))
    (values result (mock-provider-calls node)))

  (define (sending #:estimate [estimate "0x5208"]
                   #:receipt [receipt mined]
                   #:replay [replay "0x"])
    (hash "eth_estimateGas" estimate
          "eth_sendTransaction" tx-hash
          "eth_getTransactionReceipt" receipt
          "eth_call" replay))

  (define failed (hasheq 'status "0x0" 'blockNumber "0x5" 'transactionHash tx-hash 'logs '()))

  (test-case "a call is one eth_call of the call data, from the sender, at the block"
    (define-values (result calls)
      (run-mock (hash "eth_call" (hex-word 1000))
                (lambda ()
                  (contract-call token "balanceOf(address)(uint256)" anvil-alice
                                 #:from anvil-alice #:block 7))))
    (check-equal? result '(1000))
    (check-equal?
     calls
     (list (list "eth_call"
                 (hasheq 'to token
                         'from anvil-alice
                         'data "0x70a08231000000000000000000000000f39fd6e51aad88f6f4ce6ab8827279cfffb92266")
                 "0x7")))
    (define-values (_paid paid-calls)
      (run-mock (hash "eth_call" (hex-word 1))
                (lambda ()
                  (contract-call token "balanceOf(address)(uint256)" anvil-alice
                                 #:value 1000 #:gas 50000))))
    (match-define (list (list _ paid-tx "latest")) paid-calls)
    (check-equal? (hash-ref paid-tx 'value) "0x3e8")
    (check-equal? (hash-ref paid-tx 'gas) "0xc350")
    (check-false (hash-has-key? paid-tx 'from)))

  (test-case "a call returns one value per output"
    (define info (abi-encode '(string string uint8 uint256) '("Token" "TOK" 18 1000000)))
    (define-values (result _calls)
      (run-mock (hash "eth_call" (bytes->hex-string info))
                (lambda () (contract-call token "info()(string,string,uint8,uint256)"))))
    (check-equal? result '("Token" "TOK" 18 1000000))
    (define-values (none _none-calls)
      (run-mock (hash "eth_call" "0xdeadbeef")
                (lambda () (contract-call token "mint(address,uint256)()" anvil-bob 1))))
    (check-equal? none '())
    (define-values (one _one-calls)
      (run-mock (hash "eth_call" (hex-word 5))
                (lambda () (contract-call token "balanceOf(address)(uint256)" anvil-bob))))
    (check-equal? one '(5)))

  (test-case "a call returns the decoded outputs and sends encode-call's data"
    (define prng (seeded 24 40 4 1 2 3))
    (for ([_ (in-range 100)])
      (define inputs (random-abi-types prng))
      (define outputs (random-abi-types prng))
      (define sig (abi-signature "f" inputs outputs))
      (define args (for/list ([t (in-list inputs)]) (random-abi-value prng t)))
      (define vs (for/list ([t (in-list outputs)]) (random-abi-value prng t)))
      (define name (abi-signature->string sig #:outputs? #t))
      (define-values (result calls)
        (run-mock (hash "eth_call" (bytes->hex-string (abi-encode outputs vs)))
                  (lambda () (apply contract-call token sig args))))
      (check-equal? result (checksummed outputs vs) name)
      (match-define (list (list "eth_call" (hash-table ['data data]) "latest")) calls)
      (check-equal? data (bytes->hex-string (apply encode-call sig args)) name)
      (check-equal? (decode-call sig (hex-string->bytes data)) (checksummed inputs args) name)))

  (test-case "a send estimates, sends with the estimate, and returns the receipt"
    (define-values (result calls)
      (run-mock (sending)
                (lambda () (contract-send token "transfer(address,uint256)" anvil-bob 10
                                          #:from anvil-alice))))
    (check-eq? (first result) mined)
    (define with-gas (hash-set transfer-tx 'gas "0x5208"))
    (check-equal? calls
                  (list (list "eth_estimateGas" transfer-tx)
                        (list "eth_sendTransaction" with-gas)
                        (list "eth_getTransactionReceipt" tx-hash)))
    (define-values (_given given-calls)
      (run-mock (sending)
                (lambda () (contract-send token "transfer(address,uint256)" anvil-bob 10
                                          #:from anvil-alice #:gas 21000))))
    (check-equal? given-calls
                  (list (list "eth_sendTransaction" with-gas)
                        (list "eth_getTransactionReceipt" tx-hash)))
    (define-values (_outputs output-calls)
      (run-mock (sending)
                (lambda () (contract-send token "transfer(address,uint256)(bool)" anvil-bob 10
                                          #:from anvil-alice))))
    (check-equal? output-calls calls))

  (test-case "the sender is #:from, else current-account; sending with neither raises"
    (define (senders thunk)
      (define-values (_result calls) (run-mock (sending) thunk))
      (for/list ([call (in-list calls)]
                 #:when (hash? (second call)))
        (hash-ref (second call) 'from)))
    (parameterize ([current-account anvil-bob])
      (check-equal? (senders (lambda () (contract-send token "transfer(address,uint256)" anvil-carol 1)))
                    (list anvil-bob anvil-bob))
      (check-equal? (senders (lambda () (contract-send token "transfer(address,uint256)" anvil-carol 1
                                                       #:from anvil-carol)))
                    (list anvil-carol anvil-carol)))
    (for ([row (in-list '((contract-send (contract-send token "transfer(address,uint256)" anvil-bob 1))
                          (deploy-contract (deploy-contract #"\x60\x80"))
                          (deploy-contract/receipt (deploy-contract/receipt #"\x60\x80"))))])
      (match-define (list (app symbol->string who) form) row)
      (define-values (e calls)
        (run-mock (sending) (lambda () (eval form (namespace-anchor->namespace anchor)))))
      (check-pred exn:fail:web3? e who)
      (check-false (exn:fail:web3:abi? e) who)
      (check-equal? (exn-message e) (format "~a: no sender; pass #:from or set current-account" who))
      (check-equal? calls '() who)))

  (test-case "a deploy sends the init code and the encoded arguments, with no to"
    (define created (hasheq 'status "0x1" 'blockNumber "0x1" 'contractAddress (string-downcase token)))
    (define (deploy-traffic thunk)
      (run-mock (sending #:receipt created) thunk))
    (define-values (result calls)
      (deploy-traffic (lambda () (deploy-contract "0x6080AB" "constructor(uint256)" 7 #:from anvil-alice))))
    (check-equal? result (list token))
    (check-equal? (second (first calls))
                  (hasheq 'from anvil-alice 'data (string-append "0x6080ab" (substring (hex-word 7) 2))))
    (parameterize ([current-account anvil-alice])
      (define-values (_default default-calls) (deploy-traffic (lambda () (deploy-contract #"\x60\x80"))))
      (define-values (_explicit explicit-calls)
        (deploy-traffic (lambda () (deploy-contract #"\x60\x80" "constructor()"))))
      (define-values (_hex hex-calls) (deploy-traffic (lambda () (deploy-contract "0x6080"))))
      (check-equal? default-calls explicit-calls)
      (check-equal? hex-calls default-calls)
      (define-values (pair _pair-calls) (deploy-traffic (lambda () (deploy-contract/receipt "0x6080"))))
      (check-equal? (first pair) token)
      (check-eq? (second pair) created)))

  (test-case "a call that reverts raises its revert, decoded against #:errors"
    (define-values (e calls)
      (run-mock (hash "eth_call" (reverting "eth_call"))
                (lambda () (contract-call token "transfer(address,uint256)(bool)" anvil-bob 1
                                          #:errors (list insufficient-balance)))))
    (check-pred exn:fail:web3:rpc:revert? e)
    (check-equal? (exn:fail:web3:rpc:revert-revert e) insufficient-revert)
    (check-equal? (exn-message e) "eth_call: reverted with InsufficientBalance(0, 5000)")
    (check-equal? (length calls) 1)
    (define-values (unknown _unknown-calls)
      (run-mock (hash "eth_call" (reverting "eth_call"))
                (lambda () (contract-call token "transfer(address,uint256)(bool)" anvil-bob 1))))
    (check-false (abi-revert-signature (exn:fail:web3:rpc:revert-revert unknown)))
    (for ([failure (list (reverting "eth_call" 42)
                         (lambda (_) (raise-rpc-error "eth_call" -32602 "Invalid params")))])
      (define original (with-handlers ([values values]) (failure '())))
      (define-values (raised _calls)
        (run-mock (hash "eth_call" (lambda (_) (raise original)))
                  (lambda () (contract-call token "balanceOf(address)(uint256)" anvil-bob))))
      (check-eq? raised original)))

  (test-case "a send whose estimate reverts raises the revert and sends nothing"
    (define-values (e calls)
      (run-mock (sending #:estimate (reverting "eth_estimateGas"))
                (lambda () (contract-send token "transfer(address,uint256)" anvil-bob 10
                                          #:from anvil-alice #:errors (list insufficient-balance)))))
    (check-pred exn:fail:web3:rpc:revert? e)
    (check-equal? (exn:fail:web3:rpc-method e) "eth_estimateGas")
    (check-equal? (exn:fail:web3:rpc:revert-revert e) insufficient-revert)
    (check-equal? calls (list (list "eth_estimateGas" transfer-tx))))

  (test-case "a send mined with status 0 raises exn:fail:web3:transaction with the replay's revert"
    (define (mined-failure replay #:errors [errors (list insufficient-balance)])
      (run-mock (sending #:receipt failed #:replay replay)
                (lambda () (contract-send token "transfer(address,uint256)" anvil-bob 10
                                          #:from anvil-alice #:gas 100000 #:errors errors))))
    (define-values (e calls) (mined-failure (reverting "eth_call")))
    (check-pred exn:fail:web3:transaction? e)
    (check-eq? (exn:fail:web3:transaction-receipt e) failed)
    (check-equal? (exn:fail:web3:transaction-revert e) insufficient-revert)
    (check-equal? (exn-message e)
                  (format "contract-send: transaction ~a failed in block 5: reverted with InsufficientBalance(0, 5000)"
                          tx-hash))
    (match-define (list (list "eth_sendTransaction" sent) _ (list "eth_call" replayed "0x5")) calls)
    (check-eq? replayed sent)
    (check-equal? replayed (hash-set transfer-tx 'gas "0x186a0"))
    (define-values (out-of-gas _oog-calls)
      (mined-failure (lambda (_) (raise-rpc-error "eth_call" -32003 "EVM error OutOfGas"))))
    (check-pred exn:fail:web3:transaction? out-of-gas)
    (check-false (exn:fail:web3:transaction-revert out-of-gas))
    (check-regexp-match #rx": EVM error OutOfGas$" (exn-message out-of-gas))
    (define-values (passes _passes-calls) (mined-failure "0x"))
    (check-false (exn:fail:web3:transaction-revert passes))
    (check-regexp-match #rx": replaying it did not fail$" (exn-message passes))
    (for ([row (in-list
                (list (list (lambda (_) (raise (exn:fail:web3 "eth_call: other" (current-continuation-marks))))
                            "eth_call: other")
                      (list (lambda (_) (raise (exn:fail:network "connection reset" (current-continuation-marks))))
                            "connection reset")
                      (list 42 "eth_call: unexpected result 42")))])
      (match-define (list replay message) row)
      (define-values (e _calls) (mined-failure replay))
      (check-pred exn:fail:web3:transaction? e message)
      (check-eq? (exn:fail:web3:transaction-receipt e) failed message)
      (check-false (exn:fail:web3:transaction-revert e) message)
      (check-equal? (exn-message e)
                    (format "contract-send: transaction ~a failed in block 5: replaying it raised: ~a"
                            tx-hash message))))

  (test-case "a failure after the send names the transaction and keeps the error's kind"
    (define (unknown-outcome receipt [thunk (lambda ()
                                              (contract-send token "transfer(address,uint256)" anvil-bob 10
                                                             #:from anvil-alice))])
      (define-values (e calls)
        (parameterize ([receipt-timeout 0.05])
          (run-mock (sending #:receipt receipt) thunk)))
      (check-equal? (cadr (assoc "eth_sendTransaction" calls)) (hash-set transfer-tx 'gas "0x5208"))
      e)
    (define sent (format "^contract-send: sent transaction ~a, outcome unknown: " tx-hash))
    (define timed-out (unknown-outcome 'null))
    (check-pred exn:fail:web3? timed-out)
    (check-regexp-match (regexp (string-append sent (regexp-quote "wait-for-receipt: no receipt for ")))
                        (exn-message timed-out))
    (define rpc-failure
      (unknown-outcome (lambda (_) (raise-rpc-error "eth_getTransactionReceipt" -32000 "header not found"))))
    (check-pred exn:fail:web3:rpc? rpc-failure)
    (check-false (exn:fail:web3:rpc:revert? rpc-failure))
    (check-equal? (list (exn:fail:web3:rpc-method rpc-failure)
                        (exn:fail:web3:rpc-code rpc-failure)
                        (exn:fail:web3:rpc-rpc-message rpc-failure)
                        (exn:fail:web3:rpc-data rpc-failure))
                  '("eth_getTransactionReceipt" -32000 "header not found" #f))
    (check-regexp-match (regexp (string-append sent "eth_getTransactionReceipt: header not found$"))
                        (exn-message rpc-failure))
    (define revert-failure
      (unknown-outcome (lambda (_)
                         (raise (exn:fail:web3:rpc:revert "eth_getTransactionReceipt: reverted"
                                                          (current-continuation-marks)
                                                          "eth_getTransactionReceipt" 3 "reverted"
                                                          (bytes->hex-string payload)
                                                          insufficient-revert)))))
    (check-pred exn:fail:web3:rpc:revert? revert-failure)
    (check-equal? (exn:fail:web3:rpc:revert-revert revert-failure) insufficient-revert)
    (check-equal? (exn:fail:web3:rpc-data revert-failure) (bytes->hex-string payload))
    (define network-failure
      (unknown-outcome (lambda (_) (raise (exn:fail:network "connection reset" (current-continuation-marks))))))
    (check-pred exn:fail:web3? network-failure)
    (check-regexp-match (regexp (string-append sent "connection reset$")) (exn-message network-failure))
    (define-values (deploy-timeout _deploy-calls)
      (parameterize ([receipt-timeout 0.05])
        (run-mock (sending #:receipt 'null) (lambda () (deploy-contract #"\x60\x80" #:from anvil-alice)))))
    (check-regexp-match (regexp (format "^deploy-contract: sent transaction ~a, outcome unknown: " tx-hash))
                        (exn-message deploy-timeout))
    (define-values (bound-timeout _bound-calls)
      (parameterize ([receipt-timeout 0.05])
        (run-mock (sending #:receipt 'null)
                  (lambda () (contract-send/who 'token-transfer token "transfer(address,uint256)" anvil-bob 1
                                                #:from anvil-alice)))))
    (check-regexp-match (regexp (format "^token-transfer: sent transaction ~a, outcome unknown: " tx-hash))
                        (exn-message bound-timeout)))

  (test-case "a malformed gas estimate raises and sends nothing"
    (for ([estimate (in-list '("0x" "21000" "0x0" "0x00" 21000))])
      (define-values (e calls)
        (run-mock (sending #:estimate estimate)
                  (lambda () (contract-send token "transfer(address,uint256)" anvil-bob 10
                                            #:from anvil-alice))))
      (define name (format "~s" estimate))
      (check-pred exn:fail:web3? e name)
      (check-false (exn:fail:web3:rpc? e) name)
      (check-regexp-match #rx"^contract-send: (malformed gas estimate|the node estimated 0 gas)" (exn-message e)
                          name)
      (check-equal? calls (list (list "eth_estimateGas" transfer-tx)) name)))

  (test-case "a deploy mined with status 0 raises even when the receipt has an address"
    (define receipt
      (hasheq 'status "0x0" 'blockNumber "0x3" 'contractAddress (string-downcase token)))
    (define-values (e calls)
      (run-mock (sending #:receipt receipt #:replay (reverting "eth_call"))
                (lambda () (deploy-contract #"\x60\x80" #:from anvil-alice #:gas 100000))))
    (check-pred exn:fail:web3:transaction? e)
    (match-define (list "eth_call" replayed "0x3") (last calls))
    (check-false (hash-has-key? replayed 'to)))

  (test-case "malformed node data raises exn:fail:web3 under the entry point"
    (define (check-malformed handler thunk who [rx (format "^~a: malformed" who)])
      (define-values (e _calls) (run-mock handler thunk))
      (check-pred exn:fail:web3? e (format "~s" handler))
      (check-false (exn:fail:contract? e))
      (check-false (exn:fail:web3:abi? e))
      (when (exn? e)
        (check-regexp-match rx (exn-message e))))
    (define (call) (contract-call token "balanceOf(address)(uint256)" anvil-bob))
    (define (send) (contract-send token "transfer(address,uint256)" anvil-bob 1 #:from anvil-alice))
    (define (deploy) (deploy-contract #"\x60\x80" #:from anvil-alice))
    (check-malformed (hash "eth_call" "0x123") call 'contract-call)
    (check-malformed (hash "eth_call" "0xzz") call 'contract-call #rx"^eth_call: unexpected result")
    (for ([receipt (list (hash-set mined 'status "0x2") (hash-remove mined 'status) (hash-set mined 'status 1))])
      (check-malformed (sending #:receipt receipt) send 'contract-send))
    (for ([address (list 'null "0x1234")])
      (check-malformed (sending #:receipt (hasheq 'status "0x1" 'contractAddress address))
                       deploy 'deploy-contract
                       (format "^deploy-contract: malformed contract address of transaction ~a from the node"
                               tx-hash)))
    (check-malformed (sending #:receipt (hash-remove mined 'status)) send 'contract-send
                     (format "^contract-send: malformed receipt status of transaction ~a from the node"
                             tx-hash))
    (for ([receipt (list (hash-set failed 'blockNumber "zz") (hash-remove failed 'blockNumber))])
      (check-malformed (sending #:receipt receipt) send 'contract-send
                       (format "^contract-send: malformed block number of transaction ~a from the node"
                               tx-hash))))

  (test-case "receipt-succeeded? reads status 0x1 and 0x0 and raises on anything else"
    (check-true (receipt-succeeded? mined))
    (check-false (receipt-succeeded? failed))
    (for ([receipt (list (hash-set mined 'status "0x2") (hash-remove mined 'status) (hash-set mined 'status 1))])
      (check-exn (lambda (e)
                   (and (exn:fail:web3? e)
                        (regexp-match? #rx"^receipt-succeeded[?]: malformed receipt status from the node"
                                       (exn-message e))))
                 (lambda () (receipt-succeeded? receipt)))))

  (test-case "return data too short for the outputs raises a decode error under contract-call"
    (define-values (e _calls)
      (run-mock (hash "eth_call" "0x")
                (lambda () (contract-call token "balanceOf(address)(uint256)" anvil-bob))))
    (check-pred exn:fail:web3:abi:decode? e)
    (check-equal? (exn:fail:web3:abi:decode-reason e) 'short)
    (check-regexp-match #rx"^contract-call: data too short" (exn-message e)))

  (test-case "ABI errors raise under the entry point before any request"
    (define ns (namespace-anchor->namespace anchor))
    (for ([row (in-list
                '(((contract-call token "balanceOf(address)" anvil-alice) type "has no outputs")
                  ((contract-call token "balanceOf(address)(uint256)" "0x12") value (0))
                  ((contract-call token "balanceOf(address)(uint256)") value ())
                  ((contract-call token "balanceOf(address)(uint256)" anvil-alice #:errors '("Foo("))
                   type "")
                  ((contract-call token "balanceOf(address)(uint256)" anvil-alice
                                  #:errors '("burn(uint256)" "collate_propagate_storage(bytes16)"))
                   type "selector collision")
                  ((deploy-contract #"" "init(uint256)" 1) type "a constructor signature")
                  ((deploy-contract #"" "constructor(uint256)(uint256)" 1) type "a constructor signature")
                  ((deploy-contract #"" "constructor(uint8)" 256) value (0))))])
      (match-define (list form kind detail) row)
      (define who (symbol->string (first form)))
      (define-values (e calls)
        (run-mock (sending) (lambda () (eval form ns))))
      (define name (format "~s" form))
      (check-equal? calls '() name)
      (check-regexp-match (regexp (string-append "^" (regexp-quote who) ": ")) (exn-message e) name)
      (match kind
        ['type
         (check-pred exn:fail:web3:abi:type? e name)
         (check-regexp-match (regexp-quote detail) (exn-message e) name)]
        ['value
         (check-pred exn:fail:web3:abi:value? e name)
         (check-equal? (exn:fail:web3:abi:value-path e) detail name)])))

  (test-case "arguments of the wrong shape are contract violations blaming the caller"
    (check-blames-here (contract-call 42 "f()()"))
    (check-blames-here (contract-call token 'f))
    (check-blames-here (contract-call token "f()()" #:value -1))
    (check-blames-here (contract-call token "f()()" #:gas 0))
    (check-blames-here (contract-call token "f()()" #:block "newest"))
    (check-blames-here (contract-call token "f()()" #:errors "Foo()"))
    (check-blames-here (contract-call token "f()()" #:from "alice"))
    (check-blames-here (contract-send token "f()" #:gas 0))
    (check-blames-here (deploy-contract "602a"))
    (check-blames-here (deploy-contract "0x6"))
    (check-blames-here (deploy-contract/receipt #"" 'constructor))
    (check-blames-here (create2-address token (make-bytes 31) #""))
    (check-blames-here (create2-address token (expt 2 256) #""))
    (check-blames-here (create-address token -1))
    (check-blames-here (create-address token (- (expt 2 64) 1)))
    (check-blames-here (current-account "0x12")))

  (test-case "passing every keyword at its default sends what passing none does"
    (define created (hasheq 'status "0x1" 'blockNumber "0x1" 'contractAddress (string-downcase token)))
    (define handler (hash-set (sending #:receipt created) "eth_call" (hex-word 1)))
    (define (traffic thunk)
      (define-values (result calls)
        (parameterize ([current-account anvil-alice]) (run-mock handler thunk)))
      (list result calls))
    (check-equal? (traffic (lambda () (contract-call token "balanceOf(address)(uint256)" anvil-bob)))
                  (traffic (lambda ()
                             (contract-call token "balanceOf(address)(uint256)" anvil-bob
                                            #:from (current-account) #:value 0 #:gas #f #:block "latest"
                                            #:errors '() #:provider (current-provider)))))
    (check-equal? (traffic (lambda () (contract-send token "transfer(address,uint256)" anvil-bob 1)))
                  (traffic (lambda ()
                             (contract-send token "transfer(address,uint256)" anvil-bob 1
                                            #:from (current-account) #:value 0 #:gas #f
                                            #:errors '() #:provider (current-provider)))))
    (for ([deploy (list deploy-contract deploy-contract/receipt)])
      (check-equal? (traffic (lambda () (deploy #"\x60\x80")))
                    (traffic (lambda ()
                               (deploy #"\x60\x80" "constructor()"
                                       #:from (current-account) #:value 0 #:gas #f
                                       #:errors '() #:provider (current-provider)))))))

  (test-case "the manual's examples give what it shows"
    (define node
      (mock-provider (hash "eth_call" (bytes->hex-string (abi-encode '(string uint8) '("Token" 18))))))
    (check-equal? (call-with-values (lambda () (contract-call token "meta()(string,uint8)" #:provider node))
                                    list)
                  '("Token" 18))
    (define refusing
      (mock-provider (lambda (method _params)
                       (raise-rpc-error method 3 "execution reverted"
                                        (bytes->hex-string (encode-call insufficient-balance 0 5))))))
    (check-exn #rx"^eth_call: reverted with InsufficientBalance[(]0, 5[)]$"
               (lambda ()
                 (contract-call token "transfer(address,uint256)(bool)" token 5
                                #:errors (list insufficient-balance) #:provider refusing)))
    (check-equal? (create-address "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266" 0) token)
    (check-equal? (create2-address "0x0000000000000000000000000000000000000000" 0 #"\0")
                  "0x4D1A2e2bB4F88F0250f26Ffff098B0b30B26BF38"))

  (test-case "exn:fail:web3:transaction is an exn:fail:web3, not an RPC error"
    (define e (exn:fail:web3:transaction "m" (current-continuation-marks) mined #f))
    (check-true (exn:fail:web3? e))
    (check-false (exn:fail:web3:rpc? e)))

  (test-case "the internal entry points raise under the caller's name"
    (define-values (no-outputs _calls)
      (run-mock (sending)
                (lambda () (contract-call/who 'token-balance-of token "balanceOf(address)" anvil-bob))))
    (check-regexp-match #rx"^token-balance-of: signature has no outputs" (exn-message no-outputs))
    (define-values (balance _balance-calls)
      (run-mock (hash "eth_call" (hex-word 9))
                (lambda () (contract-call/who 'token-balance-of token "balanceOf(address)(uint256)" anvil-bob))))
    (check-equal? balance '(9))
    (define-values (no-sender _send-calls)
      (run-mock (sending)
                (lambda () (contract-send/who 'token-transfer token "transfer(address,uint256)" anvil-bob 1))))
    (check-equal? (exn-message no-sender) "token-transfer: no sender; pass #:from or set current-account")
    (define-values (sent _sent-calls)
      (run-mock (sending)
                (lambda () (contract-send/who 'token-transfer token "transfer(address,uint256)" anvil-bob 1
                                              #:from anvil-alice))))
    (check-eq? (first sent) mined)
    (define-values (refused _deploy-calls)
      (run-mock (sending)
                (lambda () (deploy-contract/who 'deploy-contract-instance #"" "constructor(uint8)" 256
                                                #:from anvil-alice))))
    (check-regexp-match #rx"^deploy-contract-instance: " (exn-message refused))
    (define-values (mined-failure _failure-calls)
      (run-mock (sending #:receipt failed #:replay (reverting "eth_call"))
                (lambda () (deploy-contract/who 'deploy-contract-instance #"\x60\x80"
                                                #:from anvil-alice #:gas 100000))))
    (check-regexp-match (regexp (string-append "^deploy-contract-instance: transaction " tx-hash))
                        (exn-message mined-failure)))

  (test-case "web3/private/node needs no JSON or transport modules"
    (define node (module-path-index-resolve (module-path-index-join 'web3/private/node #f) #t))
    (check-equal? (sort (filter values (map module-name (direct-imports node))) string<?)
                  '("racket/base" "web3/address" "web3/private/exn" "web3/private/hex"))
    (define names (filter values (map module-name (import-closure 'web3/private/node))))
    (check-equal? (filter (lambda (n) (or (member (collection n) '("json" "net"))
                                          (member n '("web3/rpc" "web3/eth"))))
                          names)
                  '())))
