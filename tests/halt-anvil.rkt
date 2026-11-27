#lang racket/base

;; Exceptional halts produced on anvil by hand-assembled code, read back as
;; symbols.  Skipped when anvil is not available.

(module+ test
  (require racket/list
           racket/match
           rackunit
           web3
           (only-in "helpers/accounts.rkt" anvil-keys)
           (only-in "util.rkt" anvil-alice anvil-carol raised))

  (define runtime-rows
    '((out-of-gas "5b600056" 50000 "EVM error OutOfGas" "out of gas")
      (out-of-gas "6000630100000052" 100000 "EVM error MemoryOOG" "out of gas: out of memory")
      (out-of-gas "600064ffffffffff52" 100000 "EVM error MemoryLimitOOG"
                  "out of gas: reach memory limit")
      (out-of-gas "60007fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff52"
                  100000 "EVM error InvalidOperandOOG" "out of gas: invalid operand")
      (invalid-opcode "fe" 100000 "EVM error InvalidFEOpcode" "invalid opcode: INVALID")
      (undefined-opcode "0c" 100000 "EVM error OpcodeNotFound" "invalid opcode")
      (stack-underflow "50" 100000 "EVM error StackUnderflow" "stack underflow")
      (stack-overflow "5b6000600056" 100000 "EVM error StackOverflow" "Out of stack")
      (invalid-jump "600156" 100000 "EVM error InvalidJump" "invalid jump destination")
      (return-data-out-of-bounds "6001600060003e" 100000 "EVM error OutOfOffset"
                                 "return data out of bounds")))

  (define creation-rows
    '((code-size "6160016000f3" 1000000 "EVM error CreateContractSizeLimit"
                 "max code size exceeded")
      (invalid-code "60ef60005360016000f3" 1000000 "EVM error CreateContractStartingWithEF"
                    "invalid code: must not begin with 0xef")))

  (define next-address
    (let ([counter (box #xc0de00)])
      (lambda ()
        (set-box! counter (add1 (unbox counter)))
        (integer->address (unbox counter)))))

  (define (code-bytes code)
    (hex-string->bytes (string-append "0x" code)))

  (define (installed code)
    (define address (next-address))
    (anvil-set-code address (code-bytes code))
    address)

  (define (check-rpc-halt e halt words)
    (check-pred exn:fail:web3:rpc? e words)
    (check-false (exn:fail:web3:rpc:revert? e) words)
    (check-equal? (exn:fail:web3:rpc-rpc-message e) words)
    (check-eq? (exn->halt e) halt words))

  (define (check-mined-halt e halt words)
    (check-pred exn:fail:web3:transaction? e words)
    (check-equal? (receipt-status (exn:fail:web3:transaction-receipt e)) 0 words)
    (check-false (exn:fail:web3:transaction-revert e) words)
    (check-eq? (exn:fail:web3:transaction-halt e) halt words)
    (check-eq? (exn->halt e) halt words))

  (define (tracer-error e)
    (define tx-hash (receipt-transaction-hash (exn:fail:web3:transaction-receipt e)))
    (hash-ref (debug-trace-transaction tx-hash (hasheq 'tracer "callTracer")) 'error #f))

  (define (runtime-cases)
    (test-case "A1: call-transaction raises the node's words, and exn->halt names the halt"
      (for ([row (in-list runtime-rows)])
        (match-define (list halt code _gas words _tracer) row)
        (check-rpc-halt (raised (lambda () (call-transaction anvil-alice (installed code))))
                        halt words)))

    (test-case "A2: a send mined with status 0 carries the replay's halt and no revert"
      (for ([row (in-list runtime-rows)])
        (match-define (list halt code gas words _tracer) row)
        (define e
          (raised (lambda () (send-transaction/receipt anvil-alice (installed code)
                                                       #:gas-limit gas))))
        (check-mined-halt e halt words)
        (check-regexp-match (regexp (string-append ": " words "$")) (exn-message e))))

    (test-case "A3: the call tracer's error for the mined transaction names the same halt"
      (for ([row (in-list runtime-rows)])
        (match-define (list halt code gas _words tracer) row)
        (define e
          (raised (lambda () (send-transaction/receipt anvil-alice (installed code)
                                                       #:gas-limit gas))))
        (check-equal? (tracer-error e) tracer)
        (check-eq? (message->halt tracer) halt tracer)))

    (test-case "A4: a contract call and an estimated contract send raise the halt"
      (for ([row (in-list runtime-rows)])
        (match-define (list halt code _gas words _tracer) row)
        (define address (installed code))
        (check-rpc-halt (raised (lambda () (contract-call address "f()(uint256)" #:from anvil-alice)))
                        halt words)
        (define estimated (raised (lambda () (contract-send address "f()" #:from anvil-alice))))
        (check-equal? (exn:fail:web3:rpc-method estimated) "eth_estimateGas" words)
        (check-eq? (exn->halt estimated) halt words)))

    (test-case "A5: anvil estimates a send that runs out of gas in words of its own"
      (define e (raised (lambda () (send-transaction anvil-alice (installed "5b600056")))))
      (check-equal? (exn:fail:web3:rpc-method e) "eth_estimateGas")
      (check-regexp-match #rx"^Out of gas: gas required exceeds allowance: [0-9]+$"
                          (exn:fail:web3:rpc-rpc-message e))
      (check-eq? (exn->halt e) 'out-of-gas))

    (test-case "A6: a send from a local signer carries its halt as one the node signs does"
      (define signer (local-signer (hex-string->secret-key (third anvil-keys))))
      (define e
        (raised (lambda () (send-transaction/receipt signer (installed "fe") #:gas-limit 100000))))
      (check-mined-halt e 'invalid-opcode "EVM error InvalidFEOpcode")))

  (define (creation-cases)
    (test-case "A7: a creation that halts raises its halt from a call, a send and a deploy"
      (for ([row (in-list creation-rows)])
        (match-define (list halt code gas words tracer) row)
        (define init (code-bytes code))
        (check-rpc-halt (raised (lambda () (call-transaction anvil-alice #f #:data init)))
                        halt words)
        (define mined
          (raised (lambda () (send-transaction/receipt anvil-alice #f #:data init #:gas-limit gas))))
        (check-mined-halt mined halt words)
        (check-equal? (tracer-error mined) tracer)
        (check-eq? (message->halt tracer) halt tracer)
        (check-mined-halt (raised (lambda () (deploy-contract init #:from anvil-alice #:gas gas)))
                          halt words)
        (check-eq? (exn->halt (raised (lambda () (deploy-contract init #:from anvil-alice))))
                   halt words)))

    (test-case "A8: a creation at an address that has code is an address collision"
      (define init (code-bytes "600060005360016000f3"))
      (define nonce (eth-get-transaction-count anvil-carol "pending"))
      ;; The replay runs after the block, from the next nonce; both addresses collide.
      (for ([n (in-list (list nonce (add1 nonce)))])
        (anvil-set-code (create-address anvil-carol n) #"\0"))
      (check-rpc-halt (raised (lambda () (call-transaction anvil-carol #f #:data init)))
                      'address-collision "EVM error CreateCollision")
      (define mined
        (raised (lambda ()
                  (send-transaction/receipt anvil-carol #f #:data init #:gas-limit 1000000))))
      (check-mined-halt mined 'address-collision "EVM error CreateCollision")
      (check-equal? (tracer-error mined) "contract address collision"))

    (test-case "A9: init code over the size limit is refused in the node's words, and not mined"
      (define init (make-bytes 49153 0))
      (check-rpc-halt (raised (lambda () (call-transaction anvil-alice #f #:data init)))
                      'initcode-size "max initcode size exceeded")
      (define nonce (eth-get-transaction-count anvil-alice "pending"))
      (define refused
        (raised (lambda () (send-transaction/receipt anvil-alice #f #:data init
                                                     #:gas-limit 3000000))))
      (check-rpc-halt refused 'initcode-size "max initcode size exceeded")
      (check-equal? (eth-get-transaction-count anvil-alice "pending") nonce)))

  (define (contrast-cases)
    (test-case "A10: a mined revert carries its revert and no halt"
      (define reverter (installed "60006000fd"))
      (define called (raised (lambda () (call-transaction anvil-alice reverter))))
      (check-pred exn:fail:web3:rpc:revert? called)
      (check-false (exn->halt called))
      (define e
        (raised (lambda () (send-transaction/receipt anvil-alice reverter #:gas-limit 100000))))
      (check-pred exn:fail:web3:transaction? e)
      (check-equal? (abi-revert-data (exn:fail:web3:transaction-revert e)) #"")
      (check-false (exn:fail:web3:transaction-halt e))
      (check-false (exn->halt e)))

    (test-case "A11: a halt in a nested static call is the tracer's write protection"
      (define store (installed "6001600055"))
      (define pay (installed "6000600060006000600160015af100"))
      (for ([inner (in-list (list store pay))])
        (define outer (installed (string-append "600060006000600073" (substring inner 2) "5afa00")))
        (define receipt (send-transaction/receipt anvil-alice outer #:gas-limit 200000))
        (check-equal? (receipt-status receipt) 1)
        (define trace
          (debug-trace-transaction (receipt-transaction-hash receipt) (hasheq 'tracer "callTracer")))
        (match-define (list (hash-table ('error words) _ ...)) (hash-ref trace 'calls))
        (check-equal? words "write protection")
        (check-eq? (message->halt words) 'write-protection))))

  (define (paris-cases)
    (test-case "A12: an opcode of a later fork is undefined"
      (define e (raised (lambda () (call-transaction anvil-alice (installed "5f00")))))
      (check-rpc-halt e 'undefined-opcode "EVM error NotActivated")
      (define mined
        (raised (lambda () (send-transaction/receipt anvil-alice (installed "5f00")
                                                     #:gas-limit 100000))))
      (check-mined-halt mined 'undefined-opcode "EVM error NotActivated")
      (check-equal? (tracer-error mined) "invalid opcode")))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping halt anvil tests: anvil not found")]
    [else
     (with-anvil ()
       (runtime-cases)
       (creation-cases)
       (contrast-cases))
     (with-anvil (#:hardfork "paris")
       (paris-cases))]))
