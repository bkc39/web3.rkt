#lang racket/base

(module+ test
  (require (only-in racket/contract flat-contract-predicate)
           racket/list
           rackunit
           (only-in scribble/example make-base-eval)
           (only-in web3 decode-revert exn:fail:web3 exn:fail:web3:rpc exn:fail:web3:rpc:revert
                    exn:fail:web3:transaction)
           web3/halt
           "modules.rkt"
           (only-in "util.rkt" check-blames-here))

  (define anvil-names
    '((out-of-gas "OutOfGas" "MemoryOOG" "MemoryLimitOOG" "PrecompileOOG" "InvalidOperandOOG"
                  "ReentrancySentryOOG")
      (stack-overflow "StackOverflow")
      (stack-underflow "StackUnderflow")
      (invalid-opcode "InvalidFEOpcode")
      (undefined-opcode "OpcodeNotFound" "NotActivated")
      (invalid-jump "InvalidJump")
      (return-data-out-of-bounds "OutOfOffset")
      (write-protection "StateChangeDuringStaticCall" "CallNotAllowedInsideStatic")
      (call-depth "CallTooDeep")
      (code-size "CreateContractSizeLimit")
      (initcode-size "CreateInitCodeSizeLimit")
      (invalid-code "CreateContractStartingWithEF")
      (address-collision "CreateCollision")))

  (define anvil-words
    '((out-of-gas "out of gas" "out of gas: out of memory" "out of gas: reach memory limit"
                  "out of gas: invalid operand"
                  "Out of gas: gas required exceeds allowance: 50000")
      (stack-overflow "Out of stack")
      (stack-underflow "stack underflow")
      (invalid-opcode "invalid opcode: INVALID")
      (undefined-opcode "invalid opcode")
      (invalid-jump "invalid jump destination")
      (return-data-out-of-bounds "return data out of bounds")
      (write-protection "write protection")
      (code-size "max code size exceeded")
      (initcode-size "max initcode size exceeded")
      (invalid-code "invalid code: must not begin with 0xef")
      (address-collision "contract address collision")))

  (define geth-words
    '((out-of-gas "out of gas" "contract creation code storage out of gas" "gas uint64 overflow"
                  "gas required exceeds allowance (30000000)")
      (stack-overflow "stack limit reached 1024 (1023)")
      (stack-underflow "stack underflow (0 <=> 1)" "stack underflow (1 <=> 2)")
      (invalid-opcode "invalid opcode: INVALID")
      (undefined-opcode "invalid opcode: opcode 0xc not defined"
                        "invalid opcode: opcode 0xef not defined" "invalid opcode: PUSH0")
      (invalid-jump "invalid jump destination")
      (return-data-out-of-bounds "return data out of bounds")
      (write-protection "write protection")
      (call-depth "max call depth exceeded")
      (code-size "max code size exceeded")
      (initcode-size "max initcode size exceeded")
      (invalid-code "invalid code: must not begin with 0xef")
      (address-collision "contract address collision")))

  (define (check-rows rows message-of)
    (for* ([row (in-list rows)]
           [wording (in-list (rest row))])
      (define message (message-of wording))
      (check-eq? (message->halt message) (first row) message)))

  (define (rpc-exn message #:code [code -32003])
    (exn:fail:web3:rpc (format "eth_call: ~a" message) (current-continuation-marks)
                       "eth_call" code message #f))

  (test-case "H1: anvil's EVM error names give their halts"
    (check-rows anvil-names (lambda (name) (string-append "EVM error " name))))

  (test-case "H2: anvil's tracer and estimate words give their halts"
    (check-rows anvil-words values))

  (test-case "H3: go-ethereum's words give their halts"
    (check-rows geth-words values))

  (test-case "H4: matching ignores case"
    (check-eq? (message->halt "evm error outofgas") 'out-of-gas)
    (check-eq? (message->halt "EVM ERROR STACKOVERFLOW") 'stack-overflow)
    (check-eq? (message->halt "OUT OF STACK") 'stack-overflow)
    (check-eq? (message->halt "invalid opcode: invalid") 'invalid-opcode)
    (check-eq? (message->halt "Invalid Jump Destination") 'invalid-jump))

  (test-case "H5: a revert is never a halt, whatever its reason says"
    (for ([message (in-list '("execution reverted"
                              "execution reverted: out of gas"
                              "execution reverted: out of gas token"
                              "execution reverted: EVM error OutOfGas"
                              "execution reverted: invalid opcode: INVALID"
                              "Execution Reverted: stack underflow"))])
      (check-false (message->halt message) message)))

  (test-case "H6: words that name no halt give #f"
    (for ([message (in-list '(""
                              "nonce too low"
                              "insufficient funds for gas * price + value"
                              "intrinsic gas too low"
                              "out of gasoline"
                              "running out of gas"
                              "EVM error Revert"
                              "EVM error OutOfFunds"
                              "EVM error OutOfGasX"
                              "EVM error"
                              "the EVM error OutOfGas"
                              "VM Exception while processing transaction: out of gas"
                              "invalid opcode:"
                              "invalid opcode: "
                              "stack underflow (a <=> b)"
                              "stack limit reached"
                              "write protection enabled"))])
      (check-false (message->halt message) message)))

  (test-case "H7: halts and halt/c agree, and every halt has a wording"
    (define halt? (flat-contract-predicate halt/c))
    (check-equal? (remove-duplicates halts) halts)
    (for ([halt (in-list halts)])
      (check-true (halt? halt) (format "~a" halt)))
    (for ([value (in-list '(revert out-of-gas? "out-of-gas" #f))])
      (check-false (halt? value) (format "~s" value)))
    (check-equal? (map first anvil-names) halts)
    (check-equal? (map first geth-words) halts))

  (test-case "H8: exn->halt reads an RPC error's message, not a revert's"
    (check-eq? (exn->halt (rpc-exn "EVM error InvalidJump")) 'invalid-jump)
    (check-eq? (exn->halt (rpc-exn "out of gas" #:code -32000)) 'out-of-gas)
    (check-false (exn->halt (rpc-exn "nonce too low")))
    (define revert
      (exn:fail:web3:rpc:revert "eth_call: reverted" (current-continuation-marks)
                                "eth_call" 3 "out of gas" #f (decode-revert #"")))
    (check-false (exn->halt revert)))

  (test-case "H8: exn->halt reads a failed transaction's halt field"
    (define (failed halt)
      (exn:fail:web3:transaction "f: failed" (current-continuation-marks) #f #f halt))
    (check-eq? (exn->halt (failed 'stack-overflow)) 'stack-overflow)
    (check-false (exn->halt (failed #f))))

  (test-case "H8: exn->halt of any other exception is #f, whatever its message"
    (check-false (exn->halt (exn:fail:web3 "out of gas" (current-continuation-marks))))
    (check-false (exn->halt (exn:fail "EVM error OutOfGas" (current-continuation-marks)))))

  (test-case "H9: the arguments are checked by contract"
    (check-blames-here (message->halt 'out-of-gas))
    (check-blames-here (exn->halt "EVM error OutOfGas")))

  (test-case "H10: web3 re-exports web3/halt, which needs only racket/base and private/exn"
    (define all (exported-names 'web3))
    (for ([name (in-list (exported-names 'web3/halt))])
      (check-not-false (memq name all) (format "~a" name)))
    (check-equal? (sort (exported-names 'web3/halt) symbol<?)
                  '(exn->halt halt/c halts message->halt))
    (define names (filter values (map module-name (import-closure 'web3/halt))))
    (check-equal? (sort (filter (lambda (n) (equal? (collection n) "web3")) names) string<?)
                  '("web3/halt" "web3/private/exn"))
    (define ev (make-base-eval))
    (check-not-exn (lambda () (ev '(require web3 web3/halt))))))
