#lang racket/base

(module+ test
  (require (only-in racket/contract flat-contract-predicate)
           (only-in racket/file file->string)
           racket/list
           racket/match
           rackunit
           (only-in scribble/example make-base-eval)
           (only-in web3 decode-revert exn:fail:web3 exn:fail:web3:rpc exn:fail:web3:rpc:revert
                    exn:fail:web3:transaction)
           web3/halt
           (only-in (submod web3/halt internal) halt-table)
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
      (precompile-failure "PrecompileError")
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
      (precompile-failure "precompiled failed")
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

  (define other-cases
    '((out-of-gas "evm error outofgas")
      (stack-overflow "EVM ERROR STACKOVERFLOW" "OUT OF STACK")
      (invalid-opcode "invalid opcode: invalid")
      (invalid-jump "Invalid Jump Destination")))

  (define (check-rows rows message-of)
    (for* ([row (in-list rows)]
           [wording (in-list (rest row))])
      (define message (message-of wording))
      (check-eq? (message->halt message) (first row) message)))

  (define (check-no-halt messages)
    (for ([message (in-list messages)])
      (check-false (message->halt message) message)))

  (define (rpc-exn message #:code [code -32003])
    (exn:fail:web3:rpc (format "eth_call: ~a" message) (current-continuation-marks)
                       "eth_call" code message #f))

  (define (failed halt)
    (exn:fail:web3:transaction "f: failed" (current-continuation-marks) #f #f halt))

  (define halt? (flat-contract-predicate halt/c))

  (define numbers '(("N" . "1024") ("M" . "1023")))

  (define details
    '((out-of-gas "out of memory")
      (undefined-opcode "opcode 0xc not defined" "PUSH0")))

  (define (marked form row)
    (define pattern (pregexp (format "@~a\\{((?:[^{}]|\\{[^{}]*\\})*)\\}" form)))
    (regexp-match* pattern row #:match-select cadr))

  (define (filled halt content)
    (define (fill detail)
      (regexp-replace* #px"@italic\\{(\\w+)\\}" content
                       (lambda (_italic name)
                         (if (equal? name "detail") detail (cdr (assoc name numbers))))))
    (match* ((regexp-match? #rx"@italic{detail}" content) (assq halt details))
      [(#f _) (list (fill #f))]
      [(#t (list _ row-details ...)) (map fill row-details)]))

  (struct manual-row (halt names wordings))

  (define (manual-halt-rows)
    (define manual (file->string (collection-file-path "scribblings/web3.scrbl" "web3")))
    (match-define (list _ table)
      (regexp-match #px"@subsection\\[#:tag \"halts\"\\].*?(@tabular\\[.*?)\n\n" manual))
    (for/list ([row (in-list (rest (regexp-split #rx"\\(list @racket\\['" table)))])
      (match-define (list _ halt) (regexp-match #px"^([a-z-]+)\\]" row))
      (manual-row (string->symbol halt) (marked "evm-error" row) (marked "wording" row))))

  (define (manual-row-of halt)
    (findf (lambda (row) (eq? (manual-row-halt row) halt)) (manual-halt-rows)))

  (define (shown? halt wording shown)
    (match wording
      [(? string?) (member wording shown)]
      [(list 'detail start _ ...) (member (string-append start ": @italic{detail}") shown)]
      [(? pregexp?)
       (define whole (pregexp (format "^(?i:~a)$" (object-name wording))))
       (for*/or ([content (in-list shown)]
                 #:when (regexp-match? #rx"@italic{" content)
                 [instance (in-list (filled halt content))])
         (regexp-match? whole instance))]))

  (test-case "H1: anvil's EVM error names give their halts"
    (check-rows anvil-names (lambda (name) (string-append "EVM error " name))))

  (test-case "H2: anvil's tracer and estimate words give their halts"
    (check-rows anvil-words values))

  (test-case "H3: go-ethereum's words give their halts"
    (check-rows geth-words values))

  (test-case "H4: matching ignores case"
    (check-rows other-cases values))

  (test-case "H5: a revert is never a halt, whatever its reason says"
    (check-no-halt '("execution reverted"
                     "execution reverted: out of gas"
                     "execution reverted: out of gas token"
                     "execution reverted: EVM error OutOfGas"
                     "execution reverted: invalid opcode: INVALID"
                     "Execution Reverted: stack underflow")))

  (test-case "H6: words that name no halt give #f"
    (check-no-halt '(""
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
                     "write protection enabled")))

  (test-case "H7: words with a detail need `: ` and a detail after them that starts with no space"
    (check-no-halt '("out of gas: " "out of gas:" "out of gas:out of memory" "out of gas - x"
                     "out of gas:  " "invalid opcode: \n" "invalid opcode:  PUSH0")))

  (test-case "H8: INVALID followed by more words is not an undefined opcode, nor any halt"
    (check-no-halt '("invalid opcode: INVALID " "invalid opcode: INVALID, at pc 3"
                     "invalid opcode:  INVALID")))

  (test-case "H9: halts has no duplicates"
    (check-equal? (remove-duplicates halts) halts))

  (test-case "H10: halt/c accepts every halt"
    (for ([halt (in-list halts)])
      (check-true (halt? halt) (format "~a" halt))))

  (test-case "H11: halt/c refuses what is not a halt"
    (for ([value (in-list '(revert out-of-gas? "out-of-gas" #f))])
      (check-false (halt? value) (format "~s" value))))

  (test-case "H12: halts are in the order of anvil's names, and all but a precompile's are geth's"
    (check-equal? (map first anvil-names) halts)
    (check-equal? (map first geth-words) (remq 'precompile-failure halts)))

  (test-case "H13: the manual's table has the halts in order"
    (check-equal? (map manual-row-halt (manual-halt-rows)) halts))

  (test-case "H14: every name and wording the manual's table shows gives the halt of its row"
    (for ([row (in-list (manual-halt-rows))])
      (define halt (manual-row-halt row))
      (for ([name (in-list (manual-row-names row))])
        (define message (string-append "EVM error " name))
        (check-eq? (message->halt message) halt message))
      (for* ([content (in-list (manual-row-wordings row))]
             [message (in-list (filled halt content))])
        (check-eq? (message->halt message) halt message))))

  (test-case "H25: each row of the manual's table shows exactly its halt's EVM error names"
    (for ([code-row (in-list halt-table)])
      (match-define (list halt names _) code-row)
      (define row (manual-row-of halt))
      (check-pred pair? (manual-row-names row) (format "~a" halt))
      (check-equal? (sort (manual-row-names row) string<?) (sort names string<?)
                    (format "~a" halt))))

  (test-case "H26: each row of the manual's table shows every wording of its halt"
    (for* ([code-row (in-list halt-table)]
           [wording (in-list (third code-row))])
      (define halt (first code-row))
      (check-not-false (shown? halt wording (manual-row-wordings (manual-row-of halt)))
                       (format "~a: ~s" halt wording))))

  (test-case "H15: exn->halt reads an RPC error's message"
    (check-eq? (exn->halt (rpc-exn "EVM error InvalidJump")) 'invalid-jump)
    (check-eq? (exn->halt (rpc-exn "out of gas" #:code -32000)) 'out-of-gas)
    (check-false (exn->halt (rpc-exn "nonce too low"))))

  (test-case "H16: exn->halt of a revert is #f, whatever its message"
    (define revert
      (exn:fail:web3:rpc:revert "eth_call: reverted" (current-continuation-marks)
                                "eth_call" 3 "out of gas" #f (decode-revert #"")))
    (check-false (exn->halt revert)))

  (test-case "H17: exn->halt reads a failed transaction's halt field"
    (check-eq? (exn->halt (failed 'stack-overflow)) 'stack-overflow)
    (check-false (exn->halt (failed #f))))

  (test-case "H18: exn->halt of a failed transaction built with what is not a halt is #f"
    (for ([value (in-list '(revert "out-of-gas" 0))])
      (check-false (exn->halt (failed value)) (format "~s" value))))

  (test-case "H19: exn->halt of any other exception is #f, whatever its message"
    (check-false (exn->halt (exn:fail:web3 "out of gas" (current-continuation-marks))))
    (check-false (exn->halt (exn:fail "EVM error OutOfGas" (current-continuation-marks)))))

  (test-case "H20: the arguments are checked by contract"
    (check-blames-here (message->halt 'out-of-gas))
    (check-blames-here (exn->halt "EVM error OutOfGas")))

  (test-case "H21: web3 re-exports web3/halt"
    (define all (exported-names 'web3))
    (for ([name (in-list (exported-names 'web3/halt))])
      (check-not-false (memq name all) (format "~a" name))))

  (test-case "H22: web3/halt exports its four names"
    (check-equal? (sort (exported-names 'web3/halt) symbol<?)
                  '(exn->halt halt/c halts message->halt)))

  (test-case "H23: web3/halt needs only racket/base libraries and private/exn"
    (define names (filter values (map module-name (import-closure 'web3/halt))))
    (check-equal? (sort (filter (lambda (name) (equal? (collection name) "web3")) names) string<?)
                  '("web3/halt" "web3/private/exn")))

  (test-case "H24: web3 and web3/halt require together"
    (define evaluator (make-base-eval))
    (check-not-exn (lambda () (evaluator '(require web3 web3/halt))))))
