#lang racket/base

;; Exceptional halts produced on anvil by hand-assembled code and by precompiles, read back as
;; symbols.  Skipped when anvil is not available.

(module+ test
  (require racket/list
           racket/match
           rackunit
           web3
           (only-in "helpers/accounts.rkt" anvil-keys)
           "helpers/halts.rkt"
           (only-in "util.rkt" anvil-alice anvil-carol raised))

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

  (define (destination row)
    (define target (halting-target row))
    (if (address? target) target (installed target)))

  (define (init-code row)
    (code-bytes (halting-target row)))

  (define (sent-failure to #:data [data #""] #:gas-limit gas #:from [from anvil-alice])
    (raised (lambda () (send-transaction/receipt from to #:data data #:gas-limit gas))))

  (define (called row)
    (raised (lambda ()
              (call-transaction anvil-alice (destination row)
                                #:data (halting-input row) #:gas-limit (halting-gas row)))))

  (define (mined row)
    (sent-failure (destination row) #:data (halting-input row) #:gas-limit (halting-gas row)))

  (define (called-creation row)
    (raised (lambda () (call-transaction anvil-alice #f #:data (init-code row)))))

  (define (mined-creation row)
    (sent-failure #f #:data (init-code row) #:gas-limit (halting-gas row)))

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

  (define (check-message-ends e words)
    (check-regexp-match (regexp (string-append ": " (regexp-quote words) "$")) (exn-message e)))

  (define (call-trace tx-hash)
    (debug-trace-transaction tx-hash (hasheq 'tracer "callTracer")))

  (define (tracer-error e)
    (define tx-hash (receipt-transaction-hash (exn:fail:web3:transaction-receipt e)))
    (hash-ref (call-trace tx-hash) 'error #f))

  (define (check-tracer-halt e halt words)
    (check-equal? (tracer-error e) words)
    (check-eq? (message->halt words) halt words))

  (define (runtime-cases)
    (define rows (append code-halts precompile-halts))

    (test-case "A1: call-transaction raises the node's words, and exn->halt names the halt"
      (for ([row (in-list rows)])
        (check-rpc-halt (called row) (halting-halt row) (halting-words row))))

    (test-case "A2: a send mined with status 0 carries the replay's halt and no revert"
      (for ([row (in-list rows)])
        (check-mined-halt (mined row) (halting-halt row) (halting-words row))))

    (test-case "A3: the message of a mined halt ends with the replay's words"
      (for ([row (in-list rows)])
        (check-message-ends (mined row) (halting-words row))))

    (test-case "A4: the call tracer's error for the mined transaction names the same halt"
      (for ([row (in-list rows)])
        (check-tracer-halt (mined row) (halting-halt row) (halting-tracer row))))

    (test-case "A5: a contract call raises the halt"
      (for ([row (in-list code-halts)])
        (define address (destination row))
        (define failure
          (raised (lambda () (contract-call address "f()(uint256)" #:from anvil-alice))))
        (check-rpc-halt failure (halting-halt row) (halting-words row))))

    (test-case "A6: an estimated contract send raises the halt from eth_estimateGas"
      (for ([row (in-list code-halts)])
        (define address (destination row))
        (define words (halting-words row))
        (define estimated (raised (lambda () (contract-send address "f()" #:from anvil-alice))))
        (check-equal? (exn:fail:web3:rpc-method estimated) "eth_estimateGas" words)
        (check-eq? (exn->halt estimated) (halting-halt row) words)))

    (test-case "A7: anvil estimates a send that runs out of gas in words of its own"
      (define e (raised (lambda () (send-transaction anvil-alice (installed jump-to-self)))))
      (check-equal? (exn:fail:web3:rpc-method e) "eth_estimateGas")
      (check-regexp-match #rx"^Out of gas: gas required exceeds allowance: [0-9]+$"
                          (exn:fail:web3:rpc-rpc-message e))
      (check-eq? (exn->halt e) 'out-of-gas))

    (test-case "A8: a send from a local signer carries its halt as one the node signs does"
      (define signer (local-signer (hex-string->secret-key (third anvil-keys))))
      (define e (sent-failure (installed designated-invalid) #:from signer #:gas-limit 100000))
      (check-mined-halt e 'invalid-opcode "EVM error InvalidFEOpcode")))

  (define (creation-cases)
    (test-case "A9: a creation that halts raises its halt from call-transaction"
      (for ([row (in-list creation-halts)])
        (check-rpc-halt (called-creation row) (halting-halt row) (halting-words row))))

    (test-case "A10: a creation mined with status 0 carries its halt and no revert"
      (for ([row (in-list creation-halts)])
        (check-mined-halt (mined-creation row) (halting-halt row) (halting-words row))))

    (test-case "A11: the call tracer's error for a mined creation names the same halt"
      (for ([row (in-list creation-halts)])
        (check-tracer-halt (mined-creation row) (halting-halt row) (halting-tracer row))))

    (test-case "A12: a deploy with a gas limit is mined and carries the creation's halt"
      (for ([row (in-list creation-halts)])
        (define e
          (raised (lambda ()
                    (deploy-contract (init-code row) #:from anvil-alice #:gas (halting-gas row)))))
        (check-mined-halt e (halting-halt row) (halting-words row))))

    (test-case "A13: an estimated deploy raises the creation's halt"
      (for ([row (in-list creation-halts)])
        (define e (raised (lambda () (deploy-contract (init-code row) #:from anvil-alice))))
        (check-eq? (exn->halt e) (halting-halt row) (halting-words row))))

    (test-case "A14: a creation called where the sender's next contract has code collides"
      (define nonce (eth-get-transaction-count anvil-carol "pending"))
      (anvil-set-code (create-address anvil-carol nonce) #"\0")
      (check-rpc-halt (raised (lambda ()
                                (call-transaction anvil-carol #f
                                                  #:data (code-bytes return-a-zero-byte))))
                      'address-collision "EVM error CreateCollision"))

    (test-case "A15: a mined collision is replayed from the next nonce, where it does not fail"
      (define nonce (eth-get-transaction-count anvil-carol "pending"))
      (anvil-set-code (create-address anvil-carol nonce) #"\0")
      (define e
        (sent-failure #f #:from anvil-carol #:data (code-bytes return-a-zero-byte)
                      #:gas-limit 1000000))
      (check-pred exn:fail:web3:transaction? e)
      (check-equal? (receipt-status (exn:fail:web3:transaction-receipt e)) 0)
      (check-false (exn:fail:web3:transaction-revert e))
      (check-false (exn:fail:web3:transaction-halt e))
      (check-message-ends e "replaying it did not fail"))

    (test-case "A16: the call tracer names a mined collision"
      (define nonce (eth-get-transaction-count anvil-carol "pending"))
      (anvil-set-code (create-address anvil-carol nonce) #"\0")
      (define e
        (sent-failure #f #:from anvil-carol #:data (code-bytes return-a-zero-byte)
                      #:gas-limit 1000000))
      (check-tracer-halt e 'address-collision "contract address collision"))

    (test-case "A17: a call of init code over the size limit is refused in initcode-size's words"
      (check-rpc-halt (raised (lambda ()
                                (call-transaction anvil-alice #f #:data (make-bytes 49153 0))))
                      'initcode-size "max initcode size exceeded"))

    (test-case "A18: a send of init code over the size limit is refused in initcode-size's words"
      (check-rpc-halt (sent-failure #f #:data (make-bytes 49153 0) #:gas-limit 3000000)
                      'initcode-size "max initcode size exceeded"))

    (test-case "A19: a refused send of oversized init code is not mined"
      (define nonce (eth-get-transaction-count anvil-alice "pending"))
      (sent-failure #f #:data (make-bytes 49153 0) #:gas-limit 3000000)
      (check-equal? (eth-get-transaction-count anvil-alice "pending") nonce)))

  (define (contrast-cases)
    (test-case "A20: a call that reverts is a revert, with no halt"
      (define e (raised (lambda () (call-transaction anvil-alice (installed revert-empty)))))
      (check-pred exn:fail:web3:rpc:revert? e)
      (check-false (exn->halt e)))

    (test-case "A21: a mined revert carries its revert and no halt"
      (define e (sent-failure (installed revert-empty) #:gas-limit 100000))
      (check-pred exn:fail:web3:transaction? e)
      (check-equal? (abi-revert-data (exn:fail:web3:transaction-revert e)) #"")
      (check-false (exn:fail:web3:transaction-halt e))
      (check-false (exn->halt e)))

    (test-case "A22: a halt in a nested static call is the tracer's write protection"
      (for ([inner (in-list (list (installed store-one) (installed pay-one-wei)))])
        (define outer (installed (static-call-to inner)))
        (define receipt (send-transaction/receipt anvil-alice outer #:gas-limit 200000))
        (check-equal? (receipt-status receipt) 1)
        (define trace (call-trace (receipt-transaction-hash receipt)))
        (match-define (list (hash-table ('error words) _ ...)) (hash-ref trace 'calls))
        (check-equal? words "write protection")
        (check-eq? (message->halt words) 'write-protection))))

  (define (paris-cases)
    (test-case "A23: a call of an opcode of a later fork raises an undefined opcode"
      (check-rpc-halt (raised (lambda () (call-transaction anvil-alice (installed push-zero))))
                      'undefined-opcode "EVM error NotActivated"))

    (test-case "A24: a mined opcode of a later fork carries an undefined opcode"
      (check-mined-halt (sent-failure (installed push-zero) #:gas-limit 100000)
                        'undefined-opcode "EVM error NotActivated"))

    (test-case "A25: the call tracer names a mined opcode of a later fork undefined"
      (check-tracer-halt (sent-failure (installed push-zero) #:gas-limit 100000)
                         'undefined-opcode "invalid opcode")))

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
