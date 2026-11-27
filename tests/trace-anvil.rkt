#lang racket/base

(module+ test
  (require racket/match
           rackunit
           web3
           "helpers/halts.rkt"
           (only-in "helpers/programs.rkt" caller-of echo-init echo-runtime)
           (only-in "util.rkt" anvil-alice raised word))

  (define refused (bytes-append (bytes #xff) (make-bytes 31 7)))

  (define next-address
    (let ([counter (box #x7ace00)])
      (lambda ()
        (set-box! counter (add1 (unbox counter)))
        (integer->address (unbox counter)))))

  (define (installed code)
    (define address (next-address))
    (anvil-set-code address (string-append "0x" code))
    address)

  (define (destination row)
    (define target (halting-target row))
    (if (address? target) target (installed target)))

  (define (mined-failure to #:data data #:gas-limit gas)
    (define e
      (raised (lambda () (send-transaction/receipt anvil-alice to #:data data #:gas-limit gas))))
    (check-pred exn:fail:web3:transaction? e)
    (receipt-transaction-hash (exn:fail:web3:transaction-receipt e)))

  (define (echo-cases echo)
    (test-case "A1: a mined send's trace has the echoed word as its output"
      (define receipt (send-transaction/receipt anvil-alice echo #:data (word 7)))
      (define trace (transaction-call-trace (receipt-transaction-hash receipt)))
      (check-equal? (call-trace-type trace) "CALL")
      (check-equal? (call-trace-from trace) anvil-alice)
      (check-equal? (call-trace-to trace) echo)
      (check-equal? (call-trace-value trace) 0)
      (check-equal? (call-trace-input trace) (word 7))
      (check-equal? (call-trace-output trace) (word 7))
      (check-equal? (call-trace-gas-used trace) (receipt-gas-used receipt))
      (check-false (call-trace-error trace))
      (check-false (call-trace-halt trace))
      (check-equal? (call-trace-calls trace) '()))

    (test-case "A2: a mined send that fails has the tracer's error and the revert data as output"
      (define trace (transaction-call-trace (mined-failure echo #:data refused #:gas-limit 100000)))
      (check-equal? (call-trace-error trace) "execution reverted")
      (check-equal? (call-trace-output trace) refused)
      (check-false (call-trace-halt trace)))

    (test-case "A3: call-transaction/trace traces the call call-transaction makes"
      (define trace (call-transaction/trace anvil-alice echo #:data (word 3)))
      (check-equal? (call-trace-output trace) (call-transaction anvil-alice echo #:data (word 3)))
      (check-false (call-trace-error trace))
      (define created (call-transaction/trace #f #f #:data echo-init))
      (check-equal? (call-trace-type created) "CREATE")
      (check-equal? (call-trace-output created) echo-runtime))

    (test-case "A4: anvil answers a call that reverts with a trace, not an error"
      (define trace (call-transaction/trace anvil-alice echo #:data refused))
      (check-equal? (call-trace-error trace) "execution reverted")
      (check-equal? (call-trace-output trace) refused)
      (check-pred exn:fail:web3:rpc:revert?
                  (raised (lambda () (call-transaction anvil-alice echo #:data refused)))))

    (define outer (installed (caller-of echo)))

    (test-case "A5: a contract that calls another has the inner call in call-trace-calls"
      (define receipt (send-transaction/receipt anvil-alice outer #:data (word 5)))
      (define trace (transaction-call-trace (receipt-transaction-hash receipt)))
      (check-equal? (call-trace-output trace) (word 5))
      (match-define (list inner) (call-trace-calls trace))
      (check-equal? (call-trace-from inner) outer)
      (check-equal? (call-trace-to inner) echo)
      (check-equal? (call-trace-input inner) (word 5))
      (check-equal? (call-trace-output inner) (word 5))
      (check-equal? (call-trace-calls inner) '()))

    (test-case "A5: an inner call that reverts is a frame with the error and the revert data"
      (define trace (call-transaction/trace anvil-alice outer #:data refused))
      (match-define (list called) (call-trace-calls trace))
      (check-equal? (call-trace-error called) "execution reverted")
      (check-equal? (call-trace-output called) refused))

    (define tx (hasheq 'from anvil-alice 'to echo 'data (bytes->hex-string (word 9))))

    (test-case "A6: trace-transaction answers the mined send's parity frames"
      (define receipt (send-transaction/receipt anvil-alice echo #:data (word 9)))
      (define tx-hash (receipt-transaction-hash receipt))
      (match-define (list (? hash? frame)) (trace-transaction tx-hash))
      (define result (hash-ref frame 'result))
      (check-equal? (hash-ref frame 'type) "call")
      (check-equal? (hash-ref result 'output) (bytes->hex-string (word 9)))
      (check-equal? (hash-ref frame 'transactionHash) tx-hash))

    (test-case "A6: trace-call answers the traces its types ask for and the output"
      (define traced (trace-call tx '("trace") "latest"))
      (define everything (trace-call tx '("trace" "vmTrace" "stateDiff") "latest"))
      (check-equal? (hash-ref traced 'output) (bytes->hex-string (word 9)))
      (check-pred list? (hash-ref traced 'trace))
      (check-equal? (hash-ref traced 'vmTrace) 'null)
      (check-pred hash? (hash-ref everything 'vmTrace))
      (check-pred hash? (hash-ref everything 'stateDiff)))

    (test-case "A6: trace-call's frame of a call that reverts has the error Reverted"
      (define reverting-tx (hash-set tx 'data (bytes->hex-string refused)))
      (define traced (trace-call reverting-tx '("trace") "latest"))
      (match-define (list (? hash? reverted)) (hash-ref traced 'trace))
      (check-equal? (hash-ref reverted 'error) "Reverted")))

  (define (halt-cases)
    (test-case "A7: a mined halt's trace names its halt"
      (for ([row (in-list (append code-halts precompile-halts))])
        (define words (halting-tracer row))
        (define trace
          (transaction-call-trace (mined-failure (destination row) #:data (halting-input row)
                                                 #:gas-limit (halting-gas row))))
        (check-equal? (call-trace-error trace) words)
        (check-eq? (call-trace-halt trace) (halting-halt row) words)))

    (test-case "A8: a call that halts is a trace that names its halt"
      (for ([row (in-list (append code-halts precompile-halts))])
        (define words (halting-tracer row))
        (define trace
          (call-transaction/trace anvil-alice (destination row) #:data (halting-input row)
                                  #:gas-limit (halting-gas row)))
        (check-equal? (call-trace-error trace) words)
        (check-eq? (call-trace-halt trace) (halting-halt row) words)))

    (test-case "A9: a creation that halts is a trace with no address"
      (for ([row (in-list creation-halts)])
        (define words (halting-tracer row))
        (define trace
          (call-transaction/trace anvil-alice #f
                                  #:data (hex-string->bytes (string-append "0x"
                                                                           (halting-target row)))))
        (check-equal? (call-trace-type trace) "CREATE" words)
        (check-false (call-trace-to trace) words)
        (check-equal? (call-trace-output trace) #"" words)
        (check-eq? (call-trace-halt trace) (halting-halt row) words)))

    (test-case "A10: a halt in a nested static call is in the inner frame"
      (for ([inner (in-list (list (installed store-one) (installed pay-one-wei)))])
        (define outer (installed (static-call-to inner)))
        (define receipt (send-transaction/receipt anvil-alice outer #:gas-limit 200000))
        (define trace (transaction-call-trace (receipt-transaction-hash receipt)))
        (check-false (call-trace-halt trace))
        (match-define (list static) (call-trace-calls trace))
        (check-equal? (call-trace-type static) "STATICCALL")
        (check-false (call-trace-value static))
        (check-eq? (call-trace-halt static) 'write-protection))))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping trace anvil tests: anvil not found")]
    [else
     (with-anvil ()
       (define-values (echo _deployed) (deploy-contract/receipt echo-init #:from anvil-alice))
       (echo-cases echo)
       (halt-cases))]))
