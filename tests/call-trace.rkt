#lang racket/base

;; Call traces read from the node's JSON, and the requests that fetch them, against mock nodes.

(module+ test
  (require (only-in racket/contract exn:fail:contract:blame?)
           (only-in racket/file file->string)
           racket/format
           racket/list
           racket/match
           racket/string
           rackunit
           web3
           (only-in (submod web3/eth/call-trace internal) call-trace-fields)
           "helpers/mock.rkt"
           (only-in "helpers/records.rkt"
                    check-fields check-in-web3-and-manual message-of provided-accessors)
           (only-in "modules.rkt" exported-names)
           (only-in "util.rkt" anvil-alice anvil-bob anvil-carol check-blames-here filled-hash
                    raised word))

  (define caller (integer->address #xca11e))
  (define echo "0x5FbDB2315678afecb367f032d93F642f64180aa3")
  (define reason-data (encode-call "Error(string)" "too low"))

  (define inner-json
    (hasheq 'type "CALL"
            'from (string-downcase caller)
            'to (string-downcase echo)
            'value "0x0"
            'gas "0x42d"
            'gasUsed "0x428"
            'input (bytes->hex-string (word 5))
            'output (bytes->hex-string (word 5))))

  (define frame-json
    (hasheq 'type "CALL"
            'from (string-downcase anvil-alice)
            'to (string-downcase caller)
            'value "0x7"
            'gas "0xe89"
            'gasUsed "0x611d"
            'input (bytes->hex-string (word 5))
            'output (bytes->hex-string reason-data)
            'error "execution reverted"
            'revertReason "too low"
            'calls (list inner-json)))

  (define failed-creation-json
    (hasheq 'type "CREATE"
            'from (string-downcase anvil-alice)
            'gas "0x1c8f3ee"
            'gasUsed "0x1c9c380"
            'input "0x60ef60005360016000f3"
            'error "invalid code: must not begin with 0xef"))

  (define expected-fields
    `([type "CALL"]
      [from ,anvil-alice]
      [to ,caller]
      [value 7]
      [gas #xe89]
      [gas-used #x611d]
      [input ,(word 5)]
      [output ,reason-data]
      [error "execution reverted"]
      [revert-reason "too low"]
      [calls ,(list (jsexpr->call-trace inner-json))]))

  (test-case "T1: a frame with every field reads into its fields"
    (define trace (jsexpr->call-trace frame-json))
    (check-fields trace "call-trace" call-trace-fields expected-fields)
    (check-eq? (call-trace->jsexpr trace) frame-json))

  (test-case "T1: a frame without its optional fields reads them as #f, #\"\" and '()"
    (define trace (jsexpr->call-trace failed-creation-json))
    (check-equal? (call-trace-type trace) "CREATE")
    (check-false (call-trace-to trace))
    (check-false (call-trace-value trace))
    (check-equal? (call-trace-output trace) #"")
    (check-false (call-trace-revert-reason trace))
    (check-equal? (call-trace-calls trace) '())
    (check-equal? (call-trace-error trace) "invalid code: must not begin with 0xef")
    (check-false (call-trace-error (jsexpr->call-trace (hash-remove failed-creation-json 'error))))
    (check-false (call-trace-to (jsexpr->call-trace (hash-set failed-creation-json 'to 'null)))))

  (test-case "T2: nested calls are call traces, to any depth"
    (define deepest (hash-set inner-json 'type "STATICCALL"))
    (define middle (hash-set* inner-json 'type "DELEGATECALL" 'calls (list deepest deepest)))
    (define trace (jsexpr->call-trace (hash-set frame-json 'calls (list middle inner-json))))
    (match-define (list first-call second-call) (call-trace-calls trace))
    (check-equal? (call-trace-type first-call) "DELEGATECALL")
    (check-equal? (map call-trace-type (call-trace-calls first-call)) '("STATICCALL" "STATICCALL"))
    (check-equal? (call-trace-from second-call) caller)
    (check-equal? (call-trace-to second-call) echo)
    (check-equal? (call-trace-output second-call) (word 5))
    (check-equal? (call-trace-calls second-call) '()))

  (test-case "T2: every frame type the call tracer names reads"
    (for ([type (in-list '("CALL" "STATICCALL" "DELEGATECALL" "CALLCODE" "CREATE" "CREATE2"
                           "SELFDESTRUCT"))])
      (check-equal? (call-trace-type (jsexpr->call-trace (hash-set inner-json 'type type))) type)))

  (define malformed-rows
    `([,(hash-set frame-json 'input "0xzz") "call trace input" "\"0xzz\""]
      [,(hash-set frame-json 'gas "12") "call trace gas" "\"12\""]
      [,(hash-set frame-json 'gas 12) "call trace gas" "12"]
      [,(hash-set frame-json 'gasUsed "0x") "call trace gas used" "\"0x\""]
      [,(hash-set frame-json 'calls "0x") "call trace calls" "\"0x\""]
      [,(hash-set frame-json 'calls 'null) "call trace calls" "'null"]
      [,(hash-set frame-json 'from "0x1234") "call trace from" "\"0x1234\""]
      [,(hash-set frame-json 'to "0x1234") "call trace to" "\"0x1234\""]
      [,(hash-set frame-json 'type "call") "call trace type" "\"call\""]
      [,(hash-set frame-json 'type 'null) "call trace type" "'null"]
      [,(hash-set frame-json 'error 5) "call trace error" "5"]
      [,(hash-set frame-json 'revertReason #t) "call trace revert reason" "#t"]
      [,(hash-set frame-json 'value "-1") "call trace value" "\"-1\""]
      [,(hash-set frame-json 'output 7) "call trace output" "7"]
      [,(hash-remove frame-json 'gas) "call trace gas" "'null"]
      [,(hash-remove frame-json 'input) "call trace input" "'null"]
      [,(list frame-json) "call trace" ,(~e (list frame-json))]
      [,(hash-set frame-json 'calls (list (hash-set inner-json 'gas 12)))
       "call trace gas in call trace calls 0" "12"]
      [,(hash-set frame-json 'calls (list inner-json "0x"))
       "call trace in call trace calls 1" "\"0x\""]))

  (test-case "T3: a malformed frame raises exn:fail:web3 naming the field"
    (for ([row (in-list malformed-rows)])
      (match-define (list json what shown) row)
      (check-equal? (message-of (lambda () (jsexpr->call-trace json)))
                    (~a "jsexpr->call-trace: malformed " what " from the node: " shown)
                    what)))

  (test-case "T4: call-trace->jsexpr gives back the node's object, unknown fields kept"
    (define extended (hash-set frame-json 'logs '()))
    (check-eq? (call-trace->jsexpr (jsexpr->call-trace extended)) extended)
    (define trace (jsexpr->call-trace frame-json))
    (check-equal? (jsexpr->call-trace (call-trace->jsexpr trace)) trace))

  (define tracer-wordings
    '(["out of gas" out-of-gas]
      ["out of gas: out of memory" out-of-gas]
      ["out of gas: reach memory limit" out-of-gas]
      ["out of gas: invalid operand" out-of-gas]
      ["invalid opcode: INVALID" invalid-opcode]
      ["invalid opcode" undefined-opcode]
      ["stack underflow" stack-underflow]
      ["Out of stack" stack-overflow]
      ["invalid jump destination" invalid-jump]
      ["return data out of bounds" return-data-out-of-bounds]
      ["precompiled failed" precompile-failure]
      ["write protection" write-protection]
      ["max code size exceeded" code-size]
      ["invalid code: must not begin with 0xef" invalid-code]
      ["contract address collision" address-collision]
      ["execution reverted" #f]
      ["execution reverted: out of gas" #f]))

  (test-case "T5: call-trace-halt reads the tracer's words with message->halt"
    (for ([row (in-list tracer-wordings)])
      (match-define (list words halt) row)
      (check-eq? (call-trace-halt (jsexpr->call-trace (hash-set frame-json 'error words))) halt
                 words))
    (check-false (call-trace-halt (jsexpr->call-trace (hash-remove frame-json 'error)))))

  (define printed
    `([,frame-json
       ,(~a "#<call-trace CALL " anvil-alice " to " caller " gas used 24861 1 call"
            " error \"execution reverted\">")]
      [,inner-json ,(~a "#<call-trace CALL " caller " to " echo " gas used 1064>")]
      [,(hash-set inner-json 'calls (list inner-json inner-json))
       ,(~a "#<call-trace CALL " caller " to " echo " gas used 1064 2 calls>")]
      [,failed-creation-json
       ,(~a "#<call-trace CREATE " anvil-alice " to nothing gas used 30000000"
            " error \"invalid code: must not begin with 0xef\">")]))

  (test-case "T6: a call trace prints its type, addresses, gas used, calls and error"
    (for ([row (in-list printed)])
      (match-define (list json text) row)
      (define trace (jsexpr->call-trace json))
      (for ([mode (in-list (list ~a ~s ~v))])
        (check-equal? (mode trace) text))))

  (test-case "T7: call trace accessors blame their caller"
    (check-blames-here (call-trace-gas-used 42))
    (check-blames-here (call-trace-halt (hasheq)))
    (check-blames-here (call-trace->jsexpr (hasheq))))

  (test-case "T8: every accessor is in web3, web3/trace and the manual"
    (define names
      (append (provided-accessors "call-trace" call-trace-fields)
              '(call-trace? call-trace-halt jsexpr->call-trace call-trace->jsexpr
                transaction-call-trace trace-transaction trace-call
                debug-trace-call debug-trace-transaction)))
    (check-in-web3-and-manual names)
    (define manual (file->string (collection-file-path "scribblings/web3.scrbl" "web3")))
    (check-regexp-match #rx"@defproc/filling\\[\\(call-transaction/trace " manual)
    (define traced (exported-names 'web3/trace))
    (check-not-false (memq 'call-transaction/trace (exported-names 'web3)))
    (for ([name (in-list (cons 'call-transaction/trace names))])
      (check-not-false (memq name traced) (~a name)))
    (define anvil (exported-names 'web3/anvil))
    (check-not-false (memq 'debug-trace-call anvil))
    (check-not-false (memq 'debug-trace-transaction anvil)))

  (define call-tracer (hasheq 'tracer "callTracer"))
  (define tx-hash (filled-hash #\a))

  (define (traced-by method [answer frame-json] #:chain-id [chain-id "0x7a69"])
    (hash method answer "eth_chainId" chain-id "eth_call" (bytes->hex-string (word 1))))

  (test-case "T9: transaction-call-trace sends debug_traceTransaction with the call tracer"
    (define-values (result calls)
      (run-mock (traced-by "debug_traceTransaction") (lambda () (transaction-call-trace tx-hash))))
    (check-equal? result (list (jsexpr->call-trace frame-json)))
    (check-equal? calls `(("debug_traceTransaction" (,tx-hash ,call-tracer)))))

  (test-case "T9: transaction-call-trace reads the node's answer as untrusted"
    (for ([answer (list 'null (list frame-json) (hash-set frame-json 'gas "zz"))])
      (define-values (result _calls)
        (run-mock (traced-by "debug_traceTransaction" answer)
                  (lambda () (transaction-call-trace tx-hash))))
      (match-define (list e) result)
      (check-pred exn:fail:web3? e (~s answer))
      (check-false (exn:fail:contract? e) (~s answer))
      (check-regexp-match #rx"^transaction-call-trace: malformed call trace" (exn-message e))))

  (define storage-key (make-bytes 32 1))

  (define object-rows
    `((() ,anvil-alice ,anvil-bob #hasheq((from . ,anvil-alice) (to . ,anvil-bob)))
      (() #f ,anvil-bob #hasheq((to . ,anvil-bob)))
      (() ,(string-downcase anvil-alice) ,(string-downcase anvil-bob)
          #hasheq((from . ,anvil-alice) (to . ,anvil-bob)))
      ((#:data #"\x60\x80") ,anvil-alice #f #hasheq((from . ,anvil-alice) (data . "0x6080")))
      ((#:data #"" #:value 0) ,anvil-alice ,anvil-bob
                              #hasheq((from . ,anvil-alice) (to . ,anvil-bob)))
      ((#:value 5 #:gas-limit 30000) #f ,anvil-bob
       #hasheq((to . ,anvil-bob) (value . "0x5") (gas . "0x7530")))
      ((#:type 0 #:gas-price 7 #:nonce 0) ,anvil-alice ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (type . "0x0") (gasPrice . "0x7")
               (nonce . "0x0")))
      ((#:access-list ()) ,anvil-alice ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (accessList . ())))
      ((#:value 1 #:data #"\1\2" #:nonce 3 #:gas-limit 30000 #:chain-id 31337 #:type 2
        #:max-priority-fee-per-gas 1 #:max-fee-per-gas 2
        #:access-list ((,(string-downcase anvil-carol) ,storage-key)))
       ,anvil-alice ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (value . "0x1") (data . "0x0102")
               (nonce . "0x3") (gas . "0x7530") (chainId . "0x7a69") (type . "0x2")
               (maxPriorityFeePerGas . "0x1") (maxFeePerGas . "0x2")
               (accessList . (#hasheq((address . ,anvil-carol)
                                      (storageKeys . (,(bytes->hex-string storage-key))))))))))

  (define (sent-params procedure method fields from to)
    (define-values (_result calls)
      (run-mock (traced-by method) (lambda () (call-with-fields procedure fields from to))))
    (for/list ([call (in-list calls)]
               #:when (equal? (first call) method))
      (second call)))

  (test-case "T10: call-transaction/trace sends the object call-transaction sends"
    (for ([row (in-list object-rows)])
      (match-define (list fields from to object) row)
      (define name (~s fields from to))
      (define called (sent-params call-transaction "eth_call" fields from to))
      (define traced (sent-params call-transaction/trace "debug_traceCall" fields from to))
      (check-equal? called (list (list object "latest")) name)
      (check-equal? traced (list (list object "latest" call-tracer)) name)))

  (test-case "T11: call-transaction/trace sends #:block and reads the trace"
    (for ([block (list 5 "pending" "earliest")]
          [sent (list "0x5" "pending" "earliest")])
      (define-values (result calls)
        (run-mock (traced-by "debug_traceCall")
                  (lambda () (call-transaction/trace anvil-alice anvil-bob #:block block))))
      (check-equal? result (list (jsexpr->call-trace frame-json)) (~s block))
      (check-equal? calls
                    `(("debug_traceCall" (,(hasheq 'from anvil-alice 'to anvil-bob) ,sent
                                          ,call-tracer)))
                    (~s block))))

  (test-case "T12: a revert the node raises is exn:fail:web3:rpc:revert, decoded against #:errors"
    (define-values (result _calls)
      (run-mock (traced-by "debug_traceCall" (reverting "debug_traceCall"))
                (lambda ()
                  (call-transaction/trace anvil-alice anvil-bob
                                          #:errors (list "InsufficientBalance(uint256,uint256)")))))
    (match-define (list e) result)
    (check-pred exn:fail:web3:rpc:revert? e)
    (check-equal? (exn:fail:web3:rpc-method e) "debug_traceCall")
    (check-equal? (abi-revert-name (exn->abi-revert e)) "InsufficientBalance")
    (check-equal? (abi-revert-data (exn->abi-revert e)) insufficient-payload))

  (test-case "T12: other errors pass through, and a malformed trace raises exn:fail:web3"
    (define refusal
      (with-handlers ([exn:fail:web3:rpc? values])
        (raise-rpc-error "debug_traceCall" -32601 "Method not found")))
    (define-values (passed _calls)
      (run-mock (traced-by "debug_traceCall" (lambda (_params) (raise refusal)))
                (lambda () (call-transaction/trace anvil-alice anvil-bob))))
    (check-eq? (first passed) refusal)
    (define-values (malformed _more)
      (run-mock (traced-by "debug_traceCall" (hash-set frame-json 'input 5))
                (lambda () (call-transaction/trace anvil-alice anvil-bob))))
    (check-equal? (exn-message (first malformed))
                  "call-transaction/trace: malformed call trace input from the node: 5"))

  (test-case "T13: call-transaction/trace refuses fields of two types and a wrong chain id"
    (define-values (conflict conflict-calls)
      (run-mock (traced-by "debug_traceCall")
                (lambda ()
                  (call-transaction/trace anvil-alice anvil-bob #:type 0 #:access-list '()))))
    (check-equal? (exn-message (first conflict))
                  "call-transaction/trace: #:type 0 takes no #:access-list")
    (check-equal? conflict-calls '())
    (define-values (wrong wrong-calls)
      (run-mock (traced-by "debug_traceCall")
                (lambda () (call-transaction/trace anvil-alice anvil-bob #:chain-id 1))))
    (check-pred exn:fail:web3:fill? (first wrong))
    (check-equal? (method-names wrong-calls) '("eth_chainId")))

  (define wrong-checksum "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD")
  (define checksum-mismatch "mixed case that is not its EIP-55 checksum")

  (define (first-lines e count)
    (take (string-split (exn-message e) "\n") count))

  (define refused-rows
    `([,(lambda () (transaction-call-trace "0xaa"))
       ("transaction-call-trace: not a hash: not 0x and 64 hex digits" "  argument: 1st")]
      [,(lambda () (call-transaction/trace wrong-checksum anvil-bob))
       (,(~a "call-transaction/trace: not an address or a signer: " checksum-mismatch)
        "  argument: 1st")]
      [,(lambda () (call-transaction/trace anvil-alice wrong-checksum))
       (,(~a "call-transaction/trace: not an address: " checksum-mismatch) "  argument: 2nd")]
      [,(lambda () (trace-transaction "0xaa"))
       ("trace-transaction: not a hash: not 0x and 64 hex digits" "  argument: 1st")]))

  (test-case "T14: a wrong hash or address is refused with the short message and no request"
    (for ([row (in-list refused-rows)])
      (match-define (list thunk lines) row)
      (define node (mock-provider (traced-by "debug_traceCall")))
      (define e (parameterize ([current-provider node]) (raised thunk)))
      (check-pred exn:fail:contract:blame? e (first lines))
      (check-equal? (first-lines e 2) lines)
      (check-equal? (mock-provider-calls node) '() (first lines))))

  (test-case "T15: trace-transaction and trace-call send the parity methods"
    (define listed (list (hasheq 'type "call" 'action (hasheq))))
    (define node (mock-provider (hash "trace_transaction" listed
                                      "trace_call" (hasheq 'trace listed 'output "0x"))))
    (check-equal? (trace-transaction tx-hash #:provider node) listed)
    (check-equal? (trace-call (hasheq 'to anvil-bob) '("trace" "stateDiff") 7 #:provider node)
                  (hasheq 'trace listed 'output "0x"))
    (check-equal? (mock-provider-calls node)
                  `(("trace_transaction" (,tx-hash))
                    ("trace_call" (,(hasheq 'to anvil-bob) ("trace" "stateDiff") "0x7")))))

  (test-case "T15: trace-call refuses trace types it does not know, before any request"
    (define node (mock-provider (hash "trace_call" (hasheq))))
    (for ([types (list '() '("trace" "calls") "trace" '(trace))])
      (check-exn exn:fail:contract:blame?
                 (lambda () (trace-call (hasheq 'to anvil-bob) types "latest" #:provider node))
                 (~s types)))
    (check-equal? (mock-provider-calls node) '()))

  (test-case "T15: an answer of the wrong shape raises exn:fail:web3"
    (define node (mock-provider (hash "trace_transaction" (hasheq) "trace_call" '())))
    (check-pred exn:fail:web3? (raised (lambda () (trace-transaction tx-hash #:provider node))))
    (check-pred exn:fail:web3?
                (raised (lambda () (trace-call (hasheq) '("trace") "latest" #:provider node))))))
