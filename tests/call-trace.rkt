#lang racket/base

(module+ test
  (require (only-in racket/contract exn:fail:contract:blame?)
           (only-in racket/file file->string)
           racket/format
           racket/list
           racket/match
           racket/string
           rackunit
           web3
           (prefix-in anvil: (only-in web3/anvil debug-trace-call debug-trace-transaction))
           (prefix-in trace: (only-in web3/trace debug-trace-call debug-trace-transaction))
           (only-in (submod web3/eth/call-trace internal) call-trace-fields)
           (only-in (submod web3/trace internal) call-tracer-options)
           (only-in "helpers/call-objects.rkt" call-object-rows)
           "helpers/mock.rkt"
           (only-in "helpers/records.rkt"
                    check-fields check-in-web3-and-manual message-of provided-accessors)
           (only-in "modules.rkt" exported-names import-closure module-name)
           (only-in "util.rkt" anvil-alice anvil-bob check-blames-here filled-hash
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

  (define (nested-in depth frame)
    (for/fold ([nested frame])
              ([_level (in-range depth)])
      (hash-set inner-json 'calls (list nested))))

  (define (path-length trace)
    (match (call-trace-calls trace)
      ['() 1]
      [(list inner) (add1 (path-length inner))]))

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

  (test-case "T2: nested calls are call traces"
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

  (define refused-for-depth (hash-set inner-json 'error "max call depth exceeded"))

  (define (deepest-frame trace)
    (match (call-trace-calls trace)
      ['() trace]
      [(list inner) (deepest-frame inner)]))

  (test-case "T2: a trace 1026 frames deep, the last a call the EVM refuses for depth, reads"
    (define trace (jsexpr->call-trace (nested-in 1025 refused-for-depth)))
    (check-equal? (path-length trace) 1026)
    (check-eq? (call-trace-halt (deepest-frame trace)) 'call-depth))

  (test-case "T2: a trace deeper than 1026 frames is refused for its depth"
    (define e (raised (lambda () (jsexpr->call-trace (nested-in 1026 refused-for-depth)))))
    (check-pred exn:fail:web3? e)
    (check-equal? (exn-message e)
                  (~a "jsexpr->call-trace: malformed call trace calls 0 in call trace calls 0"
                      " in call trace calls 0 in call trace calls 0, 1025 levels deep"
                      " from the node: nested deeper than 1025 levels")))

  (test-case "T2: a bad field deep in a trace names the field and the innermost levels only"
    (define message
      (message-of (lambda () (jsexpr->call-trace (nested-in 1000 (hash-set inner-json 'gas 12))))))
    (check-equal? message
                  (~a "jsexpr->call-trace: malformed call trace gas in call trace calls 0"
                      " in call trace calls 0 in call trace calls 0, 1000 levels deep"
                      " from the node: 12"))
    (check-true (< (string-length message) 200)))

  (test-case "T2: a frame of any type the node names reads"
    (for ([type (in-list '("CALL" "STATICCALL" "DELEGATECALL" "CALLCODE" "CREATE" "CREATE2"
                           "SELFDESTRUCT" "EOFCREATE" "call"))])
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
      [,(hash-set frame-json 'type 5) "call trace type" "5"]
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
       "call trace in call trace calls 1" "\"0x\""]
      [,(nested-in 3 (hash-set inner-json 'gas 12))
       "call trace gas in call trace calls 0 in call trace calls 0 in call trace calls 0"
       "12"]
      [,(nested-in 4 (hash-set inner-json 'gas 12))
       ,(~a "call trace gas in call trace calls 0 in call trace calls 0 in call trace calls 0,"
            " 4 levels deep")
       "12"]))

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

  (define documented
    (append (provided-accessors "call-trace" call-trace-fields)
            '(call-trace? call-trace-halt jsexpr->call-trace call-trace->jsexpr
              transaction-call-trace trace-transaction trace-call
              debug-trace-call debug-trace-transaction)))

  (define exported-rows
    `([web3/trace ,(cons 'call-transaction/trace documented)]
      [web3 (call-transaction/trace)]
      [web3/anvil (debug-trace-call debug-trace-transaction)]))

  (test-case "T8: every accessor is in web3 and the manual"
    (check-in-web3-and-manual documented))

  (test-case "T8: call-transaction/trace's manual entry lists the fill keywords"
    (define manual (file->string (collection-file-path "scribblings/web3.scrbl" "web3")))
    (check-regexp-match #rx"@defproc/filling\\[\\(call-transaction/trace " manual))

  (test-case "T8: web3/trace, web3 and web3/anvil export their tracing names"
    (for ([row (in-list exported-rows)])
      (match-define (list exporter names) row)
      (for ([name (in-list names)])
        (check-not-false (memq name (exported-names exporter)) (~a exporter " " name)))))

  (test-case "T8: web3/anvil's tracing methods are web3/trace's"
    (check-true (free-identifier=? #'anvil:debug-trace-call #'trace:debug-trace-call))
    (check-true (free-identifier=? #'anvil:debug-trace-transaction
                                   #'trace:debug-trace-transaction)))

  (test-case "T8: web3/anvil loads neither web3/trace nor the send stack"
    (define loaded (filter-map module-name (import-closure 'web3/anvil)))
    (for ([unloaded (in-list '("web3/trace" "web3/private/send" "web3/send" "web3/signer"
                               "web3/halt"))])
      (check-false (member unloaded loaded) unloaded)))

  (define tx-hash (filled-hash #\a))

  (define (traced-by method [answer frame-json] #:chain-id [chain-id "0x7a69"])
    (hash method answer "eth_chainId" chain-id "eth_call" (bytes->hex-string (word 1))))

  (define alice-to-bob
    (make-keyword-procedure
     (lambda (keywords arguments)
       (lambda ()
         (keyword-apply call-transaction/trace keywords arguments (list anvil-alice anvil-bob))))))

  (test-case "T9: transaction-call-trace sends debug_traceTransaction with the call tracer"
    (define-values (result calls)
      (run-mock (traced-by "debug_traceTransaction") (lambda () (transaction-call-trace tx-hash))))
    (check-equal? result (list (jsexpr->call-trace frame-json)))
    (check-equal? calls `(("debug_traceTransaction" ,tx-hash ,call-tracer-options))))

  (define untrusted-rows
    `([null #rx"^debug_traceTransaction: unexpected result 'null$"]
      [,(list frame-json) #rx"^debug_traceTransaction: unexpected result "]
      [,(hash-set frame-json 'gas "zz")
       #rx"^transaction-call-trace: malformed call trace gas from the node: \"zz\"$"]))

  (test-case "T9: transaction-call-trace reads the node's answer as untrusted"
    (for ([row (in-list untrusted-rows)])
      (match-define (list answer message) row)
      (define-values (e _calls)
        (run-mock (traced-by "debug_traceTransaction" answer)
                  (lambda () (transaction-call-trace tx-hash))))
      (check-pred exn:fail:web3? e (~s answer))
      (check-false (exn:fail:contract? e) (~s answer))
      (check-regexp-match message (exn-message e))))

  (define (sent-params procedure method fields from to)
    (define-values (_result calls)
      (run-mock (traced-by method) (lambda () (call-with-fields procedure fields from to))))
    (for/list ([call (in-list calls)]
               #:when (equal? (first call) method))
      (rest call)))

  (test-case "T10: call-transaction/trace sends the object call-transaction sends"
    (for ([row (in-list call-object-rows)])
      (match-define (list fields from to object) row)
      (define name (~s fields from to))
      (define called (sent-params call-transaction "eth_call" fields from to))
      (define traced (sent-params call-transaction/trace "debug_traceCall" fields from to))
      (check-equal? called (list (list object "latest")) name)
      (check-equal? traced (list (list object "latest" call-tracer-options)) name)))

  (test-case "T11: call-transaction/trace sends #:block and reads the trace"
    (for ([block (list 5 "pending" "earliest")]
          [sent (list "0x5" "pending" "earliest")])
      (define-values (result calls)
        (run-mock (traced-by "debug_traceCall") (alice-to-bob #:block block)))
      (check-equal? result (list (jsexpr->call-trace frame-json)) (~s block))
      (check-equal? calls
                    `(("debug_traceCall" ,(hasheq 'from anvil-alice 'to anvil-bob) ,sent
                                         ,call-tracer-options))
                    (~s block))))

  (test-case "T12: a revert the node raises is exn:fail:web3:rpc:revert, decoded against #:errors"
    (define-values (e _calls)
      (run-mock (traced-by "debug_traceCall" (reverting "debug_traceCall"))
                (alice-to-bob #:errors (list "InsufficientBalance(uint256,uint256)"))))
    (check-pred exn:fail:web3:rpc:revert? e)
    (check-equal? (exn:fail:web3:rpc-method e) "debug_traceCall")
    (check-equal? (abi-revert-name (exn->abi-revert e)) "InsufficientBalance")
    (check-equal? (abi-revert-data (exn->abi-revert e)) insufficient-payload))

  (test-case "T12: an error the node raises that is not a revert passes through"
    (define refusal
      (with-handlers ([exn:fail:web3:rpc? values])
        (raise-rpc-error "debug_traceCall" -32601 "Method not found")))
    (define-values (passed _calls)
      (run-mock (traced-by "debug_traceCall" (lambda (_params) (raise refusal))) (alice-to-bob)))
    (check-eq? passed refusal))

  (test-case "T12: a malformed trace raises exn:fail:web3 under call-transaction/trace"
    (define-values (malformed _calls)
      (run-mock (traced-by "debug_traceCall" (hash-set frame-json 'input 5)) (alice-to-bob)))
    (check-equal? (exn-message malformed)
                  "call-transaction/trace: malformed call trace input from the node: 5"))

  (test-case "T13: call-transaction/trace refuses fields of two types before any request"
    (define-values (conflict calls)
      (run-mock (traced-by "debug_traceCall") (alice-to-bob #:type 0 #:access-list '())))
    (check-equal? (exn-message conflict) "call-transaction/trace: #:type 0 takes no #:access-list")
    (check-equal? calls '()))

  (test-case "T13: call-transaction/trace refuses a chain id that is not the node's"
    (define-values (wrong calls)
      (run-mock (traced-by "debug_traceCall") (alice-to-bob #:chain-id 1)))
    (check-pred exn:fail:web3:fill? wrong)
    (check-equal? (method-names calls) '("eth_chainId")))

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

  (define parity-frames (list (hasheq 'type "call" 'action (hasheq))))

  (test-case "T15: trace-transaction sends trace_transaction"
    (define node (mock-provider (hash "trace_transaction" parity-frames)))
    (check-equal? (trace-transaction tx-hash #:provider node) parity-frames)
    (check-equal? (mock-provider-calls node) `(("trace_transaction" ,tx-hash))))

  (test-case "T15: trace-call sends trace_call with its types and block"
    (define answer (hasheq 'trace parity-frames 'output "0x"))
    (define node (mock-provider (hash "trace_call" answer)))
    (check-equal? (trace-call (hasheq 'to anvil-bob) '("trace" "stateDiff") 7 #:provider node)
                  answer)
    (check-equal? (mock-provider-calls node)
                  `(("trace_call" ,(hasheq 'to anvil-bob) ("trace" "stateDiff") "0x7"))))

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
