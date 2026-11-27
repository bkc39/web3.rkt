#lang racket/base

(module+ test
  (require (only-in racket/contract exn:fail:contract:blame?)
           (only-in racket/file file->string)
           racket/list
           racket/match
           rackunit
           (only-in libsecp256k1 integer->secret-key)
           web3
           (only-in "helpers/call-objects.rkt" call-object-rows)
           "helpers/mock.rkt"
           (only-in "helpers/secrets.rkt" check-no-leak marker-hex marker-integer)
           (only-in "helpers/signers.rkt" watched watched-calls)
           (only-in "modules.rkt" exported-names)
           (only-in "util.rkt" anvil-alice anvil-bob check-blames-here insufficient-balance))

  (define estimate "0x5830")

  (define (answers . overrides)
    (apply hash-set* (hash "eth_chainId" "0x7a69" "eth_estimateGas" estimate
                           "eth_call" "0x")
           overrides))

  (define (estimated fields
                     #:from [from anvil-alice]
                     #:to [to anvil-bob]
                     #:answers [handler (answers)])
    (run-mock handler (lambda () (call-with-fields estimate-transaction fields from to))))

  (define (sent-params procedure method fields from to)
    (define-values (_result calls)
      (run-mock (answers) (lambda () (call-with-fields procedure fields from to))))
    (for/list ([call (in-list calls)]
               #:when (equal? (first call) method))
      (rest call)))

  (test-case "E1: the estimate sends the object call-transaction sends"
    (for ([row (in-list call-object-rows)])
      (match-define (list fields from to object) row)
      (define name (format "~s ~s ~s" fields from to))
      (define called (sent-params call-transaction "eth_call" fields from to))
      (define sent (sent-params estimate-transaction "eth_estimateGas" fields from to))
      (check-equal? sent (list (list object "latest")) name)
      (check-equal? sent called name)))

  (test-case "E1: the estimate asks only eth_estimateGas, and eth_chainId for a #:chain-id"
    (for ([row (in-list call-object-rows)])
      (match-define (list fields from to _object) row)
      (define name (format "~s ~s ~s" fields from to))
      (define-values (result calls) (estimated fields #:from from #:to to))
      (check-equal? result (list #x5830) name)
      (check-equal? (method-names calls)
                    (if (memq '#:chain-id fields)
                        '("eth_chainId" "eth_estimateGas")
                        '("eth_estimateGas"))
                    name)))

  (test-case "E2: #:block reaches the request"
    (for ([block (list 5 "pending" "earliest" 0)]
          [sent (list "0x5" "pending" "earliest" "0x0")])
      (define-values (_result calls) (estimated `(#:block ,block)))
      (check-equal? calls
                    `(("eth_estimateGas" ,(hasheq 'from anvil-alice 'to anvil-bob) ,sent))
                    (format "~s" block))))

  (test-case "E3: a signer gives its address and signs nothing"
    (define signer (watched))
    (define-values (result calls) (estimated '() #:from signer))
    (check-equal? result (list #x5830))
    (check-equal? calls `(("eth_estimateGas" ,(hasheq 'from anvil-alice 'to anvil-bob) "latest")))
    (check-equal? (watched-calls signer) '(address)))

  (test-case "E4: the answer is read as a quantity below 2^64"
    (for ([answer (list "0x5208" "0x0" "0xffffffffffffffff")]
          [expected (list 21000 0 (sub1 (expt 2 64)))])
      (define-values (result _calls) (estimated '() #:answers (answers "eth_estimateGas" answer)))
      (check-equal? result (list expected) answer))
    (for ([answer (list 42 "0x" "5208" "0x10000000000000000" 'null)])
      (define-values (e _calls) (estimated '() #:answers (answers "eth_estimateGas" answer)))
      (check-pred exn:fail:web3? e (format "~s" answer))
      (check-false (exn:fail:contract? e) (format "~s" answer))
      (check-regexp-match #rx"^estimate-transaction: malformed gas estimate from the node: "
                          (exn-message e)
                          (format "~s" answer))))

  (define payload (bytes-append (bytes #xff) (make-bytes 31 1)))

  (test-case "E5: a revert raises exn:fail:web3:rpc:revert carrying its payload"
    (define-values (e calls)
      (estimated '()
                 #:answers (answers "eth_estimateGas"
                                    (reverting "eth_estimateGas" (bytes->hex-string payload)))))
    (check-pred exn:fail:web3:rpc:revert? e)
    (check-equal? (exn:fail:web3:rpc-method e) "eth_estimateGas")
    (check-equal? (abi-revert-data (exn->abi-revert e)) payload)
    (check-equal? (method-names calls) '("eth_estimateGas")))

  (test-case "E5: a revert is decoded against #:errors"
    (define-values (e _calls)
      (estimated `(#:errors (,insufficient-balance))
                 #:answers (answers "eth_estimateGas" (reverting "eth_estimateGas"))))
    (check-pred exn:fail:web3:rpc:revert? e)
    (define revert (exn->abi-revert e))
    (check-equal? (abi-revert-name revert) "InsufficientBalance")
    (check-equal? (abi-revert-arguments revert) '(0 5000))
    (check-equal? (abi-revert-data revert) insufficient-payload))

  (test-case "E6: any other error from the node passes through unchanged, its halt readable"
    (define refusal
      (with-handlers ([exn:fail:web3:rpc? values])
        (raise-rpc-error "eth_estimateGas" -32003 "EVM error InvalidFEOpcode")))
    (define-values (e _calls)
      (estimated '() #:answers (answers "eth_estimateGas" (lambda (_params) (raise refusal)))))
    (check-eq? e refusal)
    (check-eq? (exn->halt e) 'invalid-opcode))

  (define conflict-rows
    '(((#:type 0 #:access-list ()) "#:type 0 takes no #:access-list")
      ((#:type 1 #:max-priority-fee-per-gas 1) "#:type 1 takes no #:max-priority-fee-per-gas")
      ((#:type 2 #:gas-price 1) "#:type 2 takes no #:gas-price")
      ((#:gas-price 1 #:max-fee-per-gas 2)
       "#:gas-price and #:max-fee-per-gas are for different transaction types")
      ((#:max-priority-fee-per-gas 3 #:max-fee-per-gas 2)
       "#:max-priority-fee-per-gas is more than #:max-fee-per-gas")))

  (test-case "E7: fields of two types are refused before any request"
    (for ([row (in-list conflict-rows)])
      (match-define (list fields text) row)
      (define-values (e calls) (estimated fields))
      (check-true (and (exn:fail:contract? e) (not (exn:fail:contract:blame? e))) text)
      (check-equal? (exn-message e) (string-append "estimate-transaction: " text) text)
      (check-equal? calls '() text)))

  (test-case "E7: a #:chain-id other than the node's raises exn:fail:web3:fill, estimating nothing"
    (define-values (e calls) (estimated '(#:chain-id 1)))
    (check-pred exn:fail:web3:fill? e)
    (check-equal? (exn:fail:web3:fill-reason e) 'chain-id)
    (check-equal? (exn-message e)
                  (string-append "estimate-transaction: wrong chain: #:chain-id is 1,"
                                 " but the node's chain is 31337"))
    (check-equal? (method-names calls) '("eth_chainId")))

  (test-case "E8: wrong arguments blame the caller before any request, and show no key"
    (define node (mock-provider (answers)))
    (define wrong-checksum "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD")
    (define refused-rows
      `(((,wrong-checksum ,anvil-bob) ())
        ((,anvil-alice ,wrong-checksum) ())
        ((,(integer->secret-key marker-integer) ,anvil-bob) ())
        ((,marker-hex ,anvil-bob) ())
        ((,anvil-alice ,anvil-bob) (#:data "0x"))
        ((,anvil-alice ,anvil-bob) (#:block latest))
        ((,anvil-alice ,anvil-bob) (#:gas 1))
        ((,anvil-alice) ())))
    (for ([row (in-list refused-rows)])
      (match-define (list positional fields) row)
      (define (estimate-it)
        (apply call-with-fields estimate-transaction (list* '#:provider node fields) positional))
      (check-blames-here (estimate-it))
      (check-no-leak (exn-message (with-handlers ([exn:fail? values]) (estimate-it)))))
    (check-equal? (mock-provider-calls node) '()))

  (test-case "E9: web3/send and web3 export estimate-transaction, and the manual lists it"
    (check-not-false (memq 'estimate-transaction (exported-names 'web3/send)))
    (check-not-false (memq 'estimate-transaction (exported-names 'web3)))
    (define manual (file->string (collection-file-path "scribblings/web3.scrbl" "web3")))
    (check-regexp-match #rx"@defproc/filling\\[\\(estimate-transaction " manual)))
