#lang racket/base

(module+ test
  (require (only-in racket/contract exn:fail:contract:blame?)
           (only-in racket/file file->string)
           racket/list
           racket/match
           rackunit
           (only-in libsecp256k1 integer->secret-key)
           web3
           (only-in "helpers/call-objects.rkt" call-object-rows conflict-rows)
           "helpers/mock.rkt"
           (only-in "helpers/secrets.rkt" check-no-leak marker-hex marker-integer)
           (only-in "helpers/signers.rkt" watched watched-calls)
           (only-in "modules.rkt" exported-names)
           (only-in "util.rkt" anvil-alice anvil-bob check-blames-here insufficient-balance))

  (define node-estimate "0x5830")

  (define (answers . overrides)
    (apply hash-set* (hash "eth_chainId" "0x7a69" "eth_estimateGas" node-estimate
                           "eth_call" "0x")
           overrides))

  (define (run-estimate fields
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
      (define-values (result calls) (run-estimate fields #:from from #:to to))
      (check-equal? result (list #x5830) name)
      (check-equal? (method-names calls)
                    (if (memq '#:chain-id fields)
                        '("eth_chainId" "eth_estimateGas")
                        '("eth_estimateGas"))
                    name)))

  (test-case "E2: #:block reaches the request"
    (for ([block (list 5 "pending" "earliest" 0)]
          [sent (list "0x5" "pending" "earliest" "0x0")])
      (define-values (_result calls) (run-estimate `(#:block ,block)))
      (check-equal? calls
                    `(("eth_estimateGas" ,(hasheq 'from anvil-alice 'to anvil-bob) ,sent))
                    (format "~s" block))))

  (test-case "E3: a signer gives its address and signs nothing"
    (define signer (watched))
    (define-values (result calls) (run-estimate '() #:from signer))
    (check-equal? result (list #x5830))
    (check-equal? calls `(("eth_estimateGas" ,(hasheq 'from anvil-alice 'to anvil-bob) "latest")))
    (check-equal? (watched-calls signer) '(address)))

  (define malformed #rx"^estimate-transaction: malformed gas estimate from the node: ")

  (test-case "E4: the answer is read as a quantity below 2^64 other than 0"
    (for ([answer (list "0x5208" "0x1" "0xffffffffffffffff")]
          [expected (list 21000 1 (sub1 (expt 2 64)))])
      (define-values (result _calls)
        (run-estimate '() #:answers (answers "eth_estimateGas" answer)))
      (check-equal? result (list expected) answer))
    (for ([row (in-list `((42 ,malformed) ("0x" ,malformed) ("5208" ,malformed)
                          ("0x10000000000000000" ,malformed) (null ,malformed)
                          ("0x0" #rx"^estimate-transaction: the node estimated 0 gas$")))])
      (match-define (list answer message) row)
      (define name (format "~s" answer))
      (define-values (e _calls) (run-estimate '() #:answers (answers "eth_estimateGas" answer)))
      (check-pred exn:fail:web3? e name)
      (check-false (exn:fail:contract? e) name)
      (check-regexp-match message (exn-message e) name)))

  (define payload (bytes-append (bytes #xff) (make-bytes 31 1)))

  (define revert-rows
    `((() ,payload #f ())
      ((#:errors (,insufficient-balance)) ,insufficient-payload "InsufficientBalance" (0 5000))))

  (test-case "E5: a revert raises exn:fail:web3:rpc:revert with its payload, decoded by #:errors"
    (for ([row (in-list revert-rows)])
      (match-define (list fields data error-name arguments) row)
      (define name (format "~s" fields))
      (define-values (e calls)
        (run-estimate fields
                      #:answers (answers "eth_estimateGas"
                                         (reverting "eth_estimateGas" (bytes->hex-string data)))))
      (check-pred exn:fail:web3:rpc:revert? e name)
      (check-equal? (exn:fail:web3:rpc-method e) "eth_estimateGas" name)
      (define revert (exn->abi-revert e))
      (check-equal? (abi-revert-data revert) data name)
      (check-equal? (abi-revert-name revert) error-name name)
      (check-equal? (abi-revert-arguments revert) arguments name)
      (check-equal? (method-names calls) '("eth_estimateGas") name)))

  (test-case "E6: any other error from the node passes through unchanged, its halt readable"
    (define refusal
      (with-handlers ([exn:fail:web3:rpc? values])
        (raise-rpc-error "eth_estimateGas" -32003 "EVM error InvalidFEOpcode")))
    (define-values (e _calls)
      (run-estimate '() #:answers (answers "eth_estimateGas" (lambda (_params) (raise refusal)))))
    (check-eq? e refusal)
    (check-eq? (exn->halt e) 'invalid-opcode))

  (define (check-refused fields refused? message methods)
    (define-values (e calls) (run-estimate fields))
    (define name (format "~s" fields))
    (check-pred refused? e name)
    (check-equal? (exn-message e) message name)
    (check-equal? (method-names calls) methods name))

  (define (conflict? e)
    (and (exn:fail:contract? e) (not (exn:fail:contract:blame? e))))

  (define (wrong-chain? e)
    (and (exn:fail:web3:fill? e) (eq? (exn:fail:web3:fill-reason e) 'chain-id)))

  (test-case "E7: fields of two types are refused before any request"
    (for ([row (in-list conflict-rows)])
      (match-define (list fields text) row)
      (check-refused fields conflict? (string-append "estimate-transaction: " text) '())))

  (test-case "E7: a #:chain-id other than the node's raises exn:fail:web3:fill, estimating nothing"
    (check-refused '(#:chain-id 1)
                   wrong-chain?
                   (string-append "estimate-transaction: wrong chain: #:chain-id is 1,"
                                  " but the node's chain is 31337")
                   '("eth_chainId")))

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
