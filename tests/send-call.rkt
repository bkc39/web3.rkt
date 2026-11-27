#lang racket/base

(module+ test
  (require (only-in racket/contract exn:fail:contract:blame?)
           racket/list
           racket/match
           rackunit
           (only-in libsecp256k1 integer->secret-key)
           web3
           "helpers/mock.rkt"
           (only-in "helpers/secrets.rkt" check-no-leak marker-hex marker-integer)
           (only-in "helpers/signers.rkt" watched watched-calls)
           (only-in "util.rkt" anvil-alice anvil-bob anvil-carol check-blames-here filled-hash
                    insufficient-balance))

  (define returned (make-bytes 32 7))

  (define (answers . overrides)
    (apply hash-set* (hash "eth_chainId" "0x7a69" "eth_call" (bytes->hex-string returned))
           overrides))

  (define (called fields
                  #:from [from anvil-alice]
                  #:to [to anvil-bob]
                  #:answers [handler (answers)])
    (run-mock handler (lambda () (call-with-fields call-transaction fields from to))))

  (define (params-of calls method)
    (for/list ([call (in-list calls)]
               #:when (equal? (first call) method))
      (rest call)))

  (define storage-key (make-bytes 32 1))

  (define object-rows
    `((() ,anvil-alice ,anvil-bob #hasheq((from . ,anvil-alice) (to . ,anvil-bob)))
      (() #f ,anvil-bob #hasheq((to . ,anvil-bob)))
      (() ,(string-downcase anvil-alice) ,(string-downcase anvil-bob)
          #hasheq((from . ,anvil-alice) (to . ,anvil-bob)))
      ((#:data #"\x60\x80") ,anvil-alice #f #hasheq((from . ,anvil-alice) (data . "0x6080")))
      ((#:data #"" #:value 0) ,anvil-alice ,anvil-bob #hasheq((from . ,anvil-alice) (to . ,anvil-bob)))
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

  (test-case "C1: the call carries the fields given and nothing else"
    (for ([row (in-list object-rows)])
      (match-define (list fields from to object) row)
      (define name (format "~s ~s ~s" fields from to))
      (define-values (result calls) (called fields #:from from #:to to))
      (check-equal? result (list returned) name)
      (check-equal? (method-names calls)
                    (if (memq '#:chain-id fields) '("eth_chainId" "eth_call") '("eth_call"))
                    name)
      (check-equal? (params-of calls "eth_call") (list (list object "latest")) name)))

  (test-case "C1: the keywords are send-transaction's and #:block"
    (define-values (required allowed) (procedure-keywords call-transaction))
    (define-values (_send-required send-allowed) (procedure-keywords send-transaction))
    (check-equal? required '())
    (check-equal? allowed (sort (cons '#:block send-allowed) keyword<?)))

  (test-case "C2: a signer gives its address and signs nothing"
    (define signer (watched))
    (define-values (result calls) (called '() #:from signer))
    (check-equal? result (list returned))
    (check-equal? (method-names calls) '("eth_call"))
    (check-equal? (params-of calls "eth_call")
                  (list (list (hasheq 'from anvil-alice 'to anvil-bob) "latest")))
    (check-equal? (watched-calls signer) '(address)))

  (test-case "C3: #:block reaches the request"
    (for ([block (list 5 "pending" "earliest" 0)]
          [sent (list "0x5" "pending" "earliest" "0x0")])
      (define-values (_result calls) (called `(#:block ,block)))
      (check-equal? (params-of calls "eth_call")
                    (list (list (hasheq 'from anvil-alice 'to anvil-bob) sent))
                    (format "~s" block))))

  (test-case "C4: the result is the return data, as bytes"
    (for ([answer (list "0x" "0x2a" (bytes->hex-string returned))]
          [expected (list #"" #"\52" returned)])
      (define-values (result _calls) (called '() #:answers (answers "eth_call" answer)))
      (check-equal? result (list expected) answer))
    (for ([answer (list 42 "0x1" "2a" 'null)])
      (define-values (result _calls) (called '() #:answers (answers "eth_call" answer)))
      (check-pred exn:fail:web3? (first result) (format "~s" answer))))

  (define payload (bytes-append (bytes #xff) (make-bytes 31 1)))

  (test-case "C5: a revert raises exn:fail:web3:rpc:revert carrying its payload"
    (define-values (result calls)
      (called '() #:answers (answers "eth_call" (reverting "eth_call" (bytes->hex-string payload)))))
    (match-define (list e) result)
    (check-pred exn:fail:web3:rpc:revert? e)
    (when (exn:fail:web3:rpc:revert? e)
      (check-equal? (exn:fail:web3:rpc-method e) "eth_call")
      (check-equal? (abi-revert-data (exn->abi-revert e)) payload)
      (check-eq? (exn->abi-revert e) (exn:fail:web3:rpc:revert-revert e)))
    (check-equal? (method-names calls) '("eth_call")))

  (test-case "C5: a revert is decoded against #:errors"
    (define-values (result _calls)
      (called `(#:errors (,insufficient-balance))
              #:answers (answers "eth_call" (reverting "eth_call"))))
    (match-define (list e) result)
    (check-pred exn:fail:web3:rpc:revert? e)
    (when (exn:fail:web3:rpc:revert? e)
      (define revert (exn->abi-revert e))
      (check-equal? (abi-revert-name revert) "InsufficientBalance")
      (check-equal? (abi-revert-arguments revert) '(0 5000))
      (check-equal? (abi-revert-data revert) insufficient-payload)))

  (test-case "C6: any other error from the node passes through unchanged"
    (define refusal
      (with-handlers ([exn:fail:web3:rpc? values])
        (raise-rpc-error "eth_call" -32000 "header not found")))
    (define-values (result _calls)
      (called '() #:answers (answers "eth_call" (lambda (_params) (raise refusal)))))
    (check-eq? (first result) refusal))

  (define conflict-rows
    '(((#:type 0 #:access-list ()) "#:type 0 takes no #:access-list")
      ((#:type 1 #:max-priority-fee-per-gas 1) "#:type 1 takes no #:max-priority-fee-per-gas")
      ((#:type 2 #:gas-price 1) "#:type 2 takes no #:gas-price")
      ((#:gas-price 1 #:max-fee-per-gas 2)
       "#:gas-price and #:max-fee-per-gas are for different transaction types")
      ((#:max-priority-fee-per-gas 3 #:max-fee-per-gas 2)
       "#:max-priority-fee-per-gas is more than #:max-fee-per-gas")))

  (test-case "C7: fields of two types are refused before any request"
    (for ([row (in-list conflict-rows)])
      (match-define (list fields text) row)
      (define-values (result calls) (called fields))
      (match-define (list e) result)
      (check-true (and (exn:fail:contract? e) (not (exn:fail:contract:blame? e))) text)
      (check-equal? (exn-message e) (string-append "call-transaction: " text) text)
      (check-equal? calls '() text)))

  (test-case "C7: a #:chain-id other than the node's raises exn:fail:web3:fill and calls nothing"
    (define-values (result calls) (called '(#:chain-id 1)))
    (match-define (list e) result)
    (check-pred exn:fail:web3:fill? e)
    (check-equal? (exn:fail:web3:fill-reason e) 'chain-id)
    (check-equal? (exn-message e)
                  "call-transaction: wrong chain: #:chain-id is 1, but the node's chain is 31337")
    (check-equal? (method-names calls) '("eth_chainId")))

  (test-case "C8: wrong arguments blame the caller before any request, and show no key"
    (define node (mock-provider (answers)))
    (define wrong-checksum "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD")
    (define calls
      (list (lambda () (call-transaction wrong-checksum anvil-bob #:provider node))
            (lambda () (call-transaction anvil-alice wrong-checksum #:provider node))
            (lambda () (call-transaction (filled-hash #\a) anvil-bob #:provider node))
            (lambda () (call-transaction anvil-alice (filled-hash #\a) #:provider node))
            (lambda () (call-transaction (integer->secret-key marker-integer) anvil-bob
                                         #:provider node))
            (lambda () (call-transaction marker-hex anvil-bob #:provider node))
            (lambda () (call-transaction anvil-alice anvil-bob #:data "0x" #:provider node))
            (lambda () (call-transaction anvil-alice anvil-bob #:block 'latest #:provider node))
            (lambda () (call-transaction anvil-alice anvil-bob #:gas 1 #:provider node))
            (lambda () (call-transaction anvil-alice #:provider node))))
    (for ([call (in-list calls)])
      (check-blames-here (call))
      (check-no-leak (exn-message (with-handlers ([exn:fail? values]) (call)))))
    (define e
      (with-handlers ([exn:fail? values])
        (call-transaction (integer->secret-key marker-integer) anvil-bob #:provider node)))
    (check-regexp-match
     (string-append "^call-transaction: not an address or a signer: neither a string nor a signer\n"
                    "  argument: 1st\n  given: a secret key\n")
     (exn-message e))
    (check-equal? (mock-provider-calls node) '())))
