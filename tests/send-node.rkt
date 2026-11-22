#lang racket/base

(module+ test
  (require (only-in racket/contract exn:fail:contract:blame?)
           racket/list
           racket/match
           rackunit
           web3
           "helpers/mock.rkt"
           (only-in "helpers/signers.rkt" signer0)
           (only-in "util.rkt" anvil-alice anvil-bob anvil-carol check-blames-here filled-hash
                    insufficient-balance))

  (define storage-key (make-bytes 32 1))
  (define storage-key-hex (bytes->hex-string storage-key))

  (define carol-access-list
    `(#hasheq((address . ,anvil-carol) (storageKeys . (,storage-key-hex)))))

  (define send-rows
    `(((#:value 5) ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x") (value . "0x5")
               (gas . "0x5208"))
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x") (value . "0x5"))
       ("eth_estimateGas" "eth_sendTransaction"))
      ((#:gas-limit 30000) ,(string-downcase anvil-bob)
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x") (gas . "0x7530"))
       #f
       ("eth_sendTransaction"))
      ((#:chain-id 31337 #:gas-limit 21000) ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x") (chainId . "0x7a69")
               (gas . "0x5208"))
       #f
       ("eth_chainId" "eth_sendTransaction"))
      ((#:access-list ()) ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x") (accessList . ())
               (gas . "0x5208"))
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x"))
       ("eth_estimateGas" "eth_sendTransaction"))
      ((#:data #"\x60\x80") #f
       #hasheq((from . ,anvil-alice) (data . "0x6080") (gas . "0x5208"))
       #hasheq((from . ,anvil-alice) (data . "0x6080"))
       ("eth_estimateGas" "eth_sendTransaction"))
      ((#:type 0 #:nonce 0 #:gas-price 7 #:gas-limit 21000) ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x") (type . "0x0")
               (nonce . "0x0") (gasPrice . "0x7") (gas . "0x5208"))
       #f
       ("eth_sendTransaction"))
      ((#:type 0 #:gas-limit 21000) ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x") (type . "0x0")
               (gasPrice . "0x9") (gas . "0x5208"))
       #f
       ("eth_gasPrice" "eth_sendTransaction"))
      ((#:type 0 #:nonce 0) ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x") (type . "0x0")
               (nonce . "0x0") (gasPrice . "0x9") (gas . "0x5208"))
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x"))
       ("eth_gasPrice" "eth_estimateGas" "eth_sendTransaction"))
      ((#:type 1) ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x") (type . "0x1")
               (gasPrice . "0x9") (accessList . ()) (gas . "0x5208"))
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x"))
       ("eth_gasPrice" "eth_estimateGas" "eth_sendTransaction"))
      ((#:gas-price 7) ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x") (gasPrice . "0x7")
               (gas . "0x5208"))
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x"))
       ("eth_estimateGas" "eth_sendTransaction"))
      ((#:gas-price 7 #:access-list ()) ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x") (gasPrice . "0x7")
               (accessList . ()) (gas . "0x5208"))
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x"))
       ("eth_estimateGas" "eth_sendTransaction"))
      ((#:type 2) ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x") (type . "0x2")
               (gas . "0x5208"))
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x"))
       ("eth_estimateGas" "eth_sendTransaction"))
      ((#:max-priority-fee-per-gas 2 #:value 5) ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x") (value . "0x5")
               (maxPriorityFeePerGas . "0x2") (gas . "0x5208"))
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x") (value . "0x5"))
       ("eth_estimateGas" "eth_sendTransaction"))
      ((#:max-fee-per-gas 9) ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x") (maxFeePerGas . "0x9")
               (gas . "0x5208"))
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x"))
       ("eth_estimateGas" "eth_sendTransaction"))
      ((#:nonce 3 #:type 2 #:max-priority-fee-per-gas 1 #:max-fee-per-gas 2 #:data #"\1\2"
        #:access-list ((,(string-downcase anvil-carol) ,storage-key)))
       ,anvil-bob
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x0102") (nonce . "0x3")
               (type . "0x2") (maxPriorityFeePerGas . "0x1") (maxFeePerGas . "0x2")
               (accessList . ,carol-access-list) (gas . "0x5208"))
       #hasheq((from . ,anvil-alice) (to . ,anvil-bob) (data . "0x0102")
               (accessList . ,carol-access-list))
       ("eth_estimateGas" "eth_sendTransaction"))))

  (define (node-answers . overrides)
    (apply hash-set* (hash-set* (sending) "eth_chainId" "0x7a69" "eth_gasPrice" "0x9") overrides))

  (define (sent procedure fields
                #:from [from (string-downcase anvil-alice)]
                #:to [to anvil-bob]
                #:answers [answers (node-answers)])
    (run-mock answers (lambda () (call-with-fields procedure fields from to))))

  (define (params-of calls method)
    (for/list ([call (in-list calls)]
               #:when (equal? (first call) method))
      (rest call)))

  (test-case "S1: the fields given are sent, and the gas is estimated without type, nonce or fees"
    (for ([row (in-list send-rows)])
      (match-define (list fields to object estimated methods) row)
      (define name (format "~s" fields))
      (define-values (result calls) (sent send-transaction fields #:to to))
      (check-equal? result (list tx-hash) name)
      (check-equal? (method-names calls) methods name)
      (check-equal? (params-of calls "eth_sendTransaction") (list (list object)) name)
      (check-equal? (params-of calls "eth_estimateGas") (if estimated (list (list estimated)) '())
                    name)
      (define-values (receipt receipt-calls) (sent send-transaction/receipt fields #:to to))
      (check-eq? (receipt->jsexpr (first receipt)) mined name)
      (check-equal? (method-names receipt-calls) (append methods '("eth_getTransactionReceipt"))
                    name)))

  (test-case "S1: the keywords are fill-transaction's"
    (define (keywords procedure)
      (call-with-values (lambda () (procedure-keywords procedure)) list))
    (check-equal? (keywords send-transaction) (keywords fill-transaction))
    (check-equal? (keywords send-transaction/receipt) (keywords fill-transaction)))

  (test-case "S2: fields of two types are refused before any request"
    (for* ([procedure (list send-transaction send-transaction/receipt)]
           [row (in-list '(((#:type 0 #:max-fee-per-gas 1) "#:type 0 takes no #:max-fee-per-gas")
                           ((#:max-priority-fee-per-gas 3 #:max-fee-per-gas 2)
                            "#:max-priority-fee-per-gas is more than #:max-fee-per-gas")))])
      (match-define (list fields text) row)
      (define name (format "~a ~s" (object-name procedure) fields))
      (define-values (e calls) (sent procedure fields))
      (check-true (and (exn:fail:contract? e) (not (exn:fail:contract:blame? e))) name)
      (check-equal? (exn-message e) (format "~a: ~a" (object-name procedure) text) name)
      (check-equal? calls '() name)))

  (test-case "S3: a #:chain-id other than the node's raises exn:fail:web3:fill and sends nothing"
    (define-values (e calls) (sent send-transaction '(#:chain-id 1)))
    (check-pred exn:fail:web3:fill? e)
    (check-equal? (exn:fail:web3:fill-reason e) 'chain-id)
    (check-equal? (exn-message e)
                  "send-transaction: wrong chain: #:chain-id is 1, but the node's chain is 31337")
    (check-equal? (method-names calls) '("eth_chainId")))

  (test-case "S4: a reverting estimate raises the revert decoded against #:errors"
    (define-values (e calls)
      (sent send-transaction `(#:errors (,insufficient-balance))
            #:answers (node-answers "eth_estimateGas" (reverting "eth_estimateGas"))))
    (check-pred exn:fail:web3:rpc:revert? e)
    (check-equal? (abi-revert-arguments (exn:fail:web3:rpc:revert-revert e)) '(0 5000))
    (check-equal? (method-names calls) '("eth_estimateGas")))

  (test-case "S5: the node's answer is the transaction hash, in lowercase"
    (define-values (upper _upper-calls)
      (sent send-transaction '()
            #:answers (node-answers "eth_sendTransaction" (filled-hash #\A))))
    (check-equal? upper (list (filled-hash #\a)))
    (for ([answer (list "0x12" 42 'null (string-append (filled-hash #\a) "aa"))])
      (define-values (e _calls)
        (sent send-transaction/receipt '() #:answers (node-answers "eth_sendTransaction" answer)))
      (check-pred exn:fail:web3? e (format "~s" answer))
      (check-equal? (exn-message e)
                    (format "send-transaction/receipt: malformed transaction hash from the node: ~e"
                            answer))))

  (test-case "S6: a failed transaction raises with its receipt, replayed without nonce or fees"
    (define-values (e calls)
      (sent send-transaction/receipt
            `(#:value 5 #:nonce 3 #:max-priority-fee-per-gas 1 #:max-fee-per-gas 9
              #:errors (,insufficient-balance))
            #:answers (node-answers "eth_getTransactionReceipt" failed
                                    "eth_call" (reverting "eth_call"))))
    (check-pred exn:fail:web3:transaction? e)
    (check-eq? (receipt->jsexpr (exn:fail:web3:transaction-receipt e)) failed)
    (check-equal? (abi-revert-arguments (exn:fail:web3:transaction-revert e)) '(0 5000))
    (check-equal? (params-of calls "eth_call")
                  (list (list (hasheq 'from anvil-alice 'to anvil-bob 'data "0x" 'value "0x5"
                                      'gas "0x5208")
                              "0x5"))))

  (test-case "S7: every request goes to #:provider, never to current-provider"
    (define trap (mock-provider (hash)))
    (parameterize ([current-provider trap])
      (define node (mock-provider (node-answers)))
      (define receipt
        (send-transaction/receipt anvil-alice anvil-bob #:type 1 #:chain-id 31337 #:provider node))
      (check-eq? (receipt->jsexpr receipt) mined)
      (check-equal? (method-names (mock-provider-calls node))
                    '("eth_chainId" "eth_gasPrice" "eth_estimateGas" "eth_sendTransaction"
                      "eth_getTransactionReceipt"))
      (define failing
        (mock-provider (node-answers "eth_getTransactionReceipt" failed
                                     "eth_call" (reverting "eth_call"))))
      (define e
        (with-handlers ([exn:fail? values])
          (send-transaction/receipt anvil-alice anvil-bob #:provider failing)))
      (check-pred exn:fail:web3:transaction? e)
      (check-equal? (method-names (mock-provider-calls failing))
                    '("eth_estimateGas" "eth_sendTransaction" "eth_getTransactionReceipt"
                      "eth_call"))
      (check-equal? (mock-provider-calls trap) '())))

  (test-case "S8: wrong arguments blame the caller"
    (parameterize ([current-provider (mock-provider (node-answers))])
      (for ([procedure (list send-transaction send-transaction/receipt)])
        (define e (with-handlers ([exn:fail? values]) (procedure signer0 anvil-bob)))
        (check-pred exn:fail:contract:blame? e)
        (check-regexp-match #rx"expected: an address" (exn-message e))
        (check-blames-here (procedure 42 anvil-bob))
        (check-blames-here (procedure anvil-alice "bob"))
        (check-blames-here (procedure anvil-alice))
        (check-blames-here (procedure anvil-alice anvil-bob #:value -1))
        (check-blames-here (procedure anvil-alice anvil-bob #:data "0x"))
        (check-blames-here (procedure anvil-alice anvil-bob #:gas 1)))))

  (test-case "a mixed-case address with a wrong checksum is refused before any request"
    (define wrong-checksum "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD")
    (define node (mock-provider (node-answers)))
    (for* ([procedure (list send-transaction send-transaction/receipt fill-transaction)]
           [call (list (lambda () (procedure wrong-checksum anvil-bob #:provider node))
                       (lambda () (procedure anvil-alice wrong-checksum #:provider node))
                       (lambda () (procedure anvil-alice anvil-bob #:type 1 #:provider node
                                             #:access-list `((,wrong-checksum)))))])
      (check-blames-here (call)))
    (check-equal? (mock-provider-calls node) '()))

  (test-case "S10: eth-send-transaction returns the node's hash in lowercase"
    (define (answered answer)
      (with-handlers ([exn:fail:web3? exn-message])
        (eth-send-transaction (hasheq 'from anvil-alice)
                              #:provider (mock-provider (hash "eth_sendTransaction" answer)))))
    (check-equal? (answered (filled-hash #\A)) (filled-hash #\a))
    (check-equal? (answered "0x12")
                  "eth_sendTransaction: malformed transaction hash from the node: \"0x12\"")))
