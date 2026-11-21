#lang racket/base

(require racket/list
         racket/match
         web3
         (only-in "../util.rkt" filled-hash insufficient-balance word))

(provide base-block
         call-with-fields
         eip1559-hash
         eip1559-raw
         failed
         filling
         hex-word
         insufficient-payload
         legacy-block
         legacy-hash
         legacy-raw
         method-names
         mined
         raw-answer
         raw-transactions
         recording
         recording-calls
         reverting
         run-mock
         sending
         token
         tx-hash)

(define token "0x5FbDB2315678afecb367f032d93F642f64180aa3")
(define tx-hash (filled-hash #\a))
(define insufficient-payload (encode-call insufficient-balance 0 5000))
(define mined (hasheq 'status "0x1" 'blockNumber "0x2" 'transactionHash tx-hash 'logs '()))
(define failed (hasheq 'status "0x0" 'blockNumber "0x5" 'transactionHash tx-hash 'logs '()))

(define (method-names calls)
  (map first calls))

(define (hex-word n)
  (bytes->hex-string (word n)))

(define ((reverting method [data (bytes->hex-string insufficient-payload)]) _params)
  (raise-rpc-error method 3 "execution reverted: custom error" data))

(define (run-mock handler thunk)
  (define node (mock-provider handler))
  (define result
    (parameterize ([current-provider node])
      (with-handlers ([(lambda (_) #t) values])
        (call-with-values thunk list))))
  (values result (mock-provider-calls node)))

(define (sending #:estimate [estimate "0x5208"]
                 #:receipt [receipt mined]
                 #:replay [replay "0x"])
  (hash "eth_estimateGas" estimate
        "eth_sendTransaction" tx-hash
        "eth_getTransactionReceipt" receipt
        "eth_call" replay))

(define eip1559-raw
  (string-append
   "0x02f874827a6980843b9aca0084b2d05e008252089470997970c51812dc3a010c7d01b50e0d17dc79c88814d1120d"
   "7b16000080c001a03042fedbea7eedc8bda3fe469c6ba2d9d8bbc7763af08d032f5879b8bef5c562a03eafe18564"
   "baaf8691e594bdbe91dbe1647a0fe63280dde737d650b5c3409065"))
(define eip1559-hash "0x353911be23e52e6b62f32b2323be63d06541c4ab17382cde9ff87b3174c3c9f6")
(define legacy-raw
  (string-append
   "0xf86d8084773594008252089470997970c51812dc3a010c7d01b50e0d17dc79c88814d1120d7b1600008082f4f6"
   "a09bbc8b78dfea251b6cb854005da11c4461d73084136cd5b0f92c1335e6581bd4a06e2c0241dcc2a122723d1a15"
   "33d22335fcd5ea8fa47c5db754dbfc7502bb2bcf"))
(define legacy-hash "0x50323a6e847d2315723b7b38b90f503ba441b9f0722a56f2ca5448a2d82f0d46")

(define base-block (hasheq 'number "0x1" 'baseFeePerGas "0x3b9aca00"))
(define legacy-block (hasheq 'number "0x1"))

(define (raw-answer params)
  (match params
    [(list raw) (signed-transaction-hash (bytes->signed-transaction (hex-string->bytes raw)))]))

(define (filling #:chain-id [chain-id "0x7a69"]
                 #:estimate [estimate "0x5208"]
                 #:count [count "0x0"]
                 #:block [block base-block]
                 #:priority [priority "0x3b9aca00"]
                 #:gas-price [gas-price "0x77359400"]
                 #:raw [raw raw-answer]
                 #:receipt [receipt mined]
                 #:replay [replay "0x"])
  (hash "eth_chainId" chain-id
        "eth_estimateGas" estimate
        "eth_getTransactionCount" count
        "eth_getBlockByNumber" block
        "eth_maxPriorityFeePerGas" priority
        "eth_gasPrice" gas-price
        "eth_sendRawTransaction" raw
        "eth_getTransactionReceipt" receipt
        "eth_call" replay))

(define (raw-transactions calls)
  (for/list ([call (in-list calls)]
             #:when (equal? (first call) "eth_sendRawTransaction"))
    (match call
      [(list _ raw) (signed-transaction-transaction (bytes->signed-transaction
                                                     (hex-string->bytes raw)))])))

(define (call-with-fields procedure fields . positional)
  (define given (apply hasheq fields))
  (define keywords (sort (hash-keys given) keyword<?))
  (keyword-apply procedure keywords (map (lambda (kw) (hash-ref given kw)) keywords) positional))

(struct recorder (provider log)
  #:methods gen:provider
  [(define (provider-request p method params)
     (define outcome
       (with-handlers ([exn:fail? values])
         (rpc-call method params #:provider (recorder-provider p))))
     (set-box! (recorder-log p) (cons (list method params outcome) (unbox (recorder-log p))))
     (if (exn? outcome) (raise outcome) outcome))])

(define (recording provider)
  (recorder provider (box '())))

(define (recording-calls r)
  (reverse (unbox (recorder-log r))))
