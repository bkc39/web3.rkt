#lang racket/base

(module+ test
  (require racket/list
           racket/match
           rackunit
           (only-in scribble/example make-base-eval)
           web3
           (only-in (submod web3/contract internal) receipt-timeout)
           (only-in "helpers/accounts.rkt" random-natural)
           "helpers/mock.rkt"
           (only-in "helpers/signers.rkt" key0 signer0)
           (only-in "helpers/transactions.rkt" random-partial random-u64)
           "modules.rkt"
           (only-in "util.rkt" anvil-alice anvil-bob insufficient-balance pick raised seeded))

  (define one-and-a-half (ether->wei "1.5"))
  (define transfer-estimate
    (hasheq 'from anvil-alice 'to anvil-bob 'data "0x" 'value "0x14d1120d7b160000"))
  (define gwei (expt 10 9))

  (define (filled handler fields #:to [to anvil-bob] #:procedure [procedure fill-transaction])
    (run-mock handler (lambda () (call-with-fields procedure fields signer0 to))))

  (define (after-count calls)
    (match (memf (lambda (call) (equal? (first call) "eth_getTransactionCount")) calls)
      [(cons _ more) (method-names more)]))

  (test-case "F1: a send with a base fee is type 2, filled from five requests, and sent raw"
    (define-values (result calls)
      (filled (filling) `(#:value ,one-and-a-half) #:procedure send-signed-transaction))
    (check-equal? result (list eip1559-hash))
    (check-equal? calls
                  `(("eth_chainId")
                    ("eth_estimateGas" ,transfer-estimate)
                    ("eth_getTransactionCount" ,anvil-alice "pending")
                    ("eth_getBlockByNumber" "latest" #f)
                    ("eth_maxPriorityFeePerGas")
                    ("eth_sendRawTransaction" ,eip1559-raw))))

  (test-case "F2: without a base fee the send is legacy at the node's gas price"
    (define-values (result calls)
      (filled (filling #:block legacy-block) `(#:value ,one-and-a-half)
              #:procedure send-signed-transaction))
    (check-equal? result (list legacy-hash))
    (check-equal? calls
                  `(("eth_chainId")
                    ("eth_estimateGas" ,transfer-estimate)
                    ("eth_getTransactionCount" ,anvil-alice "pending")
                    ("eth_getBlockByNumber" "latest" #f)
                    ("eth_gasPrice")
                    ("eth_sendRawTransaction" ,legacy-raw))))

  (define type-rows
    '((() base 2 ("eth_getBlockByNumber" "eth_maxPriorityFeePerGas") (1000000000 3000000000))
      (() none 0 ("eth_getBlockByNumber" "eth_gasPrice") (2000000000))
      ((#:access-list ()) base 2 ("eth_getBlockByNumber" "eth_maxPriorityFeePerGas")
                          (1000000000 3000000000))
      ((#:access-list ()) none 1 ("eth_getBlockByNumber" "eth_gasPrice") (2000000000))
      ((#:gas-price 7) both 0 () (7))
      ((#:gas-price 7 #:access-list ()) both 1 () (7))
      ((#:type 0) both 0 ("eth_gasPrice") (2000000000))
      ((#:type 1) both 1 ("eth_gasPrice") (2000000000))
      ((#:type 2) base 2 ("eth_getBlockByNumber" "eth_maxPriorityFeePerGas")
                  (1000000000 3000000000))
      ((#:type 2) none no-base-fee ("eth_getBlockByNumber") ())
      ((#:max-fee-per-gas 9000000000) both 2 ("eth_maxPriorityFeePerGas") (1000000000 9000000000))
      ((#:max-priority-fee-per-gas 5) base 2 ("eth_getBlockByNumber") (5 2000000005))
      ((#:max-priority-fee-per-gas 5) none no-base-fee ("eth_getBlockByNumber") ())
      ((#:max-priority-fee-per-gas 5 #:max-fee-per-gas 9) both 2 () (5 9))
      ((#:max-fee-per-gas 1000) both fee-cap ("eth_maxPriorityFeePerGas") ())))

  (define latest-blocks
    `((both ,base-block ,legacy-block) (base ,base-block) (none ,legacy-block)))

  (define (fee-fields tx)
    (match (transaction-type tx)
      [2 (list (transaction-max-priority-fee-per-gas tx) (transaction-max-fee-per-gas tx))]
      [_ (list (transaction-gas-price tx))]))

  (test-case "F3: the fields given and the latest block decide the type and the requests"
    (for* ([row (in-list type-rows)]
           [block (in-list (match row
                             [(list _ blocks _ ...) (rest (assq blocks latest-blocks))]))])
      (match-define (list fields _ expected requests fees) row)
      (define name (format "~s ~s" row block))
      (define-values (result calls) (filled (filling #:block block) fields))
      (check-equal? (after-count calls) requests name)
      (match result
        [(list (? transaction? tx))
         (check-equal? (transaction-type tx) expected name)
         (check-equal? (fee-fields tx) fees name)]
        [(? exn:fail:web3:fill? e) (check-eq? (exn:fail:web3:fill-reason e) expected name)])))

  (define field-accessors
    `((#:nonce ,transaction-nonce "eth_getTransactionCount")
      (#:gas-limit ,transaction-gas-limit "eth_estimateGas")
      (#:chain-id ,transaction-chain-id #f)
      (#:type ,transaction-type #f)
      (#:value ,transaction-value #f)
      (#:data ,transaction-data #f)
      (#:gas-price ,transaction-gas-price "eth_gasPrice")
      (#:max-priority-fee-per-gas ,transaction-max-priority-fee-per-gas "eth_maxPriorityFeePerGas")
      (#:max-fee-per-gas ,transaction-max-fee-per-gas #f)
      (#:access-list ,transaction-access-list #f)))

  (test-case "F4: a field given is kept, and its request is never made"
    (define prng (seeded 6 18 75 1 2 3))
    (for ([_ (in-range 300)])
      (define fields (random-partial prng))
      (define given (apply hasheq fields))
      (define name (format "~s" fields))
      (define-values (result calls) (filled (filling) fields))
      (match-define (list tx) result)
      (define methods (method-names calls))
      (for ([row (in-list field-accessors)])
        (match-define (list kw accessor request) row)
        (when (hash-has-key? given kw)
          (check-equal? (accessor tx) (hash-ref given kw) (format "~a ~a" kw name))
          (when request
            (check-false (member request methods) (format "~a ~a" request name)))))
      (when (or (hash-has-key? given '#:gas-price) (hash-has-key? given '#:max-fee-per-gas)
                (memv (hash-ref given '#:type #f) '(0 1)))
        (check-false (member "eth_getBlockByNumber" methods) name))
      (check-equal? (first methods) "eth_chainId" name)
      (check-equal? (count (lambda (m) (equal? m "eth_chainId")) methods) 1 name)))

  (define (random-fee prng)
    (if (zero? (random 2 prng))
        (pick prng (list 0 1 gwei (quotient (sub1 (expt 2 255)) 2) (sub1 (expt 2 255))))
        (random-natural prng (random 33 prng))))

  (test-case "F5: filled fields are the node's answers, and a fee cap past 256 bits is refused"
    (define prng (seeded 5 5 1 2 0 9))
    (define refusals
      (for/sum ([_ (in-range 300)])
        (define base (random-fee prng))
        (define priority (random-fee prng))
        (define gas-price (random-fee prng))
        (define tx-count (random-u64 prng))
        (define estimate (max 1 (random-u64 prng)))
        (define legacy? (zero? (random 4 prng)))
        (define handler
          (filling #:block (if legacy?
                               legacy-block
                               (hasheq 'number "0x1" 'baseFeePerGas (number->hex-string base)))
                   #:priority (number->hex-string priority)
                   #:gas-price (number->hex-string gas-price)
                   #:count (number->hex-string tx-count)
                   #:estimate (number->hex-string estimate)))
        (define name (format "~s" (list base priority gas-price tx-count estimate legacy?)))
        (define-values (result _calls) (filled handler '()))
        (define cap (+ (* 2 base) priority))
        (cond
          [(and (not legacy?) (>= cap (expt 2 256)))
           (check-pred exn:fail:web3? result name)
           (check-equal? (exn-message result)
                         (format "fill-transaction: malformed base fee from the node: ~s"
                                 (number->hex-string base))
                         name)
           1]
          [else
           (match-define (list tx) result)
           (check-equal? (list (transaction-nonce tx) (transaction-gas-limit tx)
                               (transaction-chain-id tx))
                         (list tx-count estimate 31337)
                         name)
           (check-equal? (fee-fields tx) (if legacy? (list gas-price) (list priority cap)) name)
           0])))
    (check-true (< 0 refusals 300)))

  (test-case "F6: what is sent is the filled transaction signed by the key"
    (define prng (seeded 9 9 2 7 3 1))
    (for ([_ (in-range 200)])
      (define fields (random-partial prng))
      (define-values (filled-result _fill-calls) (filled (filling) fields))
      (define-values (sent calls) (filled (filling) fields #:procedure send-signed-transaction))
      (match-define (list tx) filled-result)
      (define stx (sign-transaction key0 tx))
      (define name (format "~s" fields))
      (check-equal? sent (list (signed-transaction-hash stx)) name)
      (check-equal? (last calls)
                    (list "eth_sendRawTransaction"
                          (bytes->hex-string (signed-transaction->bytes stx)))
                    name)))

  (define (estimate-object fields #:from [from signer0] #:to [to anvil-bob])
    (define-values (result calls)
      (run-mock (filling) (lambda () (call-with-fields fill-transaction fields from to))))
    (match-define (list _ (list "eth_estimateGas" object) _ ...) calls)
    (values (first result) object))

  (test-case "F7: the estimate is from and to EIP-55 addresses, with the value and access list"
    (define-values (_tx lowercase)
      (estimate-object '() #:from (string-downcase anvil-alice) #:to (string-downcase anvil-bob)))
    (check-equal? lowercase (hasheq 'from anvil-alice 'to anvil-bob 'data "0x"))
    (define-values (_creation creation) (estimate-object '(#:data #"\x60\x80") #:to #f))
    (check-equal? creation (hasheq 'from anvil-alice 'data "0x6080"))
    (define-values (_empty empty-list) (estimate-object '(#:access-list ())))
    (check-false (hash-has-key? empty-list 'accessList))
    (define entries
      (list (list (string-downcase anvil-bob) (make-bytes 32 1)) (list anvil-alice)))
    (define-values (tx listed) (estimate-object `(#:access-list ,entries #:value 5)))
    (check-equal? (hash-ref listed 'value) "0x5")
    (check-equal? (hash-ref listed 'accessList)
                  (hash-ref (transaction->jsexpr tx #:from anvil-alice) 'accessList)))

  (define (filled-calls fields #:procedure procedure)
    (define-values (_result calls) (filled (filling) fields #:procedure procedure))
    calls)

  (test-case "F17: /receipt waits for the receipt, and replays a failed transaction"
    (define-values (result calls)
      (filled (filling) `(#:value ,one-and-a-half) #:procedure send-signed-transaction/receipt))
    (check-eq? (first result) mined)
    (check-equal? (last calls) (list "eth_getTransactionReceipt" eip1559-hash))
    (check-equal? (drop-right calls 1)
                  (filled-calls `(#:value ,one-and-a-half) #:procedure send-signed-transaction))
    (define-values (failure failure-calls)
      (filled (filling #:receipt failed #:replay (reverting "eth_call"))
              `(#:value ,one-and-a-half #:errors (,insufficient-balance))
              #:procedure send-signed-transaction/receipt))
    (check-pred exn:fail:web3:transaction? failure)
    (check-equal? (abi-revert-arguments (exn:fail:web3:transaction-revert failure)) '(0 5000))
    (check-equal? (last failure-calls)
                  `("eth_call" ,(hash-set transfer-estimate 'gas "0x5208") "0x5"))
    (define-values (lost _lost-calls)
      (parameterize ([receipt-timeout 0.05])
        (filled (filling #:receipt 'null) '() #:procedure send-signed-transaction/receipt)))
    (check-regexp-match
     (pregexp (string-append "^send-signed-transaction/receipt: sent transaction 0x[0-9a-f]{64}, "
                             "outcome unknown: "))
     (exn-message lost)))

  (test-case "F18: sends in a row read the pending count each time and get consecutive nonces"
    (define sent (box 0))
    (define base (filling))
    (define (handler method params)
      (match method
        ["eth_getTransactionCount" (number->hex-string (unbox sent))]
        ["eth_sendRawTransaction"
         (set-box! sent (add1 (unbox sent)))
         (raw-answer params)]
        [_ (hash-ref base method)]))
    (define node (mock-provider handler))
    (for ([_ (in-range 3)])
      (send-signed-transaction signer0 anvil-bob #:provider node))
    (define calls (mock-provider-calls node))
    (check-equal? (map transaction-nonce (raw-transactions calls)) '(0 1 2))
    (check-equal? (count (lambda (call) (equal? (first call) "eth_getTransactionCount")) calls) 3))

  (test-case "F21: eth-max-priority-fee-per-gas reads the node's answer in wei"
    (define node (mock-provider (hash "eth_maxPriorityFeePerGas" "0x3b9aca00")))
    (check-equal? (eth-max-priority-fee-per-gas #:provider node) gwei)
    (check-equal? (mock-provider-calls node) '(("eth_maxPriorityFeePerGas")))
    (define e
      (raised (lambda ()
                (eth-max-priority-fee-per-gas
                 #:provider (mock-provider (hash "eth_maxPriorityFeePerGas" "x"))))))
    (check-equal? (exn-message e) "eth_maxPriorityFeePerGas: unexpected result \"x\""))

  (define send-names
    '(fill-transaction send-signed-transaction send-signed-transaction/receipt
                       exn:fail:web3:fill exn:fail:web3:fill? exn:fail:web3:fill-reason
                       struct:exn:fail:web3:fill))

  (test-case "F22: web3 re-exports web3/send, and requires without conflicts"
    (define all (exported-names 'web3))
    (for ([name (in-list (cons 'eth-max-priority-fee-per-gas send-names))])
      (check-not-false (memq name all) (format "~a" name)))
    (check-equal? (sort (exported-names 'web3/send) symbol<?) (sort send-names symbol<?))
    (define ev (make-base-eval))
    (check-not-exn (lambda () (ev '(require web3 libsecp256k1))))))
