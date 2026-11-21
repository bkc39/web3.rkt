#lang racket/base

(module+ test
  (require json
           (only-in racket/contract flat-contract-predicate)
           (only-in racket/match match-define)
           rackunit
           web3
           (only-in "helpers/results.rkt" full-block-json receipt-json transaction-info-json/0)
           (only-in "util.rkt" check-blames-here filled-hash max-word raised))

  (define addr "0x00000000000000000000000000000000000c0de0")
  (define a-block (jsexpr->block full-block-json))
  (define a-transaction (jsexpr->transaction-info transaction-info-json/0))
  (define a-receipt (jsexpr->receipt receipt-json))
  (define receipt-hash (hash-ref receipt-json 'transactionHash))
  (define transaction-hash (hash-ref transaction-info-json/0 'hash))
  (define a-log (hasheq 'address addr 'topics '() 'data "0x"))
  (define a-tx (hasheq 'from addr 'to addr 'data "0x"))
  (define a-query (hasheq 'address addr 'fromBlock "earliest"))
  (define history (hasheq 'oldestBlock "0x10" 'baseFeePerGas '("0x7" "0x7")
                          'gasUsedRatio '(0.5) 'reward '(("0x1" "0x2"))))
  (define a-proof (hasheq 'address addr 'accountProof '("0xf8") 'storageProof '()))
  (define an-access-list (hasheq 'accessList '() 'gasUsed "0x5208"))
  (define zero-word (filled-hash #\0))
  (define word-255 (string-append "0x" (make-string 62 #\0) "ff"))

  (define endpoints
    `([,eth-blob-base-fee () "eth_blobBaseFee" () "0x1" 1]
      [,eth-fee-history (4 16 (1/2 25 75.5)) "eth_feeHistory" ("0x4" "0x10" (0.5 25 75.5))
                        ,history ,history]
      [,eth-fee-history (1 "latest" ()) "eth_feeHistory" ("0x1" "latest" ()) ,history ,history]
      [,eth-get-proof (,addr (0 255) "latest")
                      "eth_getProof" (,addr (,zero-word ,word-255) "latest") ,a-proof ,a-proof]
      [,eth-get-block-by-hash ("0xbb" #t) "eth_getBlockByHash" ("0xbb" #t) ,full-block-json ,a-block]
      [,eth-get-block-receipts (16) "eth_getBlockReceipts" ("0x10") (,receipt-json) (,a-receipt)]
      [,eth-get-transaction-receipt (,receipt-hash) "eth_getTransactionReceipt" (,receipt-hash)
                                    ,receipt-json ,a-receipt]
      [,eth-get-block-receipts ("safe") "eth_getBlockReceipts" ("safe") () ()]
      [,eth-get-transaction-by-block-hash-and-index
       ("0xbb" 2) "eth_getTransactionByBlockHashAndIndex" ("0xbb" "0x2")
       ,transaction-info-json/0 ,a-transaction]
      [,eth-get-transaction-by-block-number-and-index
       ("latest" 0) "eth_getTransactionByBlockNumberAndIndex" ("latest" "0x0")
       ,transaction-info-json/0 ,a-transaction]
      [,eth-get-transaction-by-hash (,transaction-hash) "eth_getTransactionByHash"
                                    (,transaction-hash) ,transaction-info-json/0 ,a-transaction]
      [,eth-get-raw-transaction-by-hash ("0xaa") "eth_getRawTransactionByHash" ("0xaa")
                                        "0x02f8" "0x02f8"]
      [,eth-create-access-list (,a-tx "pending") "eth_createAccessList" (,a-tx "pending")
                               ,an-access-list ,an-access-list]
      [,eth-new-filter (,a-query) "eth_newFilter" (,a-query) "0x1f" "0x1f"]
      [,eth-new-block-filter () "eth_newBlockFilter" () "0x2" "0x2"]
      [,eth-new-pending-transaction-filter () "eth_newPendingTransactionFilter" () "0x3" "0x3"]
      [,eth-get-filter-changes ("0x1f") "eth_getFilterChanges" ("0x1f") (,a-log) (,a-log)]
      [,eth-get-filter-changes ("0x2") "eth_getFilterChanges" ("0x2") ("0xbb") ("0xbb")]
      [,eth-get-filter-changes ("0x3") "eth_getFilterChanges" ("0x3") () ()]
      [,eth-get-filter-logs ("0x1f") "eth_getFilterLogs" ("0x1f") (,a-log) (,a-log)]
      [,eth-uninstall-filter ("0x1f") "eth_uninstallFilter" ("0x1f") #t #t]
      [,eth-uninstall-filter ("0x1f") "eth_uninstallFilter" ("0x1f") #f #f]))

  (define (call-mocked procedure arguments method answer)
    (define node (mock-provider (hash method answer)))
    (define result (apply procedure arguments #:provider node))
    (list result (mock-provider-calls node)))

  (define (unexpected-message procedure arguments method answer)
    (with-handlers ([exn:fail:web3? exn-message])
      (call-mocked procedure arguments method answer)))

  (test-case "E1: each endpoint sends its method and converted parameters"
    (for ([row (in-list endpoints)])
      (match-define (list procedure arguments method params answer _) row)
      (match-define (list _ calls) (call-mocked procedure arguments method answer))
      (check-equal? calls (list (cons method params)) method)))

  (test-case "E2: each endpoint returns the node's answer"
    (for ([row (in-list endpoints)])
      (match-define (list procedure arguments method _ answer expected) row)
      (match-define (list result _) (call-mocked procedure arguments method answer))
      (check-equal? result expected method)))

  (test-case "E3: a missing typed object is #f, and a missing raw transaction 'null"
    (for ([row (in-list `([,eth-get-block-receipts ("latest") "eth_getBlockReceipts" null]
                          [,eth-get-transaction-receipt ("0xaa") "eth_getTransactionReceipt" null]
                          [,eth-get-transaction-by-hash ("0xaa") "eth_getTransactionByHash" null]
                          [,eth-get-transaction-by-block-hash-and-index
                           ("0xbb" 0) "eth_getTransactionByBlockHashAndIndex" null]
                          [,eth-get-transaction-by-block-number-and-index
                           (16 0) "eth_getTransactionByBlockNumberAndIndex" null]
                          [,eth-get-block-by-hash ("0xbb" #f) "eth_getBlockByHash" null]
                          [,eth-get-block-by-number ("latest" #t) "eth_getBlockByNumber" null]))])
      (match-define (list procedure arguments method answer) row)
      (match-define (list result _) (call-mocked procedure arguments method answer))
      (check-false result method))
    (for ([row (in-list `([,eth-get-raw-transaction-by-hash ("0xaa") "eth_getRawTransactionByHash"
                                                            null]
                          [,eth-get-raw-transaction-by-hash ("0xaa") "eth_getRawTransactionByHash"
                                                            "0x"]))])
      (match-define (list procedure arguments method answer) row)
      (match-define (list result _) (call-mocked procedure arguments method answer))
      (check-equal? result 'null method)))

  (define malformed
    `([,eth-get-raw-transaction-by-hash ("0xaa") "eth_getRawTransactionByHash" "0x123"
                                        "eth_getRawTransactionByHash: unexpected result \"0x123\""]
      [,eth-get-raw-transaction-by-hash ("0xaa") "eth_getRawTransactionByHash" 42
                                        "eth_getRawTransactionByHash: unexpected result 42"]
      [,eth-fee-history (1 "latest" ()) "eth_feeHistory" "0x1"
                        "eth_feeHistory: unexpected result \"0x1\""]
      [,eth-fee-history (1 "latest" ()) "eth_feeHistory" null
                        "eth_feeHistory: unexpected result 'null"]
      [,eth-blob-base-fee () "eth_blobBaseFee" "0x" "eth_blobBaseFee: unexpected result \"0x\""]
      [,eth-blob-base-fee () "eth_blobBaseFee" "10" "eth_blobBaseFee: unexpected result \"10\""]
      [,eth-blob-base-fee () "eth_blobBaseFee" null "eth_blobBaseFee: unexpected result 'null"]
      [,eth-get-block-by-hash ("0xbb" #f) "eth_getBlockByHash" "0xbb"
                              "eth_getBlockByHash: unexpected result \"0xbb\""]
      [,eth-get-block-receipts ("latest") "eth_getBlockReceipts" ("0xaa")
                               "eth_getBlockReceipts: malformed receipt in receipts 0 from the node: \"0xaa\""]
      [,eth-get-block-receipts ("latest") "eth_getBlockReceipts" ,(hasheq)
                               "eth_getBlockReceipts: unexpected result '#hasheq()"]
      [,eth-get-block-receipts ("latest") "eth_getBlockReceipts"
                               (,(hash-set receipt-json 'gasUsed "zz"))
                               "eth_getBlockReceipts: malformed receipt gas used in receipts 0 from the node: \"zz\""]
      [,eth-get-transaction-receipt ("0xaa") "eth_getTransactionReceipt"
                                    ,(hash-set receipt-json 'gasUsed "zz")
                                    "eth_getTransactionReceipt: malformed receipt gas used from the node: \"zz\""]
      [,eth-get-transaction-receipt ("0xaa") "eth_getTransactionReceipt" 42
                                    "eth_getTransactionReceipt: unexpected result 42"]
      [,eth-get-transaction-by-block-hash-and-index
       ("0xbb" 0) "eth_getTransactionByBlockHashAndIndex" ()
       "eth_getTransactionByBlockHashAndIndex: unexpected result '()"]
      [,eth-get-transaction-by-block-hash-and-index
       ("0xbb" 0) "eth_getTransactionByBlockHashAndIndex" ,(hash-set transaction-info-json/0 'gas "zz")
       "eth_getTransactionByBlockHashAndIndex: malformed transaction gas limit from the node: \"zz\""]
      [,eth-get-transaction-by-hash (,transaction-hash) "eth_getTransactionByHash"
                                    ,(hash-set transaction-info-json/0 'gas "zz")
                                    "eth_getTransactionByHash: malformed transaction gas limit from the node: \"zz\""]
      [,eth-get-transaction-by-hash ("0xaa") "eth_getTransactionByHash" "0xaa"
                                    "eth_getTransactionByHash: unexpected result \"0xaa\""]
      [,eth-get-transaction-by-hash (,(filled-hash #\c)) "eth_getTransactionByHash"
                                    ,transaction-info-json/0
                                    ,(format "eth_getTransactionByHash: malformed transaction hash from the node: ~s"
                                             transaction-hash)]
      [,eth-get-transaction-by-block-number-and-index
       (16 0) "eth_getTransactionByBlockNumberAndIndex" ()
       "eth_getTransactionByBlockNumberAndIndex: unexpected result '()"]
      [,eth-get-proof (,addr () "latest") "eth_getProof" null
                      "eth_getProof: unexpected result 'null"]
      [,eth-create-access-list (,a-tx "latest") "eth_createAccessList" ()
                               "eth_createAccessList: unexpected result '()"]
      [,eth-new-filter (,a-query) "eth_newFilter" 1 "eth_newFilter: unexpected result 1"]
      [,eth-new-filter (,a-query) "eth_newFilter" "abc"
                       "eth_newFilter: unexpected result \"abc\""]
      [,eth-get-filter-changes ("0x1") "eth_getFilterChanges" (1)
                               "eth_getFilterChanges: unexpected result '(1)"]
      [,eth-get-filter-changes ("0x1") "eth_getFilterChanges" null
                               "eth_getFilterChanges: unexpected result 'null"]
      [,eth-get-filter-logs ("0x1") "eth_getFilterLogs" ("0xaa")
                            "eth_getFilterLogs: unexpected result '(\"0xaa\")"]
      [,eth-uninstall-filter ("0x1") "eth_uninstallFilter" "0x1"
                             "eth_uninstallFilter: unexpected result \"0x1\""]))

  (test-case "E4: a malformed result raises exn:fail:web3 naming the method"
    (for ([row (in-list malformed)])
      (match-define (list procedure arguments method answer message) row)
      (check-equal? (unexpected-message procedure arguments method answer) message)))

  (define bad-arguments
    `([,eth-fee-history (0 "latest" ())]
      [,eth-fee-history (1 "newest" ())]
      [,eth-fee-history (1 "latest" (101))]
      [,eth-fee-history (1 "latest" (75 25))]
      [,eth-fee-history (1 "latest" (50 50))]
      [,eth-fee-history (1 "latest" (0.3333333333333333 1/3))]
      [,eth-fee-history (1 "latest" (+nan.0))]
      [,eth-fee-history (1 "latest" (-1))]
      [,eth-get-block-by-hash ("0xbb" yes)]
      [,eth-get-block-receipts ("head")]
      [,eth-get-block-receipts (-1)]
      [,eth-get-transaction-by-block-hash-and-index ("0xbb" -1)]
      [,eth-get-transaction-by-block-number-and-index ("latest" -1)]
      [,eth-get-proof (,addr (-1) "latest")]
      [,eth-get-proof (,addr (,(expt 2 256)) "latest")]
      [,eth-get-proof (,addr "0x0" "latest")]
      [,eth-create-access-list (,(hasheq 'from (void)) "latest")]
      [,eth-new-filter (x)]
      [,eth-get-filter-changes (1)]
      [,eth-uninstall-filter ("1")]))

  (test-case "E5: bad arguments blame the caller"
    (define node (mock-provider (lambda (method params) 'null)))
    (for ([row (in-list bad-arguments)])
      (match-define (list procedure arguments) row)
      (check-blames-here (apply procedure arguments #:provider node)))
    (for ([row (in-list endpoints)])
      (match-define (list procedure arguments _ _ _ _) row)
      (check-blames-here (apply procedure arguments #:provider 'nope))))

  (test-case "the wrong number of arguments is an arity error naming the endpoint"
    (define e (raised (lambda () (eth-fee-history 1 "latest" #:provider (mock-provider (hash))))))
    (check-pred exn:fail:contract:arity? e)
    (check-regexp-match #rx"procedure: eth-fee-history\n" (exn-message e)))

  (test-case "a bad argument's blame points at the endpoint's definition"
    (define message (exn-message (raised (lambda () (eth-chain-id #:provider 'nope)))))
    (check-regexp-match #rx"at:[ \n]+[^\n]*/eth[.]rkt:[0-9]+[.:][0-9]+" message)
    (check-false (regexp-match? #rx"rpc[.]rkt" message)))

  (test-case "an endpoint is a first-class procedure"
    (define node (mock-provider (hash "eth_chainId" "0x7a69" "eth_getBalance" "0x1")))
    (check-equal? (object-name eth-chain-id) 'eth-chain-id)
    (check-equal? (procedure-arity eth-get-balance) 2)
    (check-equal? (keyword-apply eth-chain-id '(#:provider) (list node) '()) 31337)
    (check-equal? (apply eth-get-balance addr '("latest") #:provider node) 1)
    (with-provider node
      (check-equal? (map eth-get-balance (list addr addr) '(1 "latest")) '(1 1))
      (check-blames-here (map eth-get-balance (list addr) '("head")))))

  (test-case "E6: existing endpoints blame the caller"
    (parameterize ([current-provider (mock-provider (hash "eth_getBalance" "0x0"))])
      (check-blames-here (eth-get-balance 'x "latest"))
      (check-blames-here (eth-chain-id #:provider 'nope))))

  (test-case "E7: percentiles are JSON numbers"
    (match-define (list _ (list (list _ _ _ percentiles)))
      (call-mocked eth-fee-history '(4 16 (1/2 25 75.5)) "eth_feeHistory" history))
    (check-false (jsexpr? '(1/2 25 75.5)))
    (check-true (jsexpr? percentiles)))

  (define (sent-slots slots)
    (match-define (list _ (list (list _ _ sent _)))
      (call-mocked eth-get-proof (list addr slots "latest") "eth_getProof" a-proof))
    sent)

  (test-case "E8: storage slots are 32-byte words"
    (check-equal? (sent-slots (list 0)) (list (string-append "0x" (make-string 64 #\0))))
    (check-equal? (sent-slots (list max-word)) (list (string-append "0x" (make-string 64 #\f))))
    (check-equal? (sent-slots (list 255)) (list (string-append "0x" (make-string 62 #\0) "ff"))))

  (test-case "E9: filter ids pass back verbatim"
    (define node (mock-provider (hash "eth_newFilter" "0x01ab" "eth_getFilterChanges" '())))
    (check-equal? (eth-get-filter-changes (eth-new-filter a-query #:provider node) #:provider node)
                  '())
    (check-equal? (mock-provider-calls node)
                  `(("eth_newFilter" ,a-query) ("eth_getFilterChanges" "0x01ab"))))

  (define sixty-five-digits (string-append "0x1" (make-string 64 #\0)))

  (test-case "E10: quantities are strict"
    (for ([row (in-list `([,eth-blob-base-fee "eth_blobBaseFee"] [,eth-chain-id "eth_chainId"]))])
      (match-define (list procedure method) row)
      (check-equal? (unexpected-message procedure '() method "0x")
                    (format "~a: unexpected result \"0x\"" method))
      (check-equal? (call-mocked procedure '() method "0x0001") `(1 ((,method))))
      (check-equal? (unexpected-message procedure '() method sixty-five-digits)
                    (format "~a: unexpected result ~s" method sixty-five-digits))))

  (test-case "E11: a bad block parameter's violation says block-param/c"
    (define e (raised (lambda () (eth-get-balance addr "head" #:provider (mock-provider (hash))))))
    (check-regexp-match #rx"expected: block-param/c" (exn-message e)))

  (define (passes? contract value)
    ((flat-contract-predicate contract) value))

  (test-case "reward percentiles increase strictly as the node reads them"
    (for ([percentiles (in-list '(() (0) (1/2 25 75.5) (0 100)))])
      (check-true (passes? reward-percentiles/c percentiles) (format "~s" percentiles)))
    (for ([percentiles (in-list '((50 50) (0.3333333333333333 1/3) (25 1/2) (101) x))])
      (check-false (passes? reward-percentiles/c percentiles) (format "~s" percentiles)))
    (define e (raised (lambda ()
                        (eth-fee-history 1 "latest" '(50 50) #:provider (mock-provider (hash))))))
    (check-regexp-match #rx"expected: reward-percentiles/c" (exn-message e)))

  (test-case "a storage slot is a 256-bit word"
    (for ([slot (in-list (list 0 max-word))])
      (check-true (passes? storage-slot/c slot)))
    (for ([slot (in-list (list -1 (add1 max-word) "0x0"))])
      (check-false (passes? storage-slot/c slot)))
    (define e (raised (lambda ()
                        (eth-get-proof addr '(-1) "latest" #:provider (mock-provider (hash))))))
    (check-regexp-match #rx"expected: storage-slot/c" (exn-message e)))

  (test-case "E12: logs are hash tables"
    (check-equal? (unexpected-message eth-get-logs (list a-query) "eth_getLogs" '("0xaa"))
                  "eth_getLogs: unexpected result '(\"0xaa\")"))

  (test-case "E13: a block hash is hex"
    (parameterize ([current-provider (mock-provider (hash "eth_getBlockTransactionCountByHash"
                                                          "0x1"))])
      (check-blames-here (eth-get-block-transaction-count-by-hash "bb")))))
