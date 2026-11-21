#lang racket/base

(module+ test
  (require racket/format
           (only-in racket/match match-define)
           rackunit
           web3/eth/receipt
           (only-in (submod web3/eth/receipt internal) receipt-fields)
           (only-in web3/eth eth-get-block-receipts eth-get-transaction-receipt wait-for-receipt)
           (only-in web3/eth/log jsexpr->log log-address log-block-timestamp log?)
           (only-in web3/event receipt-logs)
           (only-in web3/rpc hex-string->bytes mock-provider)
           (only-in "helpers/records.rkt"
                    check-fields check-in-web3-and-manual message-of provided-accessors)
           "helpers/results.rkt"
           "modules.rkt"
           "util.rkt")

  (define tx-hash "0xf6530f5992a7b2b0ff8b8014dc54ad20e0f16ae89343cfb98640c6a7d94cf5d4")
  (define block-hash "0x78e3cb19177d24232982259d6717f8ea45cfae5f369be21280af7b8e535146b5")
  (define token "0x5FbDB2315678afecb367f032d93F642f64180aa3")
  (define shouted-hash (string-append "0x" (string-upcase (substring tx-hash 2))))

  (define expected-fields
    `([transaction-hash ,tx-hash]
      [transaction-index 0]
      [block-hash ,block-hash]
      [block-number 2]
      [from "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"]
      [to ,token]
      [type 2]
      [status 1]
      [root #f]
      [cumulative-gas-used #xcbf6]
      [gas-used #xcbf6]
      [effective-gas-price #x705f9991]
      [contract-address #f]
      [logs-bloom ,(hex-string->bytes (hash-ref receipt-json 'logsBloom))]
      [blob-gas-used #f]
      [blob-gas-price #f]))

  (test-case "R1: a node's receipt reads into its fields"
    (define receipt (jsexpr->receipt receipt-json))
    (check-fields receipt "receipt" receipt-fields expected-fields)
    (check-equal? (map log-address (receipt-logs receipt)) (list token))
    (check-true (receipt-succeeded? receipt))
    (check-false (receipt-succeeded? (jsexpr->receipt (hash-set receipt-json 'status "0x0"))))
    (check-eq? (receipt->jsexpr receipt) receipt-json)
    (check-equal? receipt (jsexpr->receipt (for/hasheq ([(key v) (in-hash receipt-json)])
                                              (values key v)))))

  (test-case "R1: hashes read lowercase and addresses checksummed"
    (define receipt
      (jsexpr->receipt (hash-set* receipt-json
                                  'transactionHash shouted-hash
                                  'contractAddress "0x5fbdb2315678afecb367f032d93f642f64180aa3")))
    (check-equal? (receipt-transaction-hash receipt) tx-hash)
    (check-equal? (receipt-contract-address receipt) token))

  (test-case "R4: a receipt needs a status or a root"
    (check-regexp-match #rx"^jsexpr->receipt: malformed receipt status from the node: 'null$"
                        (message-of (lambda ()
                                      (jsexpr->receipt (hash-remove root-receipt-json 'root)))))
    (define rooted (jsexpr->receipt root-receipt-json))
    (check-false (receipt-status rooted))
    (check-equal? (receipt-root rooted) (hash-ref root-receipt-json 'root))
    (check-equal? (message-of (lambda () (receipt-succeeded? rooted)))
                  "receipt-succeeded?: the receipt has a root, not a status"))

  (test-case "R5: unknown fields are kept"
    (define extended (hash-set receipt-json 'l1Fee "0x5"))
    (check-eq? (receipt->jsexpr (jsexpr->receipt extended)) extended))

  (define printed
    `([,receipt-json
       ,(~a "#<receipt " tx-hash " block 2 index 0 status 1 gas used 52214>")]
      [,(hash-set* receipt-json 'status "0x0" 'gasUsed "0x5208" 'transactionIndex "0x3")
       ,(~a "#<receipt " tx-hash " block 2 index 3 status 0 gas used 21000>")]
      [,root-receipt-json
       ,(~a "#<receipt " tx-hash " block 2 index 0 root 0x12345678… gas used 52214>")]))

  (test-case "R6: a receipt prints its hash, place, outcome and gas in every mode"
    (for ([row (in-list printed)])
      (match-define (list json text) row)
      (define receipt (jsexpr->receipt json))
      (for ([mode (in-list (list ~a ~s ~v))])
        (check-equal? (mode receipt) text)
        (check-equal? (mode (list receipt)) (if (eq? mode ~v) (~a "(list " text ")") (~a "(" text ")"))))))

  (test-case "R7: receipt accessors blame their caller"
    (check-blames-here (receipt-gas-used 42))
    (check-blames-here (receipt->jsexpr (hasheq)))
    (check-blames-here (receipt-succeeded? (hasheq)))
    (check-blames-here (receipt-logs (hasheq))))

  (define (answering result)
    (mock-provider (hash "eth_getTransactionReceipt" result)))

  (test-case "R9: eth-get-transaction-receipt reads a receipt, or #f"
    (check-false (eth-get-transaction-receipt tx-hash #:provider (answering 'null)))
    (check-equal? (eth-get-transaction-receipt tx-hash #:provider (answering receipt-json))
                  (jsexpr->receipt receipt-json))
    (define bad (message-of (lambda ()
                              (eth-get-transaction-receipt
                               tx-hash #:provider (answering (hash-set receipt-json 'gasUsed "zz"))))))
    (check-regexp-match #rx"^eth_getTransactionReceipt: malformed receipt gas used from the node" bad)
    (check-false (regexp-match? #rx"unexpected result" bad))
    (check-regexp-match #rx"^eth_getTransactionReceipt: unexpected result \"0xaa\"$"
                        (message-of (lambda ()
                                      (eth-get-transaction-receipt
                                       tx-hash #:provider (answering "0xaa"))))))

  (test-case "eth-get-transaction-receipt reads the receipt of the transaction asked"
    (check-equal? (eth-get-transaction-receipt shouted-hash #:provider (answering receipt-json))
                  (jsexpr->receipt receipt-json))
    (check-equal? (message-of (lambda ()
                                (eth-get-transaction-receipt (filled-hash #\c)
                                                             #:provider (answering receipt-json))))
                  (~a "eth_getTransactionReceipt: malformed receipt transaction hash from the node: "
                      (~s tx-hash))))

  (test-case "R10: wait-for-receipt reads the receipt of its own transaction"
    (check-equal? (wait-for-receipt shouted-hash #:provider (answering receipt-json))
                  (jsexpr->receipt receipt-json))
    (define other (filled-hash #\c))
    (check-equal? (message-of (lambda () (wait-for-receipt other #:provider (answering receipt-json))))
                  (~a "wait-for-receipt: malformed receipt transaction hash from the node: "
                      (~s tx-hash)))
    (check-regexp-match #rx"^wait-for-receipt: malformed receipt gas used from the node: \"zz\"$"
                        (message-of (lambda ()
                                      (wait-for-receipt
                                       tx-hash
                                       #:provider (answering (hash-set receipt-json 'gasUsed "zz")))))))

  (test-case "every accessor the field table provides is in web3 and the manual"
    (check-in-web3-and-manual (provided-accessors "receipt" receipt-fields))
    (check-false (memq 'receipt-logs (exported-names 'web3/eth/receipt))))

  (test-case "A1: a receipt without an effective gas price, and a numeric log timestamp, read"
    (check-false (receipt-effective-gas-price
                  (jsexpr->receipt (hash-remove receipt-json 'effectiveGasPrice))))
    (check-equal? (log-block-timestamp (jsexpr->log (hash-set log-json 'blockTimestamp 1795291989)))
                  1795291989))

  (test-case "A5: eth-get-block-receipts names the receipt it could not read"
    (define bad (list receipt-json (hash-set receipt-json 'gasUsed "zz")))
    (check-equal? (message-of (lambda ()
                                (eth-get-block-receipts
                                 2 #:provider (mock-provider (hash "eth_getBlockReceipts" bad)))))
                  "eth_getBlockReceipts: malformed receipt gas used in receipts 1 from the node: \"zz\""))

  (test-case "R13: web3 provides every name web3/eth/receipt does"
    (define all (exported-names 'web3))
    (for ([name (in-list (exported-names 'web3/eth/receipt))])
      (check-not-false (memq name all) (~a name))))

  (test-case "receipt logs are logs"
    (check-true (andmap log? (receipt-logs (jsexpr->receipt receipt-json))))))
