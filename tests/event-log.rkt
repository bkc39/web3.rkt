#lang racket/base

;; Logs read from the node's JSON.

(module+ test
  (require racket/format
           (only-in racket/list make-list)
           rackunit
           web3/eth/log
           (only-in web3/rpc exn:fail:web3?)
           (only-in web3/abi exn:fail:web3:abi?)
           "util.rkt")

  (define address "0x5fbdb2315678afecb367f032d93f642f64180aa3")
  (define topic-0 "0xDDF252AD1BE2C89B69C2B068FC378DAA952BA7F163C4A11628F55A4DF523B3EF")
  (define topic-1 "0x000000000000000000000000f39fd6e51aad88f6f4ce6ab8827279cfffb92266")
  (define block-hash (filled-hash #\A))
  (define tx-hash (filled-hash #\b))

  (define anvil-object
    (hasheq 'address address
            'topics (list topic-0 topic-1)
            'data "0x00000000000000000000000000000000000000000000000000000000000000fa"
            'blockHash block-hash
            'blockNumber "0xa"
            'blockTimestamp "0x6553f100"
            'transactionHash tx-hash
            'transactionIndex "0x1"
            'logIndex "0x2"
            'removed #f))

  (define (with-field key v)
    (hash-set anvil-object key v))

  (define (without-field key)
    (hash-remove anvil-object key))

  (test-case "a node's log object reads into its fields"
    (define l (jsexpr->log anvil-object))
    (check-pred log? l)
    (check-equal? (log-address l) "0x5FbDB2315678afecb367f032d93F642f64180aa3")
    (check-equal? (log-topics l) (list (string-downcase topic-0) topic-1))
    (check-equal? (log-data l) (word 250))
    (check-equal? (log-block-number l) 10)
    (check-equal? (log-block-hash l) (string-downcase block-hash))
    (check-equal? (log-block-timestamp l) #x6553f100)
    (check-equal? (log-transaction-hash l) tx-hash)
    (check-equal? (log-transaction-index l) 1)
    (check-equal? (log-index l) 2)
    (check-false (log-removed? l))
    (check-eq? (log->jsexpr l) anvil-object)
    (check-equal? (log-address (jsexpr->log (with-field 'address (string-append "0x" (string-upcase (substring address 2))))))
                  "0x5FbDB2315678afecb367f032d93F642f64180aa3"))

  (test-case "a pending log has no place in the chain"
    (define keys '(blockNumber blockHash blockTimestamp transactionHash transactionIndex logIndex))
    (for ([pending (in-list (list (for/fold ([h anvil-object]) ([k (in-list keys)]) (hash-set h k 'null))
                                  (for/fold ([h anvil-object]) ([k (in-list keys)]) (hash-remove h k))))])
      (define l (jsexpr->log pending))
      (check-equal? (list (log-block-number l) (log-block-hash l) (log-block-timestamp l)
                          (log-transaction-hash l) (log-transaction-index l) (log-index l))
                    '(#f #f #f #f #f #f))))

  (test-case "removed defaults to #f, and unknown fields are kept"
    (check-false (log-removed? (jsexpr->log (without-field 'removed))))
    (check-true (log-removed? (jsexpr->log (with-field 'removed #t))))
    (define extended (with-field 'l1BatchNumber "0x5"))
    (check-eq? (log->jsexpr (jsexpr->log extended)) extended))

  (define malformed
    (list 42 'null '(1)
          (without-field 'address) (with-field 'address "0x12") (with-field 'address 'null)
          (without-field 'topics) (with-field 'topics "0xab")
          (with-field 'topics (make-list 5 topic-1))
          (with-field 'topics (list (substring topic-1 0 65)))
          (with-field 'topics (list (string-append "0xzz" (substring topic-1 4))))
          (without-field 'data) (with-field 'data "0x123") (with-field 'data "12")
          (with-field 'blockNumber "zz") (with-field 'blockNumber 5)
          (with-field 'blockHash "0x12")
          (with-field 'removed "false")))

  (test-case "malformed log objects raise exn:fail:web3"
    (for ([v (in-list malformed)])
      (define e (raised (lambda () (jsexpr->log v))))
      (check-pred exn:fail:web3? e (~e v))
      (check-false (exn:fail:contract? e) (~e v))
      (check-false (exn:fail:web3:abi? e) (~e v))
      (when e
        (check-regexp-match #rx"^jsexpr->log: malformed" (exn-message e) (~e v)))))

  (define printed
    "#<log 0x5FbDB2315678afecb367f032d93F642f64180aa3 block 10 index 2 topics [0xddf252ad… 0x00000000…] data 32 bytes>")

  (test-case "a log prints its address, place, abbreviated topics and data size in every mode"
    (define l (jsexpr->log anvil-object))
    (check-equal? (format "~a" l) printed)
    (check-equal? (format "~s" l) printed)
    (check-equal? (format "~v" l) printed)
    (check-equal? (format "~v" (list l)) (string-append "(list " printed ")"))
    (check-equal? (format "~s" (list l)) (string-append "(" printed ")"))
    (check-equal? (format "~a" (list l)) (string-append "(" printed ")"))
    (check-equal?
     (format "~a" (jsexpr->log (hash-set* anvil-object 'blockNumber 'null 'logIndex 'null 'removed #t
                                          'topics '() 'data "0x01")))
     "#<log 0x5FbDB2315678afecb367f032d93F642f64180aa3 block pending index pending topics [] data 1 byte removed>"))

  (test-case "log accessors blame their caller"
    (check-blames-here (log-address 42))
    (check-blames-here (log->jsexpr (hasheq)))))
