#lang racket/base

(module+ test
  (require racket/format
           (only-in racket/file file->string)
           (only-in racket/match match match-define)
           rackunit
           (only-in web3/account secp256k1-order signature)
           (only-in web3/eth eth-get-transaction-by-hash)
           web3/eth/transaction-info
           (only-in (submod web3/eth/transaction-info internal) transaction-info-fields)
           (only-in web3/private/record field-row-field field-row-provided?)
           (only-in web3/rpc exn:fail:web3? mock-provider number->hex-string)
           web3/transaction
           (only-in "helpers/results.rkt"
                    transaction-info-json/0
                    transaction-info-json/1
                    transaction-info-json/2)
           (only-in "helpers/transactions.rkt" example-key example-transactions nick-raw)
           "modules.rkt"
           "util.rkt")

  (define alice "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")
  (define bob "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")
  (define legacy-hash (hash-ref transaction-info-json/0 'hash))
  (define block-hash "0x56268c1258891dd669aacbe1c854f83204063dd979b3207267179897f8e45d7c")

  (define (message-of thunk)
    (define e (raised thunk))
    (check-pred exn:fail:web3? e)
    (check-false (exn:fail:contract? e))
    (and e (exn-message e)))

  (define expected-fields
    `([hash ,legacy-hash]
      [block-hash ,block-hash]
      [block-number 1]
      [block-timestamp #x6b02118f]
      [transaction-index 0]
      [from ,alice]
      [type 0]
      [chain-id 31337]
      [nonce 0]
      [to ,bob]
      [value 1]
      [data #""]
      [gas-limit 21000]
      [gas-price #x6fc23ac00]
      [max-priority-fee-per-gas #f]
      [max-fee-per-gas #f]
      [max-fee-per-blob-gas #f]
      [access-list #f]
      [blob-versioned-hashes #f]
      [v #xf4f6]
      [y-parity #f]
      [r #xbbfa8af4971395f4254befd93c53c020fb096fc451369a7dc6496814972c5948]
      [s #x17e6be4dd1cdb05aa9f72da7d69aa2af9d5c619ad2d29b538f7cf16e40a21411]))

  (define (accessor-name row)
    (string->symbol (~a "transaction-info-" (field-row-field row))))

  (define provided-rows (filter field-row-provided? transaction-info-fields))

  (test-case "T1: a node's transaction reads into its fields"
    (define info (jsexpr->transaction-info transaction-info-json/0))
    (for ([row (in-list provided-rows)])
      (match (assq (field-row-field row) expected-fields)
        [(list _ value)
         (check-equal? ((dynamic-require 'web3 (accessor-name row)) info) value
                       (~a (field-row-field row)))]
        [#f (fail (~a "no expected value for " (field-row-field row)))]))
    (check-eq? (transaction-info->jsexpr info) transaction-info-json/0)
    (check-equal? (transaction-info-access-list (jsexpr->transaction-info transaction-info-json/1))
                  (list (list bob (make-bytes 32 1)))))

  (test-case "B4: a pending transaction has no block"
    (define pending
      (jsexpr->transaction-info
       (hash-set* (hash-remove transaction-info-json/2 'blockTimestamp)
                  'blockHash 'null 'blockNumber 'null 'transactionIndex 'null)))
    (check-equal? (map (lambda (field) (field pending))
                       (list transaction-info-block-hash transaction-info-block-number
                             transaction-info-block-timestamp transaction-info-transaction-index))
                  '(#f #f #f #f))
    (check-equal? (~a pending)
                  (~a "#<transaction-info " (hash-ref transaction-info-json/2 'hash)
                      " type 2 nonce 2 block pending index pending>")))

  (define (quantity n)
    (number->hex-string n))

  (define (signed->json stx)
    (define tx (signed-transaction-transaction stx))
    (match-define (signature r s y-parity) (signed-transaction-signature stx))
    (define request (transaction->jsexpr tx #:from (signed-transaction-sender stx)))
    (define common
      (hash-set* (hash-remove request 'data)
                 'input (hash-ref request 'data)
                 'hash (signed-transaction-hash stx)
                 'r (quantity r)
                 's (quantity s)))
    (match (cons (transaction-type tx) (transaction-chain-id tx))
      [(cons 0 #f) (hash-set common 'v (quantity (+ 27 y-parity)))]
      [(cons 0 chain-id) (hash-set common 'v (quantity (+ 35 (* 2 chain-id) y-parity)))]
      [_ (hash-set* common 'v (quantity y-parity) 'yParity (quantity y-parity))]))

  (define signed-examples
    (append (for/list ([tx (in-list example-transactions)])
              (sign-transaction example-key tx))
            (list (bytes->signed-transaction nick-raw))))

  (test-case "T4: transaction-info->signed-transaction rebuilds the transaction"
    (for ([stx (in-list signed-examples)])
      (define info (jsexpr->transaction-info (signed->json stx)))
      (check-equal? (transaction-info->signed-transaction info) stx (~a stx)))
    (for ([json (in-list (list transaction-info-json/0 transaction-info-json/1
                               transaction-info-json/2
                               (hash-remove transaction-info-json/0 'chainId)))])
      (define stx (transaction-info->signed-transaction (jsexpr->transaction-info json)))
      (check-equal? (signed-transaction-hash stx) (hash-ref json 'hash))
      (check-equal? (signed-transaction-sender stx) alice)))

  (define legacy-s #x17e6be4dd1cdb05aa9f72da7d69aa2af9d5c619ad2d29b538f7cf16e40a21411)

  (define hostile-conversions
    `([2 ((type "0x3")) "^transaction-info->signed-transaction: type 3 is not supported$"]
      [2 ((type "0x4")) "^transaction-info->signed-transaction: type 4 is not supported$"]
      [2 ((type "0x7e")) "^transaction-info->signed-transaction: type 126 is not supported$"]
      [2 ((type remove)) "^transaction-info->signed-transaction: malformed transaction type from the node: 'null$"]
      [0 ((chainId "0x1")) "^transaction-info->signed-transaction: malformed transaction chain id from the node: 1$"]
      [0 ((v "0x1b")) "^transaction-info->signed-transaction: malformed transaction chain id from the node: 31337$"]
      [0 ((v "0x5") (chainId remove)) "^transaction-info->signed-transaction: malformed transaction y parity from the node: 5$"]
      [1 ((yParity "0x2")) "^jsexpr->transaction-info: malformed transaction y parity from the node: \"0x2\"$"]
      [1 ((yParity "0x0")) "^transaction-info->signed-transaction: malformed transaction y parity from the node: 0$"]
      [2 ((v "0x2") (yParity remove)) "^transaction-info->signed-transaction: malformed transaction y parity from the node: 2$"]
      [2 ((v remove) (yParity remove)) "^transaction-info->signed-transaction: malformed transaction y parity from the node: 'null$"]
      [0 ((gasPrice remove)) "^transaction-info->signed-transaction: malformed transaction gas price from the node: 'null$"]
      [1 ((accessList null)) "^transaction-info->signed-transaction: malformed transaction access list from the node: 'null$"]
      [2 ((chainId remove)) "^transaction-info->signed-transaction: malformed transaction chain id from the node: 'null$"]
      [2 ((maxFeePerGas remove)) "^transaction-info->signed-transaction: malformed transaction max fee per gas from the node: 'null$"]
      [2 ((maxFeePerGas "0x1")) "^transaction-info->signed-transaction: malformed transaction max priority fee per gas from the node: 2000000000$"]
      [0 ((r "0x0")) "^transaction-info->signed-transaction: malformed transaction r from the node: 0$"]
      [0 ((r remove)) "^transaction-info->signed-transaction: malformed transaction r from the node: 'null$"]
      [0 ((s ,(quantity secp256k1-order))) "^transaction-info->signed-transaction: malformed transaction s from the node: [0-9]+$"]
      [0 ((s ,(quantity (- secp256k1-order legacy-s))) (v "0xf4f5")) "^signed-transaction: s is more than half the curve order"]
      [0 ((hash ,(filled-hash #\c))) ,(~a "^transaction-info->signed-transaction: the node's hash 0xc+ is not the transaction's "
                                          legacy-hash "$")]
      [0 ((from ,bob)) ,(~a "^transaction-info->signed-transaction: the node's from " bob
                            " is not the signature's sender " alice "$")]))

  (define bases (vector transaction-info-json/0 transaction-info-json/1 transaction-info-json/2))

  (define (edited json edits)
    (for/fold ([json json]) ([edit (in-list edits)])
      (match edit
        [(list key 'remove) (hash-remove json key)]
        [(list key value) (hash-set json key value)])))

  (test-case "T5: a transaction the node describes badly raises exn:fail:web3"
    (for ([row (in-list hostile-conversions)])
      (match-define (list base edits pattern) row)
      (define json (edited (vector-ref bases base) edits))
      (check-regexp-match (pregexp pattern)
                          (message-of (lambda ()
                                        (transaction-info->signed-transaction
                                         (jsexpr->transaction-info json))))
                          (~s edits))))

  (test-case "T6: a transaction prints its hash, type, nonce and place in every mode"
    (define info (jsexpr->transaction-info transaction-info-json/1))
    (define text (~a "#<transaction-info " (hash-ref transaction-info-json/1 'hash)
                     " type 1 nonce 1 block 1 index 1>"))
    (for ([mode (in-list (list ~a ~s ~v))])
      (check-equal? (mode info) text)
      (check-equal? (mode (list info)) (if (eq? mode ~v) (~a "(list " text ")") (~a "(" text ")"))))
    (check-equal? (~a (jsexpr->transaction-info (hash-remove transaction-info-json/0 'type)))
                  (~a "#<transaction-info " legacy-hash " nonce 0 block 1 index 0>")))

  (test-case "T6: transaction-info procedures blame their caller"
    (check-blames-here (transaction-info-hash 42))
    (check-blames-here (transaction-info->jsexpr (hasheq)))
    (check-blames-here (transaction-info->signed-transaction transaction-info-json/0)))

  (define (answering result)
    (mock-provider (hash "eth_getTransactionByHash" result)))

  (define shouted (string-append "0x" (string-upcase (substring legacy-hash 2))))

  (test-case "eth-get-transaction-by-hash reads the transaction asked for"
    (check-equal? (eth-get-transaction-by-hash shouted #:provider (answering transaction-info-json/0))
                  (jsexpr->transaction-info transaction-info-json/0))
    (check-equal? (message-of (lambda ()
                                (eth-get-transaction-by-hash
                                 (filled-hash #\c) #:provider (answering transaction-info-json/0))))
                  (~a "eth_getTransactionByHash: malformed transaction hash from the node: "
                      (~s legacy-hash))))

  (test-case "every accessor the field table provides is in web3 and the manual"
    (define manual (file->string (collection-file-path "scribblings/web3.scrbl" "web3")))
    (define names (exported-names 'web3))
    (for ([name (in-list (append (map accessor-name provided-rows)
                                 '(transaction-info? jsexpr->transaction-info
                                   transaction-info->jsexpr
                                   transaction-info->signed-transaction)))])
      (check-not-false (memq name names) (~a name))
      (check-not-false (regexp-match? (regexp (~a "@defproc\\[\\(" (regexp-quote (~a name)) " "))
                                      manual)
                       (~a name)))))
