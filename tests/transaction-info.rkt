#lang racket/base

(module+ test
  (require racket/format
           (only-in racket/match match match-define)
           rackunit
           (only-in web3/account secp256k1-order signature)
           (only-in web3/eth eth-get-transaction-by-hash)
           web3/eth/transaction-info
           (only-in (submod web3/eth/transaction-info internal) transaction-info-fields)
           (only-in web3/private/exn exn:fail:web3:signature?)
           (only-in web3/rpc hex-string->number mock-provider number->hex-string)
           web3/transaction
           (only-in "helpers/records.rkt"
                    check-fields check-in-web3-and-manual message-of provided-accessors)
           (only-in "helpers/results.rkt"
                    transaction-info-json/0
                    transaction-info-json/1
                    transaction-info-json/2)
           (only-in "helpers/transactions.rkt" example-key example-transactions nick-raw no-point-r)
           "util.rkt")

  (define alice "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")
  (define bob "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")
  (define legacy-hash (hash-ref transaction-info-json/0 'hash))
  (define block-hash "0x56268c1258891dd669aacbe1c854f83204063dd979b3207267179897f8e45d7c")

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

  (test-case "T1: a node's transaction reads into its fields"
    (define info (jsexpr->transaction-info transaction-info-json/0))
    (check-fields info "transaction-info" transaction-info-fields expected-fields)
    (check-eq? (transaction-info->jsexpr info) transaction-info-json/0)
    (check-equal? (transaction-info-access-list (jsexpr->transaction-info transaction-info-json/1))
                  (list (list bob (make-bytes 32 1))))
    (check-regexp-match #rx"^jsexpr->transaction-info: malformed transaction y parity from the node"
                        (message-of (lambda ()
                                      (jsexpr->transaction-info
                                       (hash-set transaction-info-json/1 'yParity "0x2"))))))

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

  (define (signed->json signed)
    (define transaction (signed-transaction-transaction signed))
    (match-define (signature signature-r signature-s y-parity) (signed-transaction-signature signed))
    (define request (transaction->jsexpr transaction #:from (signed-transaction-sender signed)))
    (define common
      (hash-set* (hash-remove request 'data)
                 'input (hash-ref request 'data)
                 'hash (signed-transaction-hash signed)
                 'r (number->hex-string signature-r)
                 's (number->hex-string signature-s)))
    (match (cons (transaction-type transaction) (transaction-chain-id transaction))
      [(cons 0 #f) (hash-set common 'v (number->hex-string (+ 27 y-parity)))]
      [(cons 0 chain-id) (hash-set common 'v (number->hex-string (+ 35 (* 2 chain-id) y-parity)))]
      [_ (hash-set* common
                    'v (number->hex-string y-parity)
                    'yParity (number->hex-string y-parity))]))

  (define signed-examples
    (append (for/list ([transaction (in-list example-transactions)])
              (sign-transaction example-key transaction))
            (list (bytes->signed-transaction nick-raw))))

  (define (converted json)
    (transaction-info->signed-transaction (jsexpr->transaction-info json)))

  (test-case "T4: transaction-info->signed-transaction rebuilds the transaction"
    (for ([signed (in-list signed-examples)])
      (check-equal? (converted (signed->json signed)) signed (~a signed)))
    (for ([json (in-list (list transaction-info-json/0 transaction-info-json/1
                               transaction-info-json/2
                               (hash-remove transaction-info-json/0 'chainId)))])
      (define signed (converted json))
      (check-equal? (signed-transaction-hash signed) (hash-ref json 'hash))
      (check-equal? (signed-transaction-sender signed) alice)))

  (test-case "T4: a transaction without a type is legacy unless a typed field says otherwise"
    (check-equal? (converted (hash-remove transaction-info-json/0 'type))
                  (converted transaction-info-json/0))
    (for ([key (in-list '(maxFeePerGas maxPriorityFeePerGas yParity))])
      (check-equal? (message-of (lambda ()
                                  (converted (hash-set (hash-remove transaction-info-json/0 'type)
                                                       key "0x1"))))
                    (~a "transaction-info->signed-transaction: malformed transaction type from the "
                        "node: 'null")
                    (~a key)))
    (check-regexp-match #rx"malformed transaction type"
                        (message-of (lambda ()
                                      (converted (hash-remove transaction-info-json/1 'type))))))

  (define legacy-s (hex-string->number (hash-ref transaction-info-json/0 's)))

  (define malformed-conversions
    `([2 ((type remove)) "type" "'null"]
      [0 ((chainId "0x1")) "chain id" "1"]
      [0 ((v "0x1b")) "chain id" "31337"]
      [0 ((v "0x5") (chainId remove)) "v" "5"]
      [0 ((v "0x0")) "v" "0"]
      [0 ((v "0x1")) "v" "1"]
      [0 ((v ,(number->hex-string (expt 2 70)))) "v" ,(~a (expt 2 70))]
      [1 ((yParity "0x0")) "y parity" "0"]
      [2 ((v "0x2") (yParity remove)) "v" "2"]
      [2 ((v remove) (yParity remove)) "y parity" "'null"]
      [0 ((gasPrice remove)) "gas price" "'null"]
      [1 ((accessList null)) "access list" "'null"]
      [2 ((chainId remove)) "chain id" "'null"]
      [2 ((maxFeePerGas remove)) "max fee per gas" "'null"]
      [2 ((maxFeePerGas "0x1")) "max priority fee per gas" "2000000000"]
      [0 ((r "0x0")) "r" "0"]
      [0 ((r remove)) "r" "'null"]
      [0 ((s ,(number->hex-string secp256k1-order))) "s" ,(~a secp256k1-order)]))

  (define refused-conversions
    `([2 ((type "0x3")) "type 3 is not supported"]
      [2 ((type "0x4")) "type 4 is not supported"]
      [2 ((type "0x7e")) "type 126 is not supported"]
      [0 ((hash ,(filled-hash #\c)))
         ,(~a "the node's hash " (filled-hash #\c) " is not the transaction's " legacy-hash)]
      [0 ((from ,bob)) ,(~a "the node's from " bob " is not the signature's sender " alice)]))

  (define signature-failures
    `([0 ((s ,(number->hex-string (- secp256k1-order legacy-s))) (v "0xf4f5"))
         "s is more than half the curve order (EIP-2)"]
      [0 ((r ,(number->hex-string (no-point-r)))) "the signature recovers no address"]))

  (define bases (vector transaction-info-json/0 transaction-info-json/1 transaction-info-json/2))

  (define (edited base edits)
    (for/fold ([json (vector-ref bases base)]) ([edit (in-list edits)])
      (match edit
        [(list key 'remove) (hash-remove json key)]
        [(list key value) (hash-set json key value)])))

  (define converter-prefix "transaction-info->signed-transaction: ")

  (test-case "T5: a field the node gets wrong raises exn:fail:web3 naming it"
    (for ([row (in-list malformed-conversions)])
      (match-define (list base edits what shown) row)
      (check-equal? (message-of (lambda () (converted (edited base edits))))
                    (~a converter-prefix "malformed transaction " what " from the node: " shown)
                    (~s edits))))

  (test-case "T5: an unsupported type or a false claim raises exn:fail:web3"
    (for ([row (in-list refused-conversions)])
      (match-define (list base edits message) row)
      (check-equal? (message-of (lambda () (converted (edited base edits))))
                    (~a converter-prefix message)
                    (~s edits))))

  (test-case "T5: a bad signature raises exn:fail:web3:signature under the converter's name"
    (for ([row (in-list signature-failures)])
      (match-define (list base edits message) row)
      (define e (raised (lambda () (converted (edited base edits)))))
      (check-pred exn:fail:web3:signature? e (~s edits))
      (check-equal? (and e (exn-message e)) (~a converter-prefix message) (~s edits))))

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

  (test-case "every accessor the transaction table provides is in web3 and the manual"
    (check-in-web3-and-manual
     (append (provided-accessors "transaction-info" transaction-info-fields)
             '(transaction-info? jsexpr->transaction-info transaction-info->jsexpr
               transaction-info->signed-transaction)))))
