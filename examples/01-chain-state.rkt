#lang racket/base

;; 01: reading chain state and sending a transaction.
;;
;; Quantities are exact integers (wei, block numbers, gas).  Addresses,
;; hashes and data are "0x..." strings.  Blocks, transactions and receipts
;; are the node's JSON as hash tables.
;;
;;   racket examples/01-chain-state.rkt
;;   racket -i -e '(enter! "examples/01-chain-state.rkt")'

(require web3
         "common.rkt")

(provide (all-defined-out))

(define node (start-anvil))
(current-provider (anvil-provider node))

(define ether (expt 10 18))

(section "accounts and balances")

;; anvil holds the keys for these accounts and signs for them.
(define accounts (eth-accounts))
(define alice (car accounts))
(define bob (cadr accounts))

(show (length accounts))
(show alice)
(show (eth-get-balance alice "latest"))
(show (/ (eth-get-balance alice "latest") ether))
(show (eth-get-transaction-count alice "latest"))

(section "blocks")

(show (eth-block-number))
(define genesis (eth-get-block-by-number 0 #f))
(show (hash-ref genesis 'hash))
(show (hex-string->number (hash-ref genesis 'gasLimit)))
(show (sort (hash-keys genesis) symbol<?))

(section "send one ether from alice to bob")

(define tx
  (hasheq 'from alice
          'to bob
          'value (number->hex-string ether)))

(show (eth-estimate-gas tx))
(show (eth-gas-price))

(define tx-hash (eth-send-transaction tx))
(show tx-hash)

(define receipt (wait-for-receipt tx-hash))
(show (receipt-succeeded? receipt))
(show (hex-string->number (hash-ref receipt 'gasUsed)))
(show (hex-string->number (hash-ref receipt 'blockNumber)))

(show (/ (eth-get-balance bob "latest") ether))
(show (eth-get-transaction-count alice "latest"))

(section "the transaction and its block")

(define sent (eth-get-transaction-by-hash tx-hash))
(show (hash-ref sent 'from))
(show (hex-string->number (hash-ref sent 'value)))

;; #t asks for full transaction objects instead of hashes.
(define block (eth-get-block-by-number (eth-block-number) #t))
(show (length (hash-ref block 'transactions)))

(section "code and storage")

;; An account without code.
(show (eth-get-code bob "latest"))
(show (eth-get-storage-at bob 0 "latest"))
