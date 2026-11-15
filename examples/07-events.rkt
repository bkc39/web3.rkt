#lang racket/base

;; 07: the logs contracts emit: describing events, decoding logs, and
;; fetching them by filter.
;;
;; The contracts are examples/contracts/Token.sol and Events.sol.
;;
;;   racket examples/07-events.rkt
;;   racket -i -e '(enter! "examples/07-events.rkt")'

(require racket/list
         racket/match
         web3
         "common.rkt")

(provide (all-defined-out))

(section "the node, three accounts and a token")

(define node (start-anvil))
(current-provider (anvil-provider node))

(match-define (list alice bob carol _ ...) (map address->checksum-string (eth-accounts)))
(current-account alice)
(define token
  (deploy-contract (artifact-bytecode "Token") "constructor(string,string,uint256)"
                   "Token" "TOK" 1000000))
(show token)
(show (contract-call token "balanceOf(address)(uint256)" alice))

(section "describing an event: a string, an s-expression or a struct")

(define transfer
  (string->abi-event "event Transfer(address indexed from, address indexed to, uint256 value)"))
(show transfer)
(show (equal? transfer
              (sexp->abi-event '(event Transfer ([from address #:indexed]
                                                 [to address #:indexed]
                                                 [value uint256])))))
(show (equal? transfer
              (abi-event "Transfer"
                         (list (abi-event-parameter "from" 'address #t)
                               (abi-event-parameter "to" 'address #t)
                               (abi-event-parameter "value" 'uint256 #f))
                         #f)))
(show (abi-event->string '(event Deposit ([from address #:indexed] [value uint256] [memo string]))))
(show (event-topic transfer))

(section "one transfer: its receipt's log, decoded")

(define sent-receipt (contract-send token "transfer(address,uint256)" bob 250))
(define raw (first (receipt-logs sent-receipt)))
(show raw)
(define sent (decode-log transfer raw))
(show (event-log-arguments sent))
(show (event-log-ref sent 'to))
(show (event-log-ref sent 'value))

(section "every transfer of the token, from get-logs")

(for ([from (list alice alice bob)]
      [to (list carol bob carol)]
      [value '(100 50 25)])
  (parameterize ([current-account from])
    (contract-send token "transfer(address,uint256)" to value)))
(define transfers (log-filter #:address token #:event transfer))
(show (get-logs transfers))

(section "filtered by an indexed argument")

(define to-bob (log-filter #:address token #:event transfer #:where (hasheq 'to bob)))
(show (log-filter->jsexpr to-bob))
(show (map event-log-arguments (get-logs to-bob)))
(show (map event-log-arguments
           (get-logs (log-filter #:address token #:event transfer
                                 #:where (hasheq 'to (any-of bob carol))))))

(section "the mint: a transfer from the zero address")

(define zero-address "0x0000000000000000000000000000000000000000")
(show (map event-log-arguments
           (get-logs (log-filter #:address token #:event transfer
                                 #:where (hasheq 'from zero-address)))))

(section "a block range, and fetching in chunks")

(define start (log-block-number raw))
(show (map log-block-number
           (get-logs (log-filter #:address token #:event transfer
                                 #:from-block start #:to-block (+ start 2)))))
(show (equal? (get-logs transfers) (get-logs transfers #:chunk 2)))

(section "hashed indexed values")

(define events (deploy-contract (artifact-bytecode "Events")))
(define tagged
  "Tagged(string indexed tag, bytes indexed blob, address indexed sender, string text)")
(define tag-receipt (contract-send events "tag(string,bytes,string)" "hello" #"\1\2" "body"))
(show (map event-log-arguments (receipt-logs tag-receipt #:events (list tagged))))
(show (abi-value->topic 'string "hello"))
(show (length (get-logs (log-filter #:address events #:event tagged
                                    #:where (hasheq 'tag "hello")))))

(section "several events at once: anonymous, and sharing topic 0")

(define note "Note(address indexed sender, uint256 value) anonymous")
(show (event-topic note))
(define note-receipt (contract-send events "note(uint256)" 9))
(show (map event-log? (receipt-logs note-receipt #:events (list note))))
(define transfer-721 "Transfer(address indexed from, address indexed to, uint256 indexed tokenId)")
(show (equal? (event-topic transfer) (event-topic transfer-721)))
(define nft-receipt (contract-send events "transferNft(address,uint256)" bob 7))
(show (map event-log-arguments (receipt-logs nft-receipt #:events (list transfer transfer-721))))
(define deposit "Deposit(address indexed from, uint256 value, string memo)")
(define deposit-receipt (contract-send token "deposit(string)" "first" #:value 1000))
(show (map event-log-arguments (receipt-logs deposit-receipt #:events (list deposit))))
(define decoded (get-logs (log-filter) #:events (list transfer transfer-721 deposit tagged note)))
(show (map abi-event->string (remove-duplicates (map event-log-event (filter event-log? decoded)))))
(show (count event-log? decoded))
(show (length decoded))

(section "errors")

(show-error (decode-log deposit raw))
(show-error (string->abi-event
             "E(address indexed from, address indexed to, address indexed x, uint256 indexed v)"))
(show-error (log-filter #:block-hash (log-block-hash raw) #:from-block 1))
