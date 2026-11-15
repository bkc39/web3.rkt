#lang racket/base

;; 07: the logs contracts emit: describing events, decoding logs, and
;; fetching them by filter.
;;
;; The contracts are examples/contracts/Token.sol and Events.sol.
;;
;;   racket examples/07-events.rkt
;;   racket -i -e '(enter! "examples/07-events.rkt")'

(require racket/list
         web3
         "common.rkt")

(provide (all-defined-out))

(section "the node, three accounts and a token")

(define node (start-anvil))
(current-provider (anvil-provider node))

(define-values (alice bob carol) (apply values (map address->checksum-string (take (eth-accounts) 3))))
(current-account alice)
(define token (deploy-contract (artifact-bytecode "Token") "constructor(string,string,uint256)" "Token" "TOK" 1000000))

(section "describing an event")

(define transfer (string->abi-event "event Transfer(address indexed from, address indexed to, uint256 value)"))
(show transfer)
(show (equal? transfer
              (sexp->abi-event '(event Transfer ([from address #:indexed] [to address #:indexed] [value uint256])))))
(show (equal? transfer
              (abi-event "Transfer"
                         (list (abi-event-parameter "from" 'address #t)
                               (abi-event-parameter "to" 'address #t)
                               (abi-event-parameter "value" 'uint256 #f))
                         #f)))
(show (abi-event->string transfer))
(show (abi-event->sexp transfer))
(show (abi-signature->string (abi-event->signature transfer)))
(show (event-topic transfer))

;; A string works wherever an event goes.
(define deposit "Deposit(address indexed from, uint256 value, string memo)")
(show (event-topic deposit))

(section "the logs of a receipt")

(define sent-receipt (contract-send token "transfer(address,uint256)" bob 250))
(show (receipt-logs sent-receipt))
(define raw (car (receipt-logs sent-receipt)))
(show (log-topics raw))
(show (log-data raw))
(show (log-block-number raw))
(show (log-address raw))

(section "decoded")

(define sent (decode-log transfer raw))
(show sent)
(show (event-log-name sent))
(show (event-log-arguments sent))
(show (event-log-ref sent 'value))
(show (event-log-ref sent 'to))
;; A decoded log is a log.
(show (log-transaction-hash sent))
(show (receipt-logs sent-receipt #:events (list transfer)))

(section "more activity")

(define to-carol (contract-send token "transfer(address,uint256)" carol 100))
(define to-bob (contract-send token "transfer(address,uint256)" bob 50))
(define bob-to-carol (parameterize ([current-account bob]) (contract-send token "transfer(address,uint256)" carol 25)))
(define first-deposit (contract-send token "deposit(string)" "first" #:value 1000))
(for ([_ (in-range 3)]) (anvil-mine))
(define second-deposit (contract-send token "deposit(string)" "second" #:value 2000))

(section "all transfers")

(define transfers (log-filter #:address token #:event transfer))
(show (log-filter->jsexpr transfers))
;; The first is the constructor's mint.
(show (get-logs transfers))

(section "filtered by an indexed argument")

(show (log-filter->jsexpr (log-filter #:address token #:event transfer #:where (hasheq 'to bob))))
(show (get-logs (log-filter #:address token #:event transfer #:where (hasheq 'to bob))))
(show (get-logs (log-filter #:address token #:event transfer #:where (hasheq 'to (any-of bob carol)))))

(section "the mint in the constructor")

(define zero "0x0000000000000000000000000000000000000000")
(show (get-logs (log-filter #:address token #:event transfer #:where (hasheq 'from zero))))

(section "a block range")

(define start (log-block-number raw))
(show (get-logs (log-filter #:address token #:event transfer #:from-block start #:to-block (+ start 2))))
;; The same logs, fetched two blocks at a time.
(show (equal? (get-logs transfers) (get-logs transfers #:chunk 2)))

(section "several events at once")

(show (get-logs (log-filter #:address token) #:events (list transfer deposit)))

(section "logs nobody described, and dynamic indexed values")

(define events (deploy-contract (artifact-bytecode "Events")))
(define tagged (contract-send events "tag(string,bytes,string)" "hello" #"\1\2" "body"))
(define tagged-event "Tagged(string indexed tag, bytes indexed blob, address indexed sender, string text)")
(show (receipt-logs tagged #:events (list transfer deposit)))
(show (receipt-logs tagged #:events (list tagged-event)))
(show (abi-value->topic 'string "hello"))
(show (get-logs (log-filter #:address events #:event tagged-event #:where (hasheq 'tag "hello"))))

(section "one topic, two events")

(define nft (contract-send events "transferNft(address,uint256)" bob 7))
;; Both contracts, told apart by their number of topics.
(show (get-logs (log-filter #:event transfer)
                #:events (list transfer "Transfer(address indexed from, address indexed to, uint256 indexed tokenId)")))

(section "an anonymous event")

(define note "Note(address indexed sender, uint256 value) anonymous")
(show (event-topic note))
(define noted (contract-send events "note(uint256)" 9))
;; Four, the second log, stays undecoded.
(show (receipt-logs noted #:events (list note)))

(section "errors")

(show-error (decode-log deposit raw))
(show-error (string->abi-event "Transfer(address indexed from, address indexed to, address indexed x, uint256 indexed v)"))
(show-error (log-filter #:block-hash (log-block-hash raw) #:from-block 1))
