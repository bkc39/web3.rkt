#lang racket/base

;; 02: driving a local anvil node.
;;
;; Install code and storage directly, snapshot and revert the chain, send
;; from any address, and trace a call opcode by opcode.
;;
;;   racket examples/02-anvil.rkt
;;   racket -i -e '(enter! "examples/02-anvil.rkt")'

(require web3
         "common.rkt")

(provide (all-defined-out))

(section "the node")

;; `with-anvil` scopes a node to a body; `start-anvil` keeps one around.
(define node (start-anvil #:hardfork "osaka"))
(current-provider (anvil-provider node))

(show (anvil-port node))
(show (anvil-running? node))
(show (path->string (anvil-log-file node)))

(define contract "0x00000000000000000000000000000000000c0de0")
(define stranger "0x000000000000000000000000000000000000beef")

(section "install code, then call it")

;; PUSH1 42, PUSH1 0, MSTORE, PUSH1 32, PUSH1 0, RETURN
(define return-42 "0x602a60005260206000f3")

(anvil-set-code contract return-42)
(show (eth-get-code contract "latest"))

(define returned (eth-call (hasheq 'to contract 'data "0x") "latest"))
(show returned)
(show (bytes->words (hex-string->bytes returned)))

(section "reverts carry their data")

;; revert with Error("oops!")
(define revert-oops
  (string-append
   "0x"
   "7f08c379a0000000000000000000000000000000000000000000000000000000005f52"
   "60206004526005602452"
   "646f6f70732160d81b604452"
   "60645ffd"))

(anvil-set-code contract revert-oops)

(show-error (eth-call (hasheq 'to contract 'data "0x") "latest") #:as revert-exn)
(show (exn:fail:web3:rpc-data revert-exn))
(show (decode-revert-reason (hex-string->bytes (exn:fail:web3:rpc-data revert-exn))))

(section "storage, snapshots and revert")

;; sstore(0, calldataload(0)); stop
(define store-arg "0x5f355f5500")
(define me (car (eth-accounts)))

(anvil-set-code contract store-arg)
(define snapshot (evm-snapshot))

(define receipt
  (send-transaction/receipt
   (hasheq 'from me 'to contract 'data (bytes->hex-string (word->bytes 7)))))

(show (receipt-succeeded? receipt))
(show (hex-string->number (eth-get-storage-at contract 0 "latest")))

(show (evm-revert snapshot))
(show (hex-string->number (eth-get-storage-at contract 0 "latest")))

;; Or write the slot directly.
(anvil-set-storage-at contract 0 (bytes->hex-string (word->bytes 99)))
(show (hex-string->number (eth-get-storage-at contract 0 "latest")))

(section "balances and impersonation")

(anvil-set-balance stranger (expt 10 18))
(show (eth-get-balance stranger "latest"))

;; Send from an address whose key nobody holds.
(anvil-impersonate-account stranger)
(define impersonated
  (send-transaction/receipt
   (hasheq 'from stranger 'to me 'value (number->hex-string 1000))))
(anvil-stop-impersonating-account stranger)

(show (hash-ref impersonated 'from))
(show (receipt-succeeded? impersonated))

(section "mining")

(define height (eth-block-number))
(anvil-mine)
(show (- (eth-block-number) height))

(section "tracing")

(anvil-set-code contract return-42)
(define trace
  (debug-trace-call (hasheq 'to contract 'data "0x") "latest" (hasheq)))

(show (for/list ([step (in-list (hash-ref trace 'structLogs))])
        (list (hash-ref step 'pc) (hash-ref step 'op) (hash-ref step 'gasCost))))
(show (hash-ref trace 'returnValue))
