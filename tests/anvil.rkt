#lang racket/base

;; Integration tests against a throwaway anvil.  Skipped when anvil is not
;; available (set WEB3_ANVIL, or put anvil on PATH; `nix develop` does).

(module+ test
  (require rackunit
           web3)

  ;; PUSH1 42, PUSH1 0, MSTORE, PUSH1 32, PUSH1 0, RETURN
  (define return-42 "0x602a60005260206000f3")
  ;; revert with Error("oops!")
  (define revert-oops
    (string-append
     "0x"
     "7f08c379a0000000000000000000000000000000000000000000000000000000005f52" ; selector at 0
     "60206004526005602452"                                                   ; offset 32, length 5
     "646f6f70732160d81b604452"                                               ; "oops!" at 68
     "60645ffd"))                                                             ; revert(0, 100)
  ;; sstore(0, calldataload(0)); stop
  (define store-arg "0x5f355f5500")

  (define contract "0x00000000000000000000000000000000000c0de0")
  (define stranger "0x000000000000000000000000000000000000beef")

  (cond
    [(not (current-anvil-path))
     (displayln "skipping anvil tests: anvil not found (set WEB3_ANVIL or use `nix develop`)")]
    [else
     (test-case "a started node's port, provider and log file, and its process until stopped"
       (define node (start-anvil))
       (check-true (anvil-running? node))
       (check-equal? (http-provider-url (anvil-provider node))
                     (format "http://127.0.0.1:~a" (anvil-port node)))
       (check-true (file-exists? (anvil-log-file node)))
       (check-equal? (eth-chain-id #:provider (anvil-provider node)) 31337)
       (stop-anvil node)
       (check-false (anvil-running? node)))

     (with-anvil ()
       (define me (car (eth-accounts)))

       (test-case "chain basics"
         (check-equal? (eth-chain-id) 31337)
         (check-equal? (length (eth-accounts)) 10)
         (check-true (> (eth-get-balance me "latest") 0))
         (check-pred exact-nonnegative-integer? (eth-block-number))
         (check-pred string? (web3-client-version)))

       (test-case "install code and call it"
         (anvil-set-code contract return-42)
         (check-equal? (eth-get-code contract "latest") return-42)
         (check-equal? (bytes->words (hex-string->bytes
                                      (eth-call (hasheq 'to contract 'data "0x") "latest")))
                       '(42)))

       (test-case "reverts carry their data"
         (anvil-set-code contract revert-oops)
         (define e
           (with-handlers ([exn:fail:web3:rpc? values])
             (eth-call (hasheq 'to contract 'data "0x") "latest")))
         (check-pred exn:fail:web3:rpc? e)
         (check-equal? (decode-revert-reason (hex-string->bytes (exn:fail:web3:rpc-data e)))
                       "Error(\"oops!\")"))

       (test-case "transactions, receipts, storage, snapshots"
         (anvil-set-code contract store-arg)
         (define snap (evm-snapshot))
         (define receipt
           (send-transaction/receipt
            (hasheq 'from me 'to contract 'data (bytes->hex-string (word->bytes 7)))))
         (check-true (receipt-succeeded? receipt))
         (check-equal? (hex-string->number (eth-get-storage-at contract 0 "latest")) 7)
         (check-true (evm-revert snap))
         (check-equal? (hex-string->number (eth-get-storage-at contract 0 "latest")) 0))

       (test-case "tracing"
         (anvil-set-code contract return-42)
         (define trace (debug-trace-call (hasheq 'to contract 'data "0x") "latest" (hasheq)))
         (check-equal? (map (lambda (s) (hash-ref s 'op)) (hash-ref trace 'structLogs))
                       '("PUSH1" "PUSH1" "MSTORE" "PUSH1" "PUSH1" "RETURN"))
         (check-equal? (hex-string->number (hash-ref trace 'returnValue)) 42))

       (test-case "a gas estimate, a block with its transactions, and an account without code"
         (define tx (hasheq 'from me 'to stranger 'value (number->hex-string 1)))
         (check-equal? (eth-estimate-gas tx) 21000)
         (define tx-hash (eth-send-transaction tx))
         (define height (hex-string->number (hash-ref (wait-for-receipt tx-hash) 'blockNumber)))
         (check-equal? (hash-ref (eth-get-block-by-number height #f) 'transactions) (list tx-hash))
         (check-equal? (for/list ([full (in-list (hash-ref (eth-get-block-by-number height #t)
                                                           'transactions))])
                         (list (hash-ref full 'hash) (hash-ref full 'value)))
                       (list (list tx-hash "0x1")))
         (check-equal? (eth-get-code stranger "latest") "0x"))

       (test-case "balances, storage, impersonation and blocks set by anvil's methods"
         (anvil-set-balance stranger (expt 10 18))
         (check-equal? (eth-get-balance stranger "latest") (expt 10 18))
         (anvil-set-storage-at contract 3 (bytes->hex-string (word->bytes 99)))
         (check-equal? (hex-string->number (eth-get-storage-at contract 3 "latest")) 99)
         (define payment (hasheq 'from stranger 'to me 'value (number->hex-string 1000)))
         (anvil-impersonate-account stranger)
         (check-true (receipt-succeeded? (send-transaction/receipt payment)))
         (anvil-stop-impersonating-account stranger)
         (check-exn exn:fail:web3:rpc? (lambda () (eth-send-transaction payment)))
         (define height (eth-block-number))
         (anvil-mine)
         (check-equal? (eth-block-number) (add1 height)))

       (test-case "encode-call matches the selector"
         (check-equal? (subbytes (encode-call "get()") 0 4) (function-selector "get()"))))]))
