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
       (dynamic-wind
        void
        (lambda ()
          (check-true (anvil-running? node))
          (check-equal? (http-provider-url (anvil-provider node))
                        (format "http://127.0.0.1:~a" (anvil-port node)))
          (check-true (file-exists? (anvil-log-file node)))
          (check-equal? (eth-chain-id #:provider (anvil-provider node)) 31337))
        (lambda () (stop-anvil node)))
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
           (send-transaction/receipt me contract #:data (word->bytes 7)))
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

       (test-case "a plain transfer's gas estimate is 21000"
         (check-equal? (eth-estimate-gas (hasheq 'from me 'to stranger 'value "0x1")) 21000))

       (test-case "a block lists its transactions' hashes, or the transactions themselves"
         (define tx-hash (eth-send-transaction (hasheq 'from me 'to stranger 'value "0x1")))
         (define height (receipt-block-number (wait-for-receipt tx-hash)))
         (check-equal? (block-transactions (eth-get-block-by-number height #f)) (list tx-hash))
         (check-equal? (for/list ([full (in-list (block-transactions
                                                  (eth-get-block-by-number height #t)))])
                         (list (transaction-info-hash full) (transaction-info-value full)))
                       (list (list tx-hash 1))))

       (test-case "an address without code has code 0x"
         (check-equal? (eth-get-code stranger "latest") "0x"))

       (test-case "anvil-set-balance sets an address's balance"
         (anvil-set-balance stranger (expt 10 18))
         (check-equal? (eth-get-balance stranger "latest") (expt 10 18)))

       (test-case "anvil-set-storage-at sets a storage slot"
         (anvil-set-storage-at contract 3 (bytes->hex-string (word->bytes 99)))
         (check-equal? (hex-string->number (eth-get-storage-at contract 3 "latest")) 99))

       (test-case "an impersonated address sends until impersonation stops"
         (anvil-set-balance stranger (expt 10 18))
         (define payment (hasheq 'from stranger 'to me 'value (number->hex-string 1000)))
         (anvil-impersonate-account stranger)
         (check-true (receipt-succeeded? (send-transaction/receipt stranger me #:value 1000)))
         (anvil-stop-impersonating-account stranger)
         (check-exn exn:fail:web3:rpc? (lambda () (eth-send-transaction payment))))

       (test-case "anvil-mine mines one block"
         (define height (eth-block-number))
         (anvil-mine)
         (check-equal? (eth-block-number) (add1 height)))

       (test-case "encode-call matches the selector"
         (check-equal? (subbytes (encode-call "get()") 0 4) (function-selector "get()"))))]))
