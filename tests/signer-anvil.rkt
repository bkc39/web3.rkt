#lang racket/base

(module+ test
  (require racket/list
           racket/match
           rackunit
           web3
           (only-in "abi-gen.rkt" random-address random-bytes)
           (only-in "helpers/accounts.rkt" anvil-addresses anvil-keys signature-hex)
           (only-in "helpers/transactions.rkt" slot-1)
           (only-in "util.rkt" seeded))

  (match-define (list key0 _ key2 _ ...) (map hex-string->secret-key anvil-keys))
  (match-define (list account0 _ _ account3 _ ...) anvil-addresses)

  (define prng (seeded 8 1 3 3 7 5))

  (define (fresh-address)
    (address->checksum-string (random-address prng)))

  (define pinned-messages
    `(("hello"
       ,(string-append "0xf16ea9a3478698f695fd1401bfe27e9e4a7e8e3da94aa72b021125e31fa899cc"
                       "573c48ea3fe1d4ab61a9db10c19032026e3ed2dbccba5a178235ac27f94504311c"))
      (""
       ,(string-append "0xc1977b761f1dd36c29795783460d241885c8e7f9d962dbe7bba2753fd94e89b4"
                       "44a1cd9ed855dd09afa3b73f7c2bd097ec9abc2d2775d737505a02d3f0cafa591b"))
      (#"\xde\xad\xbe\xef"
       ,(string-append "0xa114c834af73872c6c9efe918d85b0b1b34a486d10f9011e2630e28417c828c0"
                       "60dbd65cda67e73d52ebb7c555260621dbc1b0b4036acb61086bba091ac3f1641b"))
      ("0xdeadbeef"
       ,(string-append "0x74d15668584cce98918f02eccc9dec413a5a9b99b2e71b96d7b654bf036ccc48"
                       "418fbbff262f0f6480a9d46012f65b624352b8caf1b89897e7152199d3913ced1c"))
      ("héllo"
       ,(string-append "0x2b7e2472ce826ed1748e48d176e9b4c11103529bca61721fa87a9a0f78c9841f"
                       "73156d7a80d282db7f172a60d156f6dbdfa773c22f4c31d9605597ad51483a7d1c"))))

  (define signed-transactions
    (list (legacy-transaction #:chain-id 31337 #:nonce 3 #:gas-price (gwei->wei 10)
                              #:gas-limit 21000 #:to (fresh-address) #:value 7)
          (eip2930-transaction #:chain-id 31337 #:nonce 4 #:gas-price (gwei->wei 10)
                               #:gas-limit 30000 #:to (fresh-address) #:value 7
                               #:access-list (list (list (fresh-address) slot-1)))
          (eip1559-transaction #:chain-id 31337 #:nonce 5 #:max-priority-fee-per-gas (gwei->wei 1)
                               #:max-fee-per-gas (gwei->wei 10) #:gas-limit 21000
                               #:to (fresh-address) #:value (ether->wei 1))
          (eip1559-transaction #:chain-id 31337 #:nonce 6 #:max-priority-fee-per-gas (gwei->wei 1)
                               #:max-fee-per-gas (gwei->wei 10) #:gas-limit 100000 #:to #f
                               #:data (hex-string->bytes "0x602a60005260206000f3"))))

  (define (send-with signer)
    (define sender (signer-address signer))
    (define recipient (fresh-address))
    (define tx
      (eip1559-transaction #:chain-id (eth-chain-id)
                           #:nonce (eth-get-transaction-count sender "pending")
                           #:max-priority-fee-per-gas (gwei->wei 1)
                           #:max-fee-per-gas (gwei->wei 10)
                           #:gas-limit 21000 #:to recipient #:value (ether->wei 1)))
    (define stx (signer-sign-transaction signer tx))
    (define hash (eth-send-raw-transaction (bytes->hex-string (signed-transaction->bytes stx))))
    (define receipt (wait-for-receipt hash))
    (list stx hash receipt recipient))

  (define (rpc-refusal thunk)
    (with-handlers ([exn:fail:web3:rpc? (lambda (_) 'rpc)])
      (thunk)
      'signed))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping signer node tests: anvil not found")]
    [else
     (with-anvil ()
       (define node0 (node-signer (first (eth-accounts))))

       (test-case "N1: eth_sign is EIP-191 and deterministic: the node signs as sign-message"
         (for ([row (in-list pinned-messages)])
           (match-define (list message hex) row)
           (define sig (signer-sign-message node0 message))
           (check-equal? sig (sign-message key0 message))
           (check-equal? (signature-hex sig) hex))
         (define long (random-bytes prng 1000))
         (check-equal? (signer-sign-message node0 long) (sign-message key0 long)))

       (test-case "N2: eth_signTransaction honours every field and signs as sign-transaction"
         (for ([tx (in-list signed-transactions)])
           (check-equal? (signer-sign-transaction node0 tx) (sign-transaction key0 tx)
                         (format "~a" tx))))

       (test-case "N3: anvil signs a transaction without a chain id under its own chain id"
         (define pre-155
           (legacy-transaction #:chain-id #f #:nonce 0 #:gas-price 1 #:gas-limit 21000 #:to #f))
         (define e
           (with-handlers ([exn:fail:web3:signer? values])
             (signer-sign-transaction node0 pre-155)))
         (check-pred exn:fail:web3:signer? e)
         (check-equal? (exn:fail:web3:signer-reason e) 'wrong-transaction)
         (check-regexp-match #rx"differs in chain-id$" (exn-message e)))

       (test-case "N4: a local and a node signer send through the same procedure"
         (for ([signer (list (local-signer key2) (node-signer account3))])
           (define before (eth-get-balance (signer-address signer) "latest"))
           (match-define (list stx hash receipt recipient) (send-with signer))
           (check-true (receipt-succeeded? receipt))
           (check-equal? hash (signed-transaction-hash stx))
           (check-equal? (string-downcase (hash-ref (eth-get-transaction-by-hash hash) 'from))
                         (string-downcase (signer-address signer)))
           (check-equal? (eth-get-balance recipient "latest") (ether->wei 1))
           (check-true (< (eth-get-balance (signer-address signer) "latest") before))))

       (test-case "N5: an impersonated account cannot sign, but sends transaction->jsexpr"
         (define impersonated (fresh-address))
         (anvil-impersonate-account impersonated)
         (anvil-set-balance impersonated (ether->wei 10))
         (define signer (node-signer impersonated))
         (define tx
           (eip1559-transaction #:chain-id (eth-chain-id) #:nonce 0
                                #:max-priority-fee-per-gas (gwei->wei 1)
                                #:max-fee-per-gas (gwei->wei 10) #:gas-limit 30000
                                #:to (fresh-address) #:value 5 #:data #"\1\2"))
         (check-equal? (rpc-refusal (lambda () (signer-sign-message signer "hello"))) 'rpc)
         (check-equal? (rpc-refusal (lambda () (signer-sign-transaction signer tx))) 'rpc)
         (define json (transaction->jsexpr tx #:from impersonated))
         (define hash (eth-send-transaction json))
         (check-true (receipt-succeeded? (wait-for-receipt hash)))
         (define mined (eth-get-transaction-by-hash hash))
         (for ([key (in-list '(type nonce gas maxPriorityFeePerGas maxFeePerGas value chainId))])
           (check-equal? (hex-string->number (hash-ref mined key))
                         (hex-string->number (hash-ref json key))
                         (format "~a" key)))
         (check-equal? (string-downcase (hash-ref mined 'to)) (string-downcase (hash-ref json 'to)))
         (check-equal? (hash-ref mined 'input) (hash-ref json 'data)))

       (test-case "N6: an address the node does not hold is the node's error"
         (define stranger (node-signer (fresh-address)))
         (check-equal? (rpc-refusal (lambda () (signer-sign-message stranger "hello"))) 'rpc)
         (check-equal? (rpc-refusal
                        (lambda () (signer-sign-transaction stranger (first signed-transactions))))
                       'rpc))

       (test-case "N7: anvil's eth_sign answers with v 27 or 28, and its answer recovers"
         (define answer (eth-sign account0 (bytes->hex-string #"hello")))
         (check-true (and (memv (bytes-ref (hex-string->bytes answer) 64) '(27 28)) #t))
         (check-equal? (recover-message-address (bytes->signature (hex-string->bytes answer))
                                                "hello")
                       account0)))]))
