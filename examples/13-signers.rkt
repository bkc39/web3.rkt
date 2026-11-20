#lang racket/base

(require (only-in racket/list first)
         web3)

(define key
  (hex-string->secret-key "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))
(define message "hello")
(define digest (hash-message message))
(define tx
  (eip1559-transaction #:chain-id 31337 #:nonce 0
                       #:max-priority-fee-per-gas (gwei->wei 2)
                       #:max-fee-per-gas (gwei->wei 30)
                       #:gas-limit 21000
                       #:to "0x70997970C51812dc3A010C7d01b50e0d17dc79C8"
                       #:value (ether->wei "1.5")))

(define (signature-hex sig)
  (bytes->hex-string (signature->bytes sig)))

(define (digest-signature signer)
  (with-handlers ([exn:fail:web3:signer? exn:fail:web3:signer-reason])
    (signature-hex (signer-sign-digest signer digest))))

(define (print-signer signer)
  (define sig (signer-sign-message signer message))
  (printf "~a\n" signer)
  (printf "  digest      ~a\n" (digest-signature signer))
  (printf "  message     ~a\n" (signature-hex sig))
  (printf "  recovered   ~a\n" (recover-message-address sig message))
  (printf "  verified    ~a\n" (message-signed-by? sig message (signer-address signer)))
  (printf "  transaction ~a\n" (signed-transaction-hash (signer-sign-transaction signer tx))))

(module+ main
  (printf "digest ~a\n" (bytes->hex-string digest))
  (with-anvil ()
    (for ([signer (list (local-signer key) (node-signer (first (eth-accounts))))])
      (print-signer signer))))
