#lang racket/base

(require web3)

(define contract "0x00000000000000000000000000000000000c0de0")
(define stranger "0x000000000000000000000000000000000000beef")
(define store-calldata-code "0x5f355f5500")

(define (stored-value)
  (hex-string->number (eth-get-storage-at contract 0 "latest")))

(module+ main
  (with-anvil ()
    (anvil-set-code contract store-calldata-code)
    (anvil-set-balance stranger (ether->wei 1))
    (define snapshot (evm-snapshot))
    (anvil-impersonate-account stranger)
    (define receipt
      (send-transaction/receipt
       (hasheq 'from stranger 'to contract 'data (bytes->hex-string (word->bytes 7)))))
    (anvil-stop-impersonating-account stranger)
    (printf "sent from ~a, stored ~a\n" (hash-ref receipt 'from) (stored-value))
    (evm-revert snapshot)
    (printf "reverted, stored ~a\n" (stored-value))))
