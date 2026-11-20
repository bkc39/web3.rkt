#lang racket/base

(require web3)

(define key
  (hex-string->secret-key "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))
(define recipient "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")
(define chain-id 31337)
(define value (ether->wei "1.5"))
(define slot-1 (bytes-append (make-bytes 31 0) (bytes 1)))

(define legacy
  (legacy-transaction #:chain-id chain-id #:nonce 0 #:gas-price (gwei->wei 30)
                      #:gas-limit 21000 #:to recipient #:value value))
(define eip2930
  (eip2930-transaction #:chain-id chain-id #:nonce 0 #:gas-price (gwei->wei 30)
                       #:gas-limit 30000 #:to recipient #:value value
                       #:access-list (list (list recipient slot-1))))
(define eip1559
  (eip1559-transaction #:chain-id chain-id #:nonce 0
                       #:max-priority-fee-per-gas (gwei->wei 2)
                       #:max-fee-per-gas (gwei->wei 30)
                       #:gas-limit 21000 #:to recipient #:value value))

(define (print-signed tx)
  (define raw (signed-transaction->bytes (sign-transaction key tx)))
  (define parsed (bytes->signed-transaction raw))
  (printf "type ~a\n" (transaction-type tx))
  (printf "  raw    ~a\n" (bytes->hex-string raw))
  (printf "  hash   ~a\n" (signed-transaction-hash parsed))
  (printf "  sender ~a\n" (signed-transaction-sender parsed)))

(define (print-truncated tx)
  (define raw (signed-transaction->bytes (sign-transaction key tx)))
  (with-handlers ([exn:fail:web3:rlp?
                   (lambda (e)
                     (printf "truncated: ~a at byte ~a\n"
                             (exn:fail:web3:rlp-reason e) (exn:fail:web3:rlp-offset e)))])
    (bytes->signed-transaction (subbytes raw 0 (sub1 (bytes-length raw))))))

(module+ main
  (for-each print-signed (list legacy eip2930 eip1559))
  (print-truncated eip1559))
