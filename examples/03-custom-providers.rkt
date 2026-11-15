#lang racket/base

;; 03: providers of your own.
;;
;; A mock answers from a table, so code runs without a node.  A struct
;; implementing gen:provider can wrap a real node to log, cache or retry.
;;
;;   racket examples/03-custom-providers.rkt
;;   racket -i -e '(enter! "examples/03-custom-providers.rkt")'

(require racket/generic
         racket/match
         web3
         "common.rkt")

(provide (all-defined-out))

(section "a mock answers from a table")

(define alice "0x00000000000000000000000000000000000a11ce")
(define bob "0x0000000000000000000000000000000000000b0b")

(define balances (hash alice "0xde0b6b3a7640000"))

(define mock
  (mock-provider
   (hash "eth_chainId" "0x7a69"
         "eth_blockNumber" "0x10"
         "eth_getBalance" (match-lambda
                            [(list address _block) (hash-ref balances address "0x0")]))))

(show (eth-chain-id #:provider mock))
(show (with-provider mock
        (eth-block-number)))
(show (eth-get-balance alice "latest" #:provider mock))
(show (eth-get-balance bob 5 #:provider mock))
(show (mock-provider-calls mock))

(section "mocks can fail like a node")

(show (with-handlers ([exn:fail:web3:rpc? exn-message])
        (eth-gas-price #:provider mock)))

(define overflow (bytes->hex-string (encode-call "Panic(uint256)" #x11)))

(define reverting
  (mock-provider
   (lambda (method params)
     (match method
       ["eth_call" (raise-rpc-error method 3 "execution reverted" overflow)]
       [_ (raise-rpc-error method -32601 "Method not found")]))))

(define revert-exn
  (with-handlers ([exn:fail:web3:rpc? values])
    (eth-call (hasheq 'to alice 'data "0x") "latest" #:provider reverting)))

(show (exn-message revert-exn))
(show (decode-revert-reason (hex-string->bytes (exn:fail:web3:rpc-data revert-exn))))

(section "a provider that logs every call")

(struct logging (inner)
  #:methods gen:provider
  [(define/generic inner-request provider-request)
   (define/generic inner-close provider-close)
   (define (provider-request p method params)
     (define start (current-inexact-milliseconds))
     (begin0 (inner-request (logging-inner p) method params)
       (printf ";; ~a ~s took ~a ms\n"
               method
               params
               (real->decimal-string (- (current-inexact-milliseconds) start) 1))))
   (define (provider-close p)
     (inner-close (logging-inner p)))])

(define node (start-anvil))
(current-provider (logging (anvil-provider node)))

(define me (car (eth-accounts)))
(show (eth-chain-id))
(show (eth-get-balance me "latest"))
(anvil-mine)
(show (eth-block-number))

(section "provider-close")

(show (provider-close (current-provider)))
(show (eth-block-number))
(show (provider-close mock))
(show (eth-chain-id #:provider mock))
