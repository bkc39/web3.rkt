#lang racket/base

;; 03: providers of your own.
;;
;; Endpoints only need something that answers JSON-RPC requests.  A mock
;; answers from a table, so code can be tried and tested without a node; a
;; struct implementing gen:provider can wrap a real node to log, cache or
;; retry.
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

;; A value is the result; a procedure computes it from the params.
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

;; What the endpoints sent: block 5 went out as "0x5".
(show (mock-provider-calls mock))

(section "mocks can fail like a node")

(define (try thunk)
  (with-handlers ([exn:fail:web3:rpc?
                   (lambda (e)
                     (list 'rpc-error
                           (exn:fail:web3:rpc-code e)
                           (exn:fail:web3:rpc-rpc-message e)
                           (exn:fail:web3:rpc-data e)))])
    (thunk)))

(show (try (lambda () (eth-gas-price #:provider mock))))

(define overflow (bytes->hex-string (encode-call "Panic(uint256)" #x11)))

(define reverting
  (mock-provider
   (lambda (method params)
     (match method
       ["eth_call" (raise-rpc-error method 3 "execution reverted" overflow)]
       [_ (raise-rpc-error method -32601 "Method not found")]))))

(show (try (lambda () (eth-call (hasheq 'to alice 'data "0x") "latest" #:provider reverting))))
(show (decode-revert-reason (hex-string->bytes overflow)))

(section "a provider that logs every call")

(struct logging (inner [entries #:auto #:mutable])
  #:auto-value '()
  #:methods gen:provider
  [(define/generic inner-request provider-request)
   (define/generic inner-close provider-close)
   (define (provider-request p method params)
     (define start (current-inexact-milliseconds))
     (dynamic-wind
       void
       (lambda () (inner-request (logging-inner p) method params))
       (lambda ()
         (define ms (/ (round (* 100 (- (current-inexact-milliseconds) start))) 100))
         (set-logging-entries! p (cons (list method params ms) (logging-entries p))))))
   (define (provider-close p)
     (inner-close (logging-inner p)))])

;; (method params elapsed-ms), oldest first.
(define (logging-log p)
  (reverse (logging-entries p)))

(define node (start-anvil))
(define logged (logging (anvil-provider node)))
(current-provider logged)

(define me (car (eth-accounts)))
(show (eth-chain-id))
(show (eth-get-balance me "latest"))
(show (try (lambda () (rpc-call "eth_noSuchMethod" '()))))
(anvil-mine)
(show (eth-block-number))

(show (logging-log logged))

(section "provider-close")

;; The HTTP connection closes; the next request opens a new one.
(show (provider-close logged))
(show (eth-block-number))

;; Mocks hold nothing open, so closing one does nothing.
(show (provider-close mock))
(show (eth-chain-id #:provider mock))
