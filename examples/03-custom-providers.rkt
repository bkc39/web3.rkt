#lang racket/base

(require racket/generic
         racket/match
         web3)

(struct logging (inner)
  #:methods gen:provider
  [(define/generic inner-request provider-request)
   (define/generic inner-close provider-close)
   (define (provider-request provider method params)
     (printf "request ~a\n" method)
     (inner-request (logging-inner provider) method params))
   (define (provider-close provider)
     (inner-close (logging-inner provider)))])

(define alice "0x00000000000000000000000000000000000a11ce")
(define balances (hash alice "0xde0b6b3a7640000"))

(define node
  (mock-provider
   (hash "eth_chainId" "0x7a69"
         "eth_getBalance" (match-lambda
                            [(list address _block) (hash-ref balances address "0x0")])
         "eth_call" (lambda (params)
                      (raise-rpc-error "eth_call" 3 "execution reverted" "0x")))))

(module+ main
  (with-provider (logging node)
    (printf "chain id ~a\n" (eth-chain-id))
    (printf "balance ~a ether\n" (wei->string (eth-get-balance alice "latest")))
    (with-handlers ([exn:fail:web3:rpc? (lambda (e) (printf "refused: ~a\n" (exn-message e)))])
      (eth-call (hasheq 'to alice 'data "0x") "latest")))
  (printf "the mock answered ~a requests\n" (length (mock-provider-calls node))))
