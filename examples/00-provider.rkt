#lang racket/base

(require web3)

(define-jsonrpc-endpoint eth_getUncleCountByBlockNumber
  (-> [exact-nonnegative-integer? number->hex-string] exact-nonnegative-integer?)
  #:result-transform hex-string->number)

(define (print-rpc-error e)
  (printf "refused ~a: ~a\n" (exn:fail:web3:rpc-code e) (exn:fail:web3:rpc-rpc-message e)))

(module+ main
  (with-anvil ()
    (printf "chain id ~a\n" (eth-chain-id))
    (printf "block ~a\n" (eth-block-number))
    (printf "eth_chainId ~s\n" (rpc-call "eth_chainId" '()))
    (printf "uncles in block 0: ~a\n" (eth-get-uncle-count-by-block-number 0))
    (with-handlers ([exn:fail:web3:rpc? print-rpc-error])
      (rpc-call "eth_noSuchMethod" '()))))
