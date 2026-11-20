#lang racket/base

(require web3)

(define-jsonrpc-endpoint eth_getUncleCountByBlockNumber
  (-> [exact-nonnegative-integer? number->hex-string] exact-nonnegative-integer?)
  #:result-transform hex-string->number)

(define (rpc-error-code method)
  (with-handlers ([exn:fail:web3:rpc? exn:fail:web3:rpc-code])
    (rpc-call method '())))

(module+ main
  (with-anvil ()
    (printf "chain id ~a\n" (eth-chain-id))
    (printf "block ~a\n" (eth-block-number))
    (printf "eth_chainId ~s\n" (rpc-call "eth_chainId" '()))
    (printf "uncles in block 0: ~a\n" (eth-get-uncle-count-by-block-number 0))
    (printf "eth_noSuchMethod error ~a\n" (rpc-error-code "eth_noSuchMethod"))))
