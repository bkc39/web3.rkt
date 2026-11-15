#lang racket/base

;; 00: providers and raw calls.
;;
;; A provider is a connection to a node.  Every endpoint takes one with
;; #:provider, or uses `current-provider`.
;;
;;   racket examples/00-provider.rkt
;;   racket -i -e '(enter! "examples/00-provider.rkt")'   ; then keep typing

(require web3
         "common.rkt")

(provide (all-defined-out))

;; A throwaway local chain.  For a real node, pass its URL instead:
;;   (url->http-provider "https://ethereum-rpc.publicnode.com")
(define node (start-anvil))
(define url (format "http://127.0.0.1:~a" (anvil-port node)))
(define provider (url->http-provider url))

(section "three ways to pick the provider")

(show (eth-chain-id #:provider provider))

(show (with-provider provider
        (eth-block-number)))

;; For the rest of this session.
(current-provider provider)
(show (web3-client-version))

(section "raw JSON-RPC")

(show (rpc-call "eth_chainId" '()))
(show (rpc-call "eth_feeHistory" (list "0x1" "latest" (list 50))))

(section "errors")

(define (try thunk)
  (with-handlers ([exn:fail:web3:rpc?
                   (lambda (e)
                     (list 'rpc-error
                           (exn:fail:web3:rpc-method e)
                           (exn:fail:web3:rpc-code e)
                           (exn:fail:web3:rpc-rpc-message e)))]
                  [exn:fail:web3?
                   (lambda (e) (list 'web3-error (exn-message e)))])
    (thunk)))

(show (try (lambda () (rpc-call "eth_noSuchMethod" '()))))
(show (try (lambda () (parameterize ([current-provider #f])
                        (eth-chain-id)))))

(section "a new endpoint is one form")

(define-jsonrpc-endpoint eth_maxPriorityFeePerGas
  (-> exact-nonnegative-integer?)
  #:result-transform hex-string->number)

(show (eth-max-priority-fee-per-gas))

(section "hex helpers")

(show (hex-string->number "0xff"))
(show (number->hex-string 255))
(show (hex-string->bytes "0xdeadbeef"))
(show (bytes->hex-string (bytes 222 173 190 239)))
