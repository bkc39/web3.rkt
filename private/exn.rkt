#lang racket/base

;; Requires nothing but racket/base: web3/abi/type raises these and must
;; stay usable at phase 1.

(provide (struct-out exn:fail:web3)
         (struct-out exn:fail:web3:rpc)
         (struct-out exn:fail:web3:abi)
         (struct-out exn:fail:web3:abi:type)
         (struct-out exn:fail:web3:abi:value))

(struct exn:fail:web3 exn:fail ())
(struct exn:fail:web3:rpc exn:fail:web3 (method code rpc-message data))
(struct exn:fail:web3:abi exn:fail:web3 ())
(struct exn:fail:web3:abi:type exn:fail:web3:abi (input position))
(struct exn:fail:web3:abi:value exn:fail:web3:abi (type value path))
