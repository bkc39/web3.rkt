#lang racket/base

;; Requires nothing but racket/base: web3/abi/type raises these and must
;; stay usable at phase 1.

(provide (struct-out exn:fail:web3)
         (struct-out exn:fail:web3:rpc)
         (struct-out exn:fail:web3:rpc:revert)
         (struct-out exn:fail:web3:transaction)
         (struct-out exn:fail:web3:abi)
         (struct-out exn:fail:web3:abi:type)
         (struct-out exn:fail:web3:abi:value)
         (struct-out exn:fail:web3:abi:decode)
         (struct-out exn:fail:web3:key)
         (struct-out exn:fail:web3:rlp)
         (struct-out exn:fail:web3:signature)
         (struct-out exn:fail:web3:units))

(struct exn:fail:web3 exn:fail ())
(struct exn:fail:web3:rpc exn:fail:web3 (method code rpc-message data))
(struct exn:fail:web3:rpc:revert exn:fail:web3:rpc (revert))
(struct exn:fail:web3:transaction exn:fail:web3 (receipt revert))
(struct exn:fail:web3:abi exn:fail:web3 ())
(struct exn:fail:web3:abi:type exn:fail:web3:abi (input position))
(struct exn:fail:web3:abi:value exn:fail:web3:abi (type value path))
(struct exn:fail:web3:abi:decode exn:fail:web3:abi (type offset reason))
(struct exn:fail:web3:key exn:fail:web3 ())
(struct exn:fail:web3:rlp exn:fail:web3 (offset reason))
(struct exn:fail:web3:signature exn:fail:web3 ())
(struct exn:fail:web3:units exn:fail:web3 (index reason))
