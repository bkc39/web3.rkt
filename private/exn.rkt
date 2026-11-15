#lang racket/base

;; The exception roots, apart from rpc.rkt so that modules with no runtime
;; dependencies (web3/abi/type) can raise subtypes of them.

(provide (struct-out exn:fail:web3)
         (struct-out exn:fail:web3:rpc))

(struct exn:fail:web3 exn:fail ())
(struct exn:fail:web3:rpc exn:fail:web3 (method code rpc-message data))
