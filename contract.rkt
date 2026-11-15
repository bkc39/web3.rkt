#lang racket/base

(require racket/contract/base
         (only-in json jsexpr?)
         (only-in keccak keccak256)
         (only-in racket/match match match-define struct*)
         "abi.rkt"
         (submod "abi.rkt" internal)
         (only-in (submod "abi/type.rkt" private) raise-abi-type-error)
         "address.rkt"
         "eth.rkt"
         "rpc.rkt"
         "private/exn.rkt"
         "private/node.rkt"
         "private/who.rkt")

(provide (struct-out exn:fail:web3:transaction)
         (contract-out
          [current-account (parameter/c (or/c #f (abi-value/c 'address)))]
          [contract-call
           (->* ((abi-value/c 'address) signature/c)
                (#:from (or/c #f (abi-value/c 'address))
                 #:value exact-nonnegative-integer?
                 #:gas (or/c #f exact-positive-integer?)
                 #:block block-param/c
                 #:errors errors/c
                 #:provider (or/c #f provider?))
                #:rest list?
                any)]
          [contract-send
           (->* ((abi-value/c 'address) signature/c)
                (#:from (or/c #f (abi-value/c 'address))
                 #:value exact-nonnegative-integer?
                 #:gas (or/c #f exact-positive-integer?)
                 #:errors errors/c
                 #:provider (or/c #f provider?))
                #:rest list?
                receipt/c)]
          [deploy-contract
           (->* (init-code/c)
                (signature/c
                 #:from (or/c #f (abi-value/c 'address))
                 #:value exact-nonnegative-integer?
                 #:gas (or/c #f exact-positive-integer?)
                 #:errors errors/c
                 #:provider (or/c #f provider?))
                #:rest list?
                checksum-address?)]
          [deploy-contract/receipt
           (->* (init-code/c)
                (signature/c
                 #:from (or/c #f (abi-value/c 'address))
                 #:value exact-nonnegative-integer?
                 #:gas (or/c #f exact-positive-integer?)
                 #:errors errors/c
                 #:provider (or/c #f provider?))
                #:rest list?
                (values checksum-address? receipt/c))]
          [create-address (-> (abi-value/c 'address) nonce/c checksum-address?)]
          [create2-address (-> (abi-value/c 'address) salt/c init-code/c checksum-address?)]))

(define signature/c (or/c string? abi-signature?))
(define errors/c (listof (or/c string? abi-signature?)))
(define init-code/c (or/c bytes? (and/c string? #px"^0x(?:[0-9a-fA-F]{2})*$")))
(define salt/c
  (or/c (and/c bytes? (property/c bytes-length (=/c 32))) (integer-in 0 (sub1 (expt 2 256)))))
(define nonce/c (integer-in 0 (- (expt 2 64) 2)))
(define receipt/c (and/c hash? jsexpr?))

(define current-account (make-parameter #f))

(define/who (contract-call who address signature
                           #:from [from (current-account)]
                           #:value [value 0]
                           #:gas [gas #f]
                           #:block [block "latest"]
                           #:errors [errors '()]
                           #:provider [provider (current-provider)]
                           . args)
  (error 'unimplemented))

(define/who (contract-send who address signature
                           #:from [from (current-account)]
                           #:value [value 0]
                           #:gas [gas #f]
                           #:errors [errors '()]
                           #:provider [provider (current-provider)]
                           . args)
  (error 'unimplemented))

(define/who (deploy-contract who init-code [constructor "constructor()"]
                             #:from [from (current-account)]
                             #:value [value 0]
                             #:gas [gas #f]
                             #:errors [errors '()]
                             #:provider [provider (current-provider)]
                             . args)
  (error 'unimplemented))

(define/who (deploy-contract/receipt who init-code [constructor "constructor()"]
                                     #:from [from (current-account)]
                                     #:value [value 0]
                                     #:gas [gas #f]
                                     #:errors [errors '()]
                                     #:provider [provider (current-provider)]
                                     . args)
  (error 'unimplemented))

(define (create-address sender nonce)
  (error 'unimplemented))

(define (create2-address deployer salt init-code)
  (error 'unimplemented))

(module* internal #f
  (provide contract-call/who
           contract-send/who
           deploy-contract/who))
