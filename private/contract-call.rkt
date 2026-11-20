#lang racket/base

(require (only-in racket/match match struct*)
         "../abi.rkt"
         (submod "../abi.rkt" internal)
         (only-in (submod "../abi/type.rkt" private) raise-abi-type-error)
         (only-in "../eth.rkt" eth-call)
         "../rpc.rkt"
         "exn.rkt"
         "node.rkt"
         "send.rkt")

(provide call-function
         current-account
         deploy-code
         estimate-function
         init-code->bytes
         receipt-timeout
         send-function)

(define current-account (make-parameter #f))

(define (sender who from)
  (or from
      (raise (exn:fail:web3 (format "~a: no sender; pass #:from or set current-account" who)
                            (current-continuation-marks)))))

(define (call-function who address signature args
                       #:from from
                       #:value value
                       #:gas gas
                       #:block block
                       #:errors errors
                       #:provider provider)
  (define sig (result-signature/who who signature))
  (check-error-signatures/who who errors)
  (define tx (call-object #:to address
                           #:from from
                           #:data (apply encode-call/who who sig args)
                           #:value value
                           #:gas gas))
  (define result (call-raising-reverts errors (lambda () (eth-call tx block #:provider provider))))
  (apply values
         (cond
           [(null? (abi-signature-outputs sig)) '()]
           [else (decode-result/who who sig (node-data->bytes who "return data" result))])))

(define (send-function who address signature args
                       #:from from #:value value #:gas gas #:errors errors #:provider provider)
  (define sig (string->abi-signature/who who signature))
  (check-error-signatures/who who errors)
  (define data (apply encode-call/who who sig args))
  (define-values (_tx-hash receipt)
    (submit who
            (call-object #:to address #:from (sender who from) #:data data #:value value #:gas gas)
            errors
            provider))
  receipt)

(define (estimate-function who address signature args
                           #:from from #:value value #:errors errors #:provider provider)
  (define sig (string->abi-signature/who who signature))
  (check-error-signatures/who who errors)
  (define data (apply encode-call/who who sig args))
  (estimate-gas who
                (call-object #:to address #:from from #:data data #:value value #:gas #f)
                errors
                provider))

(define (constructor-signature who constructor)
  (match (string->abi-signature/who who constructor)
    [(and sig (struct* abi-signature ([name "constructor"] [outputs #f]))) sig]
    [sig (raise-abi-type-error who "a constructor signature is constructor(T,...)"
                               (abi-signature->string sig #:outputs? #t) #f)]))

(define (init-code->bytes code)
  (if (bytes? code) code (hex-string->bytes code)))

(define (deploy-code who init-code constructor args
                     #:from from #:value value #:gas gas #:errors errors #:provider provider)
  (define sig (constructor-signature who constructor))
  (check-error-signatures/who who errors)
  (define data
    (bytes-append (init-code->bytes init-code)
                  (abi-encode/who who (abi-signature-inputs sig) args)))
  (define-values (tx-hash receipt)
    (submit who (call-object #:from (sender who from) #:data data #:value value #:gas gas)
            errors
            provider))
  (values (node-address->string who
                                (of-transaction "contract address" tx-hash)
                                (hash-ref receipt 'contractAddress #f))
          receipt))
