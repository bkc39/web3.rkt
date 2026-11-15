#lang racket/base

;; Calls, sends, estimates and deploys under their caller's who, with every
;; option given.  web3/contract and the procedures define-contract binds
;; supply the defaults.

(require (only-in racket/match match struct*)
         "../abi.rkt"
         (submod "../abi.rkt" internal)
         (only-in (submod "../abi/type.rkt" private) raise-abi-type-error)
         "../eth.rkt"
         "../rpc.rkt"
         "exn.rkt"
         "node.rkt")

(provide call-function
         current-account
         deploy-code
         estimate-function
         init-code->bytes
         receipt-timeout
         send-function)

(define current-account (make-parameter #f))

(define receipt-timeout (make-parameter 30.0))

(define (transaction #:to [to #f] #:from from #:data data #:value value #:gas gas)
  (define fields
    (hasheq 'to to
            'from from
            'data (bytes->hex-string data)
            'value (and (positive? value) (number->hex-string value))
            'gas (and gas (number->hex-string gas))))
  (for/hasheq ([(key v) (in-hash fields)]
               #:when v)
    (values key v)))

(define (call-raising-reverts errors thunk)
  (with-handlers ([exn:fail:web3:rpc?
                   (lambda (e) (raise (or (rpc-exn->revert-exn e #:errors errors) e)))])
    (thunk)))

(define (sender who from)
  (or from
      (raise (exn:fail:web3 (format "~a: no sender; pass #:from or set current-account" who)
                            (current-continuation-marks)))))

(define (call-function who address signature args
                       #:from from #:value value #:gas gas #:block block #:errors errors #:provider provider)
  (define sig (result-signature/who who signature))
  (check-error-signatures/who who errors)
  (define tx (transaction #:to address
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
            (transaction #:to address #:from (sender who from) #:data data #:value value #:gas gas)
            errors
            provider))
  receipt)

(define (estimate-function who address signature args #:from from #:value value #:errors errors #:provider provider)
  (define sig (string->abi-signature/who who signature))
  (check-error-signatures/who who errors)
  (define data (apply encode-call/who who sig args))
  (estimate-gas who (transaction #:to address #:from from #:data data #:value value #:gas #f) errors provider))

(define (estimate-gas who tx errors provider)
  (define estimate
    (call-raising-reverts errors (lambda () (rpc-call "eth_estimateGas" (list tx) #:provider provider))))
  (match (node-quantity->number who "gas estimate" estimate)
    [0 (raise (exn:fail:web3 (format "~a: the node estimated 0 gas" who) (current-continuation-marks)))]
    [gas gas]))

(define (with-message e message)
  (define marks (exn-continuation-marks e))
  (match e
    [(exn:fail:web3:rpc:revert _ _ method code rpc-message data revert)
     (exn:fail:web3:rpc:revert message marks method code rpc-message data revert)]
    [(exn:fail:web3:rpc _ _ method code rpc-message data)
     (exn:fail:web3:rpc message marks method code rpc-message data)]
    [_ (exn:fail:web3 message marks)]))

(define (of-transaction what tx-hash)
  (format "~a of transaction ~a" what tx-hash))

(define (wait-for-outcome who tx-hash provider)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (raise (with-message e (format "~a: sent transaction ~a, outcome unknown: ~a"
                                                    who tx-hash (exn-message e)))))])
    (wait-for-receipt tx-hash #:timeout (receipt-timeout) #:provider provider)))

(define (submit who tx errors provider)
  (define tx+gas
    (cond
      [(hash-ref tx 'gas #f) tx]
      [else (hash-set tx 'gas (number->hex-string (estimate-gas who tx errors provider)))]))
  (define tx-hash
    (call-raising-reverts errors (lambda () (eth-send-transaction tx+gas #:provider provider))))
  (define receipt (wait-for-outcome who tx-hash provider))
  (cond
    [(node-status->boolean who (of-transaction "receipt status" tx-hash) (hash-ref receipt 'status #f))
     (values tx-hash receipt)]
    [else
     (define block
       (node-quantity->number who
                              (of-transaction "block number" tx-hash)
                              (hash-ref receipt 'blockNumber #f)))
     (define-values (revert tail) (replay tx+gas block errors provider))
     (raise (exn:fail:web3:transaction
             (format "~a: transaction ~a failed in block ~a: ~a" who tx-hash block tail)
             (current-continuation-marks)
             receipt
             revert))]))

(define (rpc-replay-outcome e errors)
  (cond
    [(rpc-exn->revert e #:errors errors)
     => (lambda (revert) (values revert (format "reverted with ~a" (abi-revert->string revert))))]
    [else (values #f (exn:fail:web3:rpc-rpc-message e))]))

(define (replay tx block errors provider)
  (with-handlers ([exn:fail:web3:rpc? (lambda (e) (rpc-replay-outcome e errors))]
                  [exn:fail? (lambda (e) (values #f (format "replaying it raised: ~a" (exn-message e))))])
    (eth-call tx block #:provider provider)
    (values #f "replaying it did not fail")))

(define (constructor-signature who constructor)
  (match (string->abi-signature/who who constructor)
    [(and sig (struct* abi-signature ([name "constructor"] [outputs #f]))) sig]
    [sig (raise-abi-type-error who "a constructor signature is constructor(T,...)"
                               (abi-signature->string sig #:outputs? #t) #f)]))

(define (init-code->bytes code)
  (if (bytes? code) code (hex-string->bytes code)))

;; The new contract's address and the receipt.
(define (deploy-code who init-code constructor args
                     #:from from #:value value #:gas gas #:errors errors #:provider provider)
  (define sig (constructor-signature who constructor))
  (check-error-signatures/who who errors)
  (define data
    (bytes-append (init-code->bytes init-code) (abi-encode/who who (abi-signature-inputs sig) args)))
  (define-values (tx-hash receipt)
    (submit who (transaction #:from (sender who from) #:data data #:value value #:gas gas)
            errors
            provider))
  (values (node-address->string who
                                (of-transaction "contract address" tx-hash)
                                (hash-ref receipt 'contractAddress #f))
          receipt))
