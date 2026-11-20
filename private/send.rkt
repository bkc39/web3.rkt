#lang racket/base

(require (only-in racket/match match)
         "../abi.rkt"
         (submod "../abi.rkt" internal)
         "../eth.rkt"
         "../rpc.rkt"
         "exn.rkt"
         "node.rkt")

(provide call-object
         call-raising-reverts
         estimate-gas
         of-transaction
         receipt-timeout
         submit)

(define receipt-timeout (make-parameter 30.0))

(define (call-object #:to [to #f] #:from from #:data data #:value value #:gas gas)
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

(define (estimate-gas who tx errors provider)
  (define estimate
    (call-raising-reverts errors
                          (lambda () (rpc-call "eth_estimateGas" (list tx) #:provider provider))))
  (match (node-quantity->number who "gas estimate" estimate)
    [0 (raise (exn:fail:web3 (format "~a: the node estimated 0 gas" who)
                             (current-continuation-marks)))]
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
    [(node-status->boolean who
                           (of-transaction "receipt status" tx-hash)
                           (hash-ref receipt 'status #f))
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
                  [exn:fail?
                   (lambda (e) (values #f (format "replaying it raised: ~a" (exn-message e))))])
    (eth-call tx block #:provider provider)
    (values #f "replaying it did not fail")))
