#lang racket/base

(require (for-syntax racket/base)
         racket/contract/base
         (only-in racket/dict in-dict)
         (only-in racket/match match match-lambda)
         "../abi.rkt"
         (submod "../abi.rkt" internal)
         (only-in "../address.rkt" address->checksum-string)
         (only-in "../eth.rkt" eth-call eth-send-transaction wait-for-receipt)
         "../rpc.rkt"
         (only-in (submod "../signer.rkt" internal)
                  signer-address/who
                  signer-sign-transaction/who)
         "../transaction.rkt"
         "exn.rkt"
         "node.rkt"
         (only-in "opaque.rkt" opaque/c)
         (only-in "signer.rkt" signer?)
         syntax/parse/define
         "who.rkt")

(provide call-object
         call-raising-reverts
         estimate-gas
         fill-reason/c
         fill-transaction
         fill-transaction/who
         from/c
         of-transaction
         receipt-timeout
         send-signed-transaction
         send-signed-transaction/who
         send-signed-transaction/receipt
         send-signed-transaction/receipt/who
         sender-address
         sender/c
         submit)

(define (sender? v)
  (or (abi-value? 'address v) (signer? v)))

(define from/c
  (opaque/c 'from/c "#f, an address or a signer" (lambda (v) (or (not v) (sender? v)))))
(define sender/c (opaque/c 'sender/c "an address or a signer" sender?))

(define receipt-timeout (make-parameter 30.0))

(define field-types
  '((#:gas-price 0 1) (#:max-priority-fee-per-gas 2) (#:max-fee-per-gas 2) (#:access-list 1 2)))

(define fill-phrases
  (hasheq 'chain-id "wrong chain" 'no-base-fee "no base fee" 'fee-cap "fee cap too low"))

(define fill-reason/c
  (flat-named-contract 'fill-reason/c (apply or/c (hash-keys fill-phrases #t))))

(define (raise-fill who reason fmt . args)
  (raise (exn:fail:web3:fill
          (format "~a: ~a: ~a" who (hash-ref fill-phrases reason) (apply format fmt args))
          (current-continuation-marks)
          reason)))

(define (raise-conflict who fmt . args)
  (raise (exn:fail:contract (format "~a: ~a" who (apply format fmt args))
                            (current-continuation-marks))))

(define (sender-address who sender)
  (if (signer? sender)
      (signer-address/who who sender)
      (address->checksum-string sender)))

(define (access-entry->json entry)
  (match entry
    [(cons address keys)
     (hasheq 'address (address->checksum-string address)
             'storageKeys (map bytes->hex-string keys))]))

(define (call-object #:to [to #f] #:from from #:data data #:value value #:gas gas
                     #:access-list [access-list #f])
  (define fields
    (hasheq 'to to
            'from from
            'data (bytes->hex-string data)
            'value (and (positive? value) (number->hex-string value))
            'gas (and gas (number->hex-string gas))
            'accessList (and (pair? access-list) (map access-entry->json access-list))))
  (for/hasheq ([(key v) (in-hash fields)]
               #:when v)
    (values key v)))

(define (call-raising-reverts errors thunk)
  (with-handlers ([exn:fail:web3:rpc?
                   (lambda (e) (raise (or (rpc-exn->revert-exn e #:errors errors) e)))])
    (thunk)))

(define (estimate-gas who tx errors provider #:limit [limit #f])
  (define estimate
    (call-raising-reverts errors
                          (lambda () (rpc-call "eth_estimateGas" (list tx) #:provider provider))))
  (match (node-quantity->number who "gas estimate" estimate)
    [0 (raise (exn:fail:web3 (format "~a: the node estimated 0 gas" who)
                             (current-continuation-marks)))]
    [gas #:when (and limit (>= gas limit)) (raise-node-malformed who "gas estimate" estimate)]
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

(define (settle who tx-hash tx errors provider)
  (define receipt (wait-for-outcome who tx-hash provider))
  (cond
    [(node-status->boolean who
                           (of-transaction "receipt status" tx-hash)
                           (hash-ref receipt 'status #f))
     receipt]
    [else
     (define block
       (node-quantity->number who
                              (of-transaction "block number" tx-hash)
                              (hash-ref receipt 'blockNumber #f)))
     (define-values (revert tail) (replay tx block errors provider))
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

;;;; filling

(define (fill who from to order)
  (error 'unimplemented))

(define (send-raw who signer to order)
  (error 'unimplemented))

(define (send-raw/receipt who signer to order)
  (error 'unimplemented))

(define-syntax-parse-rule (define/filling (name:id who:id from:id to:id order:id)
                            body:expr ...+)
  #:with ((kw arg default) ...) #'((#:value value 0)
                                   (#:data data #"")
                                   (#:nonce nonce #f)
                                   (#:gas-limit gas-limit #f)
                                   (#:chain-id chain-id #f)
                                   (#:type type #f)
                                   (#:gas-price gas-price #f)
                                   (#:max-priority-fee-per-gas max-priority-fee-per-gas #f)
                                   (#:max-fee-per-gas max-fee-per-gas #f)
                                   (#:access-list access-list #f)
                                   (#:errors errors '())
                                   (#:provider provider (current-provider)))
  (define/who (name who from to (~@ kw [arg default]) ...)
    (define order (make-immutable-hasheq (list (cons 'kw arg) ...)))
    body ...))

(define/filling (fill-transaction who from to order)
  (define-values (tx _call) (fill who from to order))
  tx)

(define/filling (send-signed-transaction who signer to order)
  (define-values (tx-hash _call) (send-raw who signer to order))
  tx-hash)

(define/filling (send-signed-transaction/receipt who signer to order)
  (define-values (_tx-hash receipt) (send-raw/receipt who signer to order))
  receipt)

;;;; contract paths

(define (submit who sender #:to to #:data data #:value value #:gas gas #:errors errors
                #:provider provider)
  (cond
    [(signer? sender) (error 'unimplemented)]
    [else
     (define tx (call-object #:to to #:from sender #:data data #:value value #:gas gas))
     (define tx+gas
       (cond
         [gas tx]
         [else (hash-set tx 'gas (number->hex-string (estimate-gas who tx errors provider)))]))
     (define tx-hash
       (call-raising-reverts errors
                             (lambda () (eth-send-transaction tx+gas #:provider provider))))
     (values tx-hash (settle who tx-hash tx+gas errors provider))]))
