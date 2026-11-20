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

(define (given order kw)
  (hash-ref order kw #f))

(define (given-types order)
  (filter (match-lambda [(cons kw _) (given order kw)]) field-types))

(define (first-disjoint rows)
  (match rows
    ['() #f]
    [(cons (cons kw types) more)
     (or (for/first ([(other other-types) (in-dict more)]
                     #:unless (for/or ([type (in-list types)]) (memv type other-types)))
           (list kw other))
         (first-disjoint more))]))

(define (check-fields who order rows)
  (define type (given order '#:type))
  (define excluded
    (and type (for/first ([(kw types) (in-dict rows)] #:unless (memv type types)) kw)))
  (define priority (given order '#:max-priority-fee-per-gas))
  (define max-fee (given order '#:max-fee-per-gas))
  (cond
    [excluded (raise-conflict who "#:type ~a takes no ~a" type excluded)]
    [(first-disjoint rows)
     => (match-lambda
          [(list kw other)
           (raise-conflict who "~a and ~a are for different transaction types" kw other)])]
    [(and priority max-fee (> priority max-fee))
     (raise-conflict who "#:max-priority-fee-per-gas is more than #:max-fee-per-gas")]
    [else (void)]))

(define (candidate-types rows)
  (for/fold ([types '(0 1 2)]) ([(_kw allowed) (in-dict rows)])
    (filter (lambda (type) (memv type allowed)) types)))

(define (decided-type type candidates)
  (cond
    [type type]
    [(equal? candidates '(2)) 2]
    [(memv 2 candidates) #f]
    [else (apply min candidates)]))

(define (node-natural who what v bits)
  (define n (node-quantity->number who what v))
  (if (< n (expt 2 bits)) n (raise-node-malformed who what v)))

(define (ask who what bits provider method . params)
  (node-natural who what (rpc-call method params #:provider provider) bits))

(define (node-chain-id who order provider)
  (define chain-id (ask who "chain id" 64 provider "eth_chainId"))
  (define wanted (given order '#:chain-id))
  (cond
    [(or (not wanted) (= wanted chain-id)) chain-id]
    [else (raise-fill who 'chain-id "#:chain-id is ~a, but the node's chain is ~a"
                      wanted chain-id)]))

(define (latest-base-fee who provider)
  (match (rpc-call "eth_getBlockByNumber" (list "latest" #f) #:provider provider)
    [(? hash? block)
     (match (hash-ref block 'baseFeePerGas 'null)
       ['null #f]
       [raw (cons (node-quantity->number who "base fee" raw) raw)])]
    [block (raise-node-malformed who "latest block" block)]))

(define (fee-cap who base-fee priority)
  (match base-fee
    [(cons base raw)
     (define cap (+ (* 2 base) priority))
     (if (< cap (expt 2 256)) cap (raise-node-malformed who "base fee" raw))]))

(define (eip1559-fees who order base-fee provider)
  (define priority
    (or (given order '#:max-priority-fee-per-gas)
        (ask who "priority fee" 256 provider "eth_maxPriorityFeePerGas")))
  (define cap (or (given order '#:max-fee-per-gas) (fee-cap who base-fee priority)))
  (cond
    [(< cap priority)
     (raise-fill who 'fee-cap "#:max-fee-per-gas is ~a wei, below the node's priority fee of ~a wei"
                 cap priority)]
    [else (values priority cap)]))

(define (gas-price who order provider)
  (or (given order '#:gas-price) (ask who "gas price" 256 provider "eth_gasPrice")))

(define (fill who from to order)
  (define errors (given order '#:errors))
  (define provider (given order '#:provider))
  (define value (given order '#:value))
  (define data (given order '#:data))
  (define access-list (given order '#:access-list))
  (check-error-signatures/who who errors)
  (define rows (given-types order))
  (check-fields who order rows)
  (define address (sender-address who from))
  (define candidates (candidate-types rows))
  (define type (decided-type (given order '#:type) candidates))
  (define chain-id (node-chain-id who order provider))
  (define call
    (call-object #:from address #:to (and to (address->checksum-string to)) #:data data
                 #:value value #:gas #f #:access-list access-list))
  (define gas-limit
    (or (given order '#:gas-limit) (estimate-gas who call errors provider #:limit (expt 2 64))))
  (define nonce
    (or (given order '#:nonce)
        (ask who "transaction count" 64 provider "eth_getTransactionCount" address "pending")))
  (define block? (or (not type) (and (= type 2) (not (given order '#:max-fee-per-gas)))))
  (define base-fee (and block? (latest-base-fee who provider)))
  (define final-type (or type (if base-fee 2 (apply min (remv 2 candidates)))))
  (define tx
    (match final-type
      [2 #:when (and block? (not base-fee))
         (raise-fill who 'no-base-fee
                     (string-append "the latest block has no base fee, so a type 2 transaction "
                                    "needs #:max-fee-per-gas"))]
      [2
       (define-values (priority cap) (eip1559-fees who order base-fee provider))
       (eip1559-transaction #:chain-id chain-id #:nonce nonce #:max-priority-fee-per-gas priority
                            #:max-fee-per-gas cap #:gas-limit gas-limit #:to to #:value value
                            #:data data #:access-list (or access-list '()))]
      [1
       (eip2930-transaction #:chain-id chain-id #:nonce nonce
                            #:gas-price (gas-price who order provider) #:gas-limit gas-limit
                            #:to to #:value value #:data data
                            #:access-list (or access-list '()))]
      [0
       (legacy-transaction #:chain-id chain-id #:nonce nonce
                           #:gas-price (gas-price who order provider) #:gas-limit gas-limit
                           #:to to #:value value #:data data)]))
  (values tx call))

(define (send-raw who signer to order)
  (define-values (tx call) (fill who signer to order))
  (define provider (given order '#:provider))
  (define stx
    (parameterize ([current-provider provider])
      (signer-sign-transaction/who who signer tx)))
  (define tx-hash (signed-transaction-hash stx))
  (define raw (bytes->hex-string (signed-transaction->bytes stx)))
  (define answer
    (call-raising-reverts (given order '#:errors)
                          (lambda ()
                            (rpc-call "eth_sendRawTransaction" (list raw) #:provider provider))))
  (cond
    [(and (string? answer) (string=? (string-downcase answer) tx-hash))
     (values tx-hash (hash-set call 'gas (number->hex-string (transaction-gas-limit tx))))]
    [else
     (raise (exn:fail:web3 (format "~a: sent transaction ~a, but the node answered ~e"
                                   who tx-hash answer)
                           (current-continuation-marks)))]))

(define (send-raw/receipt who signer to order)
  (define-values (tx-hash call) (send-raw who signer to order))
  (values tx-hash
          (settle who tx-hash call (given order '#:errors) (given order '#:provider))))

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

(define (check-width who kw v bits)
  (when (>= v (expt 2 bits))
    (raise-conflict who "~a is more than a transaction's ~a bits hold" kw bits)))

(define (submit who sender #:to to #:data data #:value value #:gas gas #:errors errors
                #:provider provider)
  (cond
    [(signer? sender)
     (check-width who '#:value value 256)
     (check-width who '#:gas (or gas 0) 64)
     (send-raw/receipt who sender to
                       (hasheq '#:value value '#:data data '#:gas-limit gas '#:errors errors
                               '#:provider provider))]
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
