#lang racket/base

(require (for-syntax racket/base
                     (only-in racket/syntax format-id)
                     syntax/parse)
         racket/contract/base
         (only-in racket/list first rest)
         (only-in racket/match match match-lambda)
         "../abi.rkt"
         (submod "../abi.rkt" internal)
         (only-in "../address.rkt" address->checksum-string)
         (only-in "../eth.rkt" eth-call eth-send-transaction)
         (only-in (submod "../eth.rkt" internal) wait-for-receipt/who)
         (only-in "../eth/receipt.rkt" receipt-block-number receipt-status)
         (only-in (submod "../eth/receipt.rkt" internal) jsexpr->receipt-of)
         "../rpc.rkt"
         (only-in (submod "../signer.rkt" internal)
                  raise-signer
                  signer-address/who
                  signer-sign-transaction/who)
         "../transaction.rkt"
         "contracts.rkt"
         "exn.rkt"
         "node.rkt"
         (only-in "opaque.rkt" opaque->)
         (only-in "signer.rkt" signer?)
         syntax/parse/define
         "who.rkt")

(provide call-object
         call-raising-reverts
         estimate-gas
         fill-reason/c
         fill-reasons
         fill-transaction
         fill-transaction/who
         filling->
         of-transaction
         receipt-timeout
         send-signed-transaction
         send-signed-transaction/who
         send-signed-transaction/receipt
         send-signed-transaction/receipt/who
         sender-address
         submit)

(define receipt-timeout (make-parameter 30.0))

(define field-types
  '((#:gas-price 0 1) (#:max-priority-fee-per-gas 2) (#:max-fee-per-gas 2) (#:access-list 1 2)))

(define fill-phrases
  (hasheq 'chain-id "wrong chain" 'no-base-fee "no base fee" 'fee-cap "fee cap too low"))

(define fill-reasons (hash-keys fill-phrases #t))

(define fill-reason/c (flat-named-contract 'fill-reason/c (apply or/c fill-reasons)))

(begin-for-syntax
  (define-syntax-class fill-keyword
    #:description "[#:keyword field contract default]"
    (pattern [kw:keyword field:id contract:expr default:expr])))

(define-syntax-parse-rule (define-fill-keywords order:id define/filling:id filling->:id
                            key:fill-keyword ...)
  #:with new-order (format-id #'order "new-~a" #'order)
  #:with order-ref (format-id #'order "~a-ref" #'order)
  #:with (accessor ...) (for/list ([field (in-list (attribute key.field))])
                          (format-id #'order "~a-~a" #'order field))
  (begin
    (struct order (key.field ...) #:constructor-name new-order #:omit-define-syntaxes)
    (define (order (~@ key.kw [key.field key.default]) ...)
      (new-order key.field ...))
    (define (order-ref fields kw)
      (case kw
        [(key.kw) (accessor fields)] ...))
    (define-syntax-parse-rule (define/filling (name:id who:id from:id to:id fields:id)
                                body:expr (... ...+))
      (define/who (name who from to (~@ key.kw [key.field key.default]) ...)
        (define fields (new-order key.field ...))
        body (... ...)))
    (define-syntax-parse-rule (filling-> first/c:expr result/c:expr)
      (opaque-> first/c to/c [key.kw key.contract] ... result/c))))

(define-fill-keywords fill-order define/filling filling->
  [#:value value uint256/c 0]
  [#:data data bytes? #""]
  [#:nonce nonce (or/c #f uint64/c) #f]
  [#:gas-limit gas-limit (or/c #f uint64/c) #f]
  [#:chain-id chain-id (or/c #f uint64/c) #f]
  [#:type type (or/c #f 0 1 2) #f]
  [#:gas-price gas-price (or/c #f uint256/c) #f]
  [#:max-priority-fee-per-gas max-priority-fee-per-gas (or/c #f uint256/c) #f]
  [#:max-fee-per-gas max-fee-per-gas (or/c #f uint256/c) #f]
  [#:access-list access-list (or/c #f access-list/c) #f]
  [#:errors errors errors/c '()]
  [#:provider provider provider/c (current-provider)])

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
    (wait-for-receipt/who 'wait-for-receipt tx-hash unread-receipt
                          #:timeout (receipt-timeout) #:provider provider)))

(define (unread-receipt _who _tx-hash raw)
  raw)

(define (settle who tx-hash tx errors provider)
  (define raw (wait-for-outcome who tx-hash provider))
  (define receipt (jsexpr->receipt-of who tx-hash raw (of-transaction "" tx-hash)))
  (match (receipt-status receipt)
    [1 receipt]
    [0
     (define block (receipt-block-number receipt))
     (define-values (revert tail) (replay tx block errors provider))
     (raise (exn:fail:web3:transaction
             (format "~a: transaction ~a failed in block ~a: ~a" who tx-hash block tail)
             (current-continuation-marks)
             receipt
             revert))]
    [#f (raise-node-missing who (of-transaction "receipt status" tx-hash))]))

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

(define (given-rows order)
  (filter (match-lambda [(cons kw _) (fill-order-ref order kw)]) field-types))

(define (excluded-by type rows)
  (for/first ([row (in-list rows)]
              #:unless (memv type (rest row)))
    (first row)))

(define (shares-a-type? types row)
  (for/or ([type (in-list types)])
    (memv type (rest row))))

(define (first-disjoint rows)
  (match rows
    ['() #f]
    [(cons (cons kw types) more)
     (match (findf (lambda (row) (not (shares-a-type? types row))) more)
       [#f (first-disjoint more)]
       [(cons other _) (list kw other)])]))

(define (check-conflicts who order rows)
  (define type (fill-order-type order))
  (define priority (fill-order-max-priority-fee-per-gas order))
  (define max-fee (fill-order-max-fee-per-gas order))
  (define excluded (and type (excluded-by type rows)))
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
  (for/fold ([types '(0 1 2)]) ([row (in-list rows)])
    (filter (lambda (type) (memv type (rest row))) types)))

(define (fixed-type order candidates)
  (cond
    [(fill-order-type order)]
    [(equal? candidates '(2)) 2]
    [(memv 2 candidates) #f]
    [else (apply min candidates)]))

(define (block-needed? order fixed)
  (or (not fixed) (and (= fixed 2) (not (fill-order-max-fee-per-gas order)))))

(define (ask who what bits provider method . params)
  (node-natural who what (rpc-call method params #:provider provider) bits))

(define (node-chain-id who order provider)
  (define chain-id (ask who "chain id" 64 provider "eth_chainId"))
  (define wanted (fill-order-chain-id order))
  (cond
    [(or (not wanted) (= wanted chain-id)) chain-id]
    [else (raise-fill who 'chain-id "#:chain-id is ~a, but the node's chain is ~a"
                      wanted chain-id)]))

(define (estimate-object address to order)
  (call-object #:from address #:to (and to (address->checksum-string to))
               #:data (fill-order-data order) #:value (fill-order-value order) #:gas #f
               #:access-list (fill-order-access-list order)))

(define (filled-gas-limit who address to order provider)
  (or (fill-order-gas-limit order)
      (estimate-gas who (estimate-object address to order) (fill-order-errors order) provider
                    #:limit (expt 2 64))))

(define (filled-nonce who address order provider)
  (or (fill-order-nonce order)
      (ask who "transaction count" 64 provider "eth_getTransactionCount" address "pending")))

(define (latest-base-fee who provider)
  (match (rpc-call "eth_getBlockByNumber" (list "latest" #f) #:provider provider)
    [(? hash? block)
     (match (hash-ref block 'baseFeePerGas 'null)
       ['null #f]
       [raw (cons (node-quantity->number who "base fee" raw) raw)])]
    [block (raise-node-malformed who "latest block" block)]))

(define (final-type who fixed candidates block? base-fee)
  (cond
    [(and (eqv? fixed 2) block? (not base-fee))
     (raise-fill who 'no-base-fee
                 (string-append "the latest block has no base fee, so a type 2 transaction "
                                "needs #:max-fee-per-gas"))]
    [fixed fixed]
    [base-fee 2]
    [else (apply min (remv 2 candidates))]))

(define (fee-cap who base-fee priority)
  (match base-fee
    [(cons base raw)
     (define cap (+ (* 2 base) priority))
     (if (< cap (expt 2 256)) cap (raise-node-malformed who "base fee" raw))]))

(define (eip1559-fees who order base-fee provider)
  (define priority
    (or (fill-order-max-priority-fee-per-gas order)
        (ask who "priority fee" 256 provider "eth_maxPriorityFeePerGas")))
  (define cap (or (fill-order-max-fee-per-gas order) (fee-cap who base-fee priority)))
  (cond
    [(< cap priority)
     (raise-fill who 'fee-cap "#:max-fee-per-gas is ~a wei, below the node's priority fee of ~a wei"
                 cap priority)]
    [else (values priority cap)]))

(define (gas-price who order provider)
  (or (fill-order-gas-price order) (ask who "gas price" 256 provider "eth_gasPrice")))

(define (build who type order to chain-id nonce gas-limit base-fee provider)
  (define value (fill-order-value order))
  (define data (fill-order-data order))
  (define access-list (or (fill-order-access-list order) '()))
  (match type
    [2
     (define-values (priority cap) (eip1559-fees who order base-fee provider))
     (eip1559-transaction #:chain-id chain-id #:nonce nonce #:max-priority-fee-per-gas priority
                          #:max-fee-per-gas cap #:gas-limit gas-limit #:to to #:value value
                          #:data data #:access-list access-list)]
    [1
     (eip2930-transaction #:chain-id chain-id #:nonce nonce
                          #:gas-price (gas-price who order provider) #:gas-limit gas-limit
                          #:to to #:value value #:data data #:access-list access-list)]
    [0
     (legacy-transaction #:chain-id chain-id #:nonce nonce
                         #:gas-price (gas-price who order provider) #:gas-limit gas-limit
                         #:to to #:value value #:data data)]))

(define (fill who from to order)
  (define provider (fill-order-provider order))
  (define rows (given-rows order))
  (check-error-signatures/who who (fill-order-errors order))
  (check-conflicts who order rows)
  (define address (sender-address who from))
  (define candidates (candidate-types rows))
  (define fixed (fixed-type order candidates))
  (define chain-id (node-chain-id who order provider))
  (define gas-limit (filled-gas-limit who address to order provider))
  (define nonce (filled-nonce who address order provider))
  (define block? (block-needed? order fixed))
  (define base-fee (and block? (latest-base-fee who provider)))
  (define type (final-type who fixed candidates block? base-fee))
  (values address (build who type order to chain-id nonce gas-limit base-fee provider)))

;;;; sending

(define (signed-for who signer tx address provider)
  (define stx
    (parameterize ([current-provider provider])
      (signer-sign-transaction/who who signer tx)))
  (define sender (signed-transaction-sender stx))
  (if (equal? sender address)
      stx
      (raise-signer who 'wrong-signer
                    "the transaction's sender is ~a, not the address it was filled for ~a"
                    sender address)))

(define (send-raw who signer to order)
  (define provider (fill-order-provider order))
  (define-values (address tx) (fill who signer to order))
  (define stx (signed-for who signer tx address provider))
  (define tx-hash (signed-transaction-hash stx))
  (define raw (bytes->hex-string (signed-transaction->bytes stx)))
  (define answer
    (call-raising-reverts (fill-order-errors order)
                          (lambda ()
                            (rpc-call "eth_sendRawTransaction" (list raw) #:provider provider))))
  (cond
    [(and (string? answer) (string=? (string-downcase answer) tx-hash)) stx]
    [else
     (raise (exn:fail:web3 (format "~a: sent transaction ~a, but the node answered ~e"
                                   who tx-hash answer)
                           (current-continuation-marks)))]))

(define (replay-object stx)
  (define tx (signed-transaction-transaction stx))
  (call-object #:from (signed-transaction-sender stx) #:to (transaction-to tx)
               #:data (transaction-data tx) #:value (transaction-value tx)
               #:gas (transaction-gas-limit tx)
               #:access-list (and (positive? (transaction-type tx)) (transaction-access-list tx))))

(define (send-raw/receipt who signer to order)
  (define stx (send-raw who signer to order))
  (define tx-hash (signed-transaction-hash stx))
  (values tx-hash
          (settle who tx-hash (replay-object stx) (fill-order-errors order)
                  (fill-order-provider order))))

(define/filling (fill-transaction who from to order)
  (define-values (_address tx) (fill who from to order))
  tx)

(define/filling (send-signed-transaction who signer to order)
  (signed-transaction-hash (send-raw who signer to order)))

(define/filling (send-signed-transaction/receipt who signer to order)
  (define-values (_tx-hash receipt) (send-raw/receipt who signer to order))
  receipt)

;;;; contract paths

(define (check-width who kw v bits)
  (when (>= v (expt 2 bits))
    (raise-conflict who "~a is more than a transaction's ~a bits hold" kw bits)))

(define (send-from-signer who signer to data value gas errors provider)
  (check-width who '#:value value 256)
  (check-width who '#:gas (or gas 0) 64)
  (send-raw/receipt who signer to
                    (fill-order #:value value #:data data #:gas-limit gas #:errors errors
                                #:provider provider)))

(define (send-from-node who address to data value gas errors provider)
  (define tx (call-object #:to to #:from address #:data data #:value value #:gas gas))
  (define tx+gas
    (cond
      [gas tx]
      [else (hash-set tx 'gas (number->hex-string (estimate-gas who tx errors provider)))]))
  (define tx-hash
    (call-raising-reverts errors (lambda () (eth-send-transaction tx+gas #:provider provider))))
  (values tx-hash (settle who tx-hash tx+gas errors provider)))

(define (submit who sender #:to to #:data data #:value value #:gas gas #:errors errors
                #:provider provider)
  (cond
    [(signer? sender) (send-from-signer who sender to data value gas errors provider)]
    [else (send-from-node who sender to data value gas errors provider)]))
