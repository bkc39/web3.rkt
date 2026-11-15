#lang racket/base

(require (for-syntax racket/base)
         racket/contract/base
         (only-in "abi/description.rkt"
                  abi-function?
                  abi-function-state-mutability
                  contract-abi?
                  contract-abi-bytecode)
         (only-in "eth/log.rkt" log?)
         (only-in json jsexpr?)
         (only-in keccak keccak256)
         (only-in racket/match match struct*)
         syntax/parse/define
         "abi.rkt"
         (submod "abi.rkt" internal)
         (only-in (submod "abi/type.rkt" private) raise-abi-type-error)
         "address.rkt"
         "eth.rkt"
         "rpc.rkt"
         "private/exn.rkt"
         (only-in "private/hex.rkt" hex-data-string?)
         "private/node.rkt"
         "private/who.rkt")

(define-syntax-parse-rule (transaction->* (required:expr ...) (optional ...) result:expr)
  (->* (required ...)
       (optional ...
        #:from (or/c #f (abi-value/c 'address))
        #:value exact-nonnegative-integer?
        #:gas (or/c #f exact-positive-integer?)
        #:errors errors/c
        #:provider (or/c #f provider?))
       #:rest list?
       result))

(provide (struct-out exn:fail:web3:transaction)
         define-contract
         contract-bindings-out
         (contract-out
          [current-account (parameter/c (or/c #f (abi-value/c 'address)))]
          [contract-call call/c]
          [contract-send send/c]
          [contract-estimate-gas estimate/c]
          [contract-instance (->* (contract-abi? (abi-value/c 'address))
                                  (#:provider (or/c #f provider?)
                                   #:from (or/c #f (abi-value/c 'address)))
                                  contract-instance?)]
          [contract-instance? (-> any/c boolean?)]
          [contract-instance-abi (-> contract-instance? contract-abi?)]
          [contract-instance-address (-> contract-instance? checksum-address?)]
          [contract-instance-provider (-> contract-instance? (or/c #f provider?))]
          [contract-instance-from (-> contract-instance? (or/c #f (abi-value/c 'address)))]
          [contract-logs
           (->i ([target target/c])
                ([event (target) (event-spec/c target)]
                 #:where [where (target event) (where-for/c target event)]
                 #:from-block [from block/c]
                 #:to-block [to block/c]
                 #:chunk [chunk (or/c #f exact-positive-integer?)]
                 #:provider [provider (or/c #f provider?)])
                [_ (listof log?)])]
          [deploy-contract-instance
           (->i ([d deployable-abi/c])
                (#:from [from (or/c #f (abi-value/c 'address))]
                 #:value [value (d) (constructor-value/c d)]
                 #:gas [gas (or/c #f exact-positive-integer?)]
                 #:errors [errors errors/c]
                 #:provider [provider (or/c #f provider?)])
                #:rest [args list?]
                [_ contract-instance?])]
          [read-contract-abi (->* () (input-port?) contract-abi?)]
          [file->contract-abi (-> path-string? contract-abi?)]
          [deploy-contract (transaction->* (init-code/c) (signature/c) checksum-address?)]
          [deploy-contract/receipt
           (transaction->* (init-code/c) (signature/c) (values checksum-address? receipt/c))]
          [create-address (-> (abi-value/c 'address) nonce/c checksum-address?)]
          [create2-address (-> (abi-value/c 'address) salt/c init-code/c checksum-address?)]))

(define signature/c (or/c string? abi-signature?))
(define errors/c (listof (or/c string? abi-signature?)))
(define init-code/c
  (or/c bytes? (flat-named-contract '(and/c string? #px"^0x(?:[0-9a-fA-F]{2})*$") hex-data-string?)))
(define salt/c
  (or/c (and/c bytes? (property/c bytes-length (=/c 32))) (integer-in 0 (sub1 (expt 2 256)))))
(define nonce/c (integer-in 0 (- (expt 2 64) 2)))
(define receipt/c (and/c hash? jsexpr?))
(define block/c (flat-named-contract 'block-param/c block-param/c))

(define unimplemented
  (make-keyword-procedure (lambda _ (error 'unimplemented))))

(struct contract-instance (abi address provider from)
  #:name contract-instance-struct #:constructor-name raw-contract-instance)

(define (contract-instance abi address #:provider [provider #f] #:from [from #f])
  (unimplemented))

(define target/c
  (flat-named-contract 'target/c (or/c (abi-value/c 'address) contract-instance?)))

(define (function-spec/c target args) unimplemented)
(define (value-ok/c target function args) unimplemented)
(define (event-spec/c target) unimplemented)
(define (where-for/c target event) unimplemented)
(define (constructor-value/c d) unimplemented)

(define deployable-abi/c
  (flat-named-contract 'deployable-abi/c (lambda (v) (and (contract-abi? v) (contract-abi-bytecode v) #t))))

(define call/c
  (->i ([target target/c]
        [function (target args) (function-spec/c target args)])
       (#:from [from (or/c #f (abi-value/c 'address))]
        #:value [value (target function args) (value-ok/c target function args)]
        #:gas [gas (or/c #f exact-positive-integer?)]
        #:block [block block/c]
        #:errors [errors errors/c]
        #:provider [provider (or/c #f provider?)])
       #:rest [args list?]
       any))

(define send/c
  (->i ([target target/c]
        [function (target args) (function-spec/c target args)])
       (#:from [from (or/c #f (abi-value/c 'address))]
        #:value [value (target function args) (value-ok/c target function args)]
        #:gas [gas (or/c #f exact-positive-integer?)]
        #:errors [errors errors/c]
        #:provider [provider (or/c #f provider?)])
       #:rest [args list?]
       [_ receipt/c]))

(define estimate/c
  (->i ([target target/c]
        [function (target args) (function-spec/c target args)])
       (#:from [from (or/c #f (abi-value/c 'address))]
        #:value [value (target function args) (value-ok/c target function args)]
        #:errors [errors errors/c]
        #:provider [provider (or/c #f provider?)])
       #:rest [args list?]
       [_ exact-nonnegative-integer?]))

(define contract-estimate-gas unimplemented)
(define contract-logs unimplemented)
(define deploy-contract-instance unimplemented)
(define read-contract-abi unimplemented)
(define file->contract-abi unimplemented)

(define-syntax (define-contract stx)
  (raise-syntax-error 'define-contract "unimplemented" stx))

(define-syntax (contract-bindings-out stx)
  (raise-syntax-error 'contract-bindings-out "unimplemented" stx))

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

(define/who (contract-call who address signature
                           #:from [from (current-account)]
                           #:value [value 0]
                           #:gas [gas #f]
                           #:block [block "latest"]
                           #:errors [errors '()]
                           #:provider [provider (current-provider)]
                           . args)
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

(define/who (contract-send who address signature
                           #:from [from (current-account)]
                           #:value [value 0]
                           #:gas [gas #f]
                           #:errors [errors '()]
                           #:provider [provider (current-provider)]
                           . args)
  (define sig (string->abi-signature/who who signature))
  (check-error-signatures/who who errors)
  (define data (apply encode-call/who who sig args))
  (define-values (_tx-hash receipt)
    (submit who
            (transaction #:to address #:from (sender who from) #:data data #:value value #:gas gas)
            errors
            provider))
  receipt)

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

(define/who (deploy-contract/receipt who init-code [constructor "constructor()"]
                                     #:from [from (current-account)]
                                     #:value [value 0]
                                     #:gas [gas #f]
                                     #:errors [errors '()]
                                     #:provider [provider (current-provider)]
                                     . args)
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

(define/who (deploy-contract who init-code [constructor "constructor()"]
                             #:from [from (current-account)]
                             #:value [value 0]
                             #:gas [gas #f]
                             #:errors [errors '()]
                             #:provider [provider (current-provider)]
                             . args)
  (define-values (address _receipt)
    (apply deploy-contract/receipt/who who init-code constructor
           #:from from #:value value #:gas gas #:errors errors #:provider provider
           args))
  address)

(define (digest->address digest)
  (address->checksum-string (bytes->hex-string (subbytes digest 12))))

(define (natural->minimal-bytes n)
  (let loop ([n n] [acc '()])
    (if (zero? n)
        (apply bytes acc)
        (loop (arithmetic-shift n -8) (cons (bitwise-and n 255) acc)))))

(define (create-preimage sender nonce)
  (define encoded-nonce
    (cond
      [(zero? nonce) (bytes #x80)]
      [(< nonce 128) (bytes nonce)]
      [else
       (define bs (natural->minimal-bytes nonce))
       (bytes-append (bytes (+ #x80 (bytes-length bs))) bs)]))
  (define payload (bytes-append (bytes #x94) sender encoded-nonce))
  (bytes-append (bytes (+ #xc0 (bytes-length payload))) payload))

(define (create-address sender nonce)
  (digest->address (keccak256 (create-preimage (hex-string->bytes sender) nonce))))

(define (create2-address deployer salt init-code)
  (define salt-bytes (if (bytes? salt) salt (word->bytes salt)))
  (digest->address
   (keccak256 (bytes-append #"\377"
                            (hex-string->bytes deployer)
                            salt-bytes
                            (keccak256 (init-code->bytes init-code))))))

(module* internal #f
  (provide contract-call/who
           contract-send/who
           deploy-contract/who
           receipt-timeout))
