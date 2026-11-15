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

(define (transaction #:to [to #f] #:from from #:data data #:value value #:gas gas)
  (for/fold ([tx (hasheq 'data (bytes->hex-string data))])
            ([key (in-list '(to from value gas))]
             [v (in-list (list to
                               from
                               (and (positive? value) (number->hex-string value))
                               (and gas (number->hex-string gas))))]
             #:when v)
    (hash-set tx key v)))

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
  (match (abi-signature-outputs sig)
    ['() (values)]
    [_ (apply values (decode-result/who who sig (node-data->bytes who "return data" result)))]))

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
  (submit who
          (transaction #:to address #:from (sender who from) #:data data #:value value #:gas gas)
          errors
          provider))

(define (submit who tx errors provider)
  (define tx+gas
    (cond
      [(hash-ref tx 'gas #f) tx]
      [else
       (define estimate
         (call-raising-reverts errors (lambda () (eth-estimate-gas tx #:provider provider))))
       (hash-set tx 'gas (number->hex-string estimate))]))
  (define tx-hash
    (call-raising-reverts errors (lambda () (eth-send-transaction tx+gas #:provider provider))))
  (define receipt (wait-for-receipt tx-hash #:provider provider))
  (match (receipt-status who receipt)
    ['success receipt]
    ['failure (raise-transaction-failure who tx+gas tx-hash receipt errors provider)]))

(define (raise-transaction-failure who tx tx-hash receipt errors provider)
  (define block (node-quantity->number who "block number" (hash-ref receipt 'blockNumber #f)))
  (match-define (list revert tail)
    (with-handlers ([exn:fail:web3:rpc?
                     (lambda (e)
                       (match (rpc-exn->revert e #:errors errors)
                         [#f (list #f (exn:fail:web3:rpc-rpc-message e))]
                         [r (list r (format "reverted with ~a" (abi-revert->string r)))]))])
      (eth-call tx block #:provider provider)
      (list #f "replaying it did not fail")))
  (raise (exn:fail:web3:transaction
          (format "~a: transaction ~a failed in block ~a: ~a" who tx-hash block tail)
          (current-continuation-marks)
          receipt
          revert)))

(define (constructor-signature who constructor)
  (match (string->abi-signature/who who constructor)
    [(and sig (struct* abi-signature ([name "constructor"] [outputs #f]))) sig]
    [sig (raise-abi-type-error who "a constructor signature is constructor(T,...)"
                               (abi-signature->string sig #:outputs? #t) #f)]))

(define (init-code->bytes code)
  (if (bytes? code) code (hex-string->bytes code)))

(define (deploy who init-code constructor from value gas errors provider args)
  (define sig (constructor-signature who constructor))
  (check-error-signatures/who who errors)
  (define data
    (bytes-append (init-code->bytes init-code) (abi-encode/who who (abi-signature-inputs sig) args)))
  (define receipt
    (submit who (transaction #:from (sender who from) #:data data #:value value #:gas gas)
            errors
            provider))
  (values (node-address->string who "contract address" (hash-ref receipt 'contractAddress #f))
          receipt))

(define/who (deploy-contract who init-code [constructor "constructor()"]
                             #:from [from (current-account)]
                             #:value [value 0]
                             #:gas [gas #f]
                             #:errors [errors '()]
                             #:provider [provider (current-provider)]
                             . args)
  (define-values (address _receipt)
    (deploy who init-code constructor from value gas errors provider args))
  address)

(define/who (deploy-contract/receipt who init-code [constructor "constructor()"]
                                     #:from [from (current-account)]
                                     #:value [value 0]
                                     #:gas [gas #f]
                                     #:errors [errors '()]
                                     #:provider [provider (current-provider)]
                                     . args)
  (deploy who init-code constructor from value gas errors provider args))

(define (hash->address h)
  (address->checksum-string (bytes->hex-string (subbytes h 12))))

(define (natural->minimal-bytes n)
  (let loop ([n n] [acc '()])
    (if (zero? n)
        (apply bytes acc)
        (loop (arithmetic-shift n -8) (cons (bitwise-and n 255) acc)))))

;; RLP of [sender, nonce]; #13's rlp-encode replaces it.
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
  (hash->address (keccak256 (create-preimage (hex-string->bytes sender) nonce))))

(define (create2-address deployer salt init-code)
  (define salt-bytes (if (bytes? salt) salt (word->bytes salt)))
  (hash->address
   (keccak256 (bytes-append #"\377"
                            (hex-string->bytes deployer)
                            salt-bytes
                            (keccak256 (init-code->bytes init-code))))))

(module* internal #f
  (provide contract-call/who
           contract-send/who
           deploy-contract/who))
