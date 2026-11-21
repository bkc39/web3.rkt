#lang racket/base

(require racket/contract/base
         (only-in racket/contract/combinator raise-blame-error)
         (only-in keccak keccak256)
         (only-in racket/match match match* struct*)
         (only-in racket/string string-contains? string-join)
         syntax/parse/define
         "abi.rkt"
         (submod "abi/description.rkt" private)
         (only-in (submod "abi/event.rkt" internal) event-where/c)
         (only-in (submod "abi/event-type.rkt" private) spec->abi-event/who)
         "address.rkt"
         (only-in (submod "address.rkt" private) digest->address)
         (only-in "eth.rkt" block-param/c)
         "event.rkt"
         (only-in "rlp.rkt" natural->rlp-scalar rlp-encode)
         "rpc.rkt"
         "private/abi-json.rkt"
         "private/bindings.rkt"
         "private/contract-call.rkt"
         "private/exn.rkt"
         (only-in "private/hex.rkt" hex-data-string?)
         (only-in "private/contracts.rkt" errors/c from/c provider/c receipt/c)
         "private/who.rkt")

(define-syntax-parse-rule (transaction->* (required:expr ...) (optional ...) result:expr)
  (->* (required ...)
       (optional ...
        #:from from/c
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
          [current-account (parameter/c from/c)]
          [contract-call call/c]
          [contract-send send/c]
          [contract-estimate-gas estimate/c]
          [contract-instance (->* (contract-abi? (abi-value/c 'address))
                                  (#:provider (or/c #f provider?)
                                   #:from from/c)
                                  contract-instance?)]
          [contract-instance? (-> any/c boolean?)]
          [contract-instance-abi (-> contract-instance? contract-abi?)]
          [contract-instance-address (-> contract-instance? checksum-address?)]
          [contract-instance-provider (-> contract-instance? (or/c #f provider?))]
          [contract-instance-from (-> contract-instance? from/c)]
          [contract-logs
           (->i ([target target/c])
                ([event (target) (event/c target)]
                 #:where [where (event) (where/c event)]
                 #:from-block [from block-param/c]
                 #:to-block [to block-param/c]
                 #:chunk [chunk (or/c #f exact-positive-integer?)]
                 #:provider [provider provider/c])
                [_ (listof log?)])]
          [deploy-contract-instance
           (->i ([d deployable-abi/c])
                (#:from [from from/c]
                 #:value [value (d) (value/c d)]
                 #:gas [gas (or/c #f exact-positive-integer?)]
                 #:errors [errors errors/c]
                 #:provider [provider provider/c])
                #:rest [args list?]
                [_ contract-instance?])]
          [read-contract-abi (->* () (input-port?) contract-abi?)]
          [file->contract-abi (-> path-string? contract-abi?)]
          [deploy-contract (transaction->* (init-code/c) (signature/c) checksum-address?)]
          [deploy-contract/receipt
           (transaction->* (init-code/c) (signature/c) (values checksum-address? receipt/c))]
          [create-address (-> (abi-value/c 'address) nonce/c checksum-address?)]
          [create2-address (-> (abi-value/c 'address) salt/c init-code/c checksum-address?)]))

(define (count-of n what)
  (and (positive? n) (format "~a ~a~a" n what (if (= n 1) "" "s"))))

(define (write-instance i port _mode)
  (define d (contract-instance-abi i))
  (define counts
    (filter values
            (list (count-of (length (contract-abi-functions d)) "function")
                  (count-of (length (contract-abi-events d)) "event")
                  (count-of (length (contract-abi-errors d)) "error"))))
  (fprintf port "#<contract-instance ~a~a>"
           (contract-instance-address i)
           (if (null? counts) "" (string-append " " (string-join counts ", ")))))

(struct contract-instance (abi address provider from) #:transparent
  #:name contract-instance-struct
  #:constructor-name raw-contract-instance
  #:property prop:custom-write write-instance)

(define signature/c (or/c string? abi-signature?))
(define init-code/c
  (or/c bytes?
        (flat-named-contract '(and/c string? #px"^0x(?:[0-9a-fA-F]{2})*$") hex-data-string?)))
(define salt/c
  (or/c (and/c bytes? (property/c bytes-length (=/c 32))) (integer-in 0 (sub1 (expt 2 256)))))
(define nonce/c (integer-in 0 (- (expt 2 64) 2)))

(define target/c
  (flat-named-contract 'target/c (or/c (abi-value/c 'address) contract-instance?)))

(define named-function/c
  (flat-named-contract 'named-function/c (or/c symbol? string? abi-signature?)))

(define signature-function/c
  (flat-named-contract 'signature-function/c
                       (or/c abi-signature? (and/c string? (lambda (s) (string-contains? s "("))))))

(define (function/c target)
  (if (contract-instance? target) named-function/c signature-function/c))

(define named-event/c (flat-named-contract 'named-event/c (or/c #f abi-event-spec/c symbol?)))

(define (event/c target)
  (if (contract-instance? target) named-event/c (or/c #f abi-event-spec/c)))

(define (supplied? v)
  (not (unsupplied-arg? v)))

(define no-where/c (flat-named-contract 'no-where/c (lambda (v) (and (hash? v) (hash-empty? v)))))

(define (where/c event)
  (if (and (supplied? event) event) hash? no-where/c))

(define constructor-value/c
  (flat-contract-with-explanation
   (lambda (v)
     (or (eqv? v 0)
         (lambda (blame)
           (raise-blame-error blame v '("~a" given: "~e")
                              "the constructor is nonpayable; #:value must be 0" v))))
   #:name 'constructor-value/c))

(define (value/c d)
  (define constructor (and (contract-abi? d) (contract-abi-constructor d)))
  (if (and constructor (eq? (abi-constructor-state-mutability constructor) 'payable))
      exact-nonnegative-integer?
      (and/c exact-nonnegative-integer? constructor-value/c)))

(define deployable-abi/c
  (flat-named-contract 'deployable-abi/c
                       (lambda (v) (and (contract-abi? v) (contract-abi-bytecode v) #t))))

(define call/c
  (->i ([target target/c]
        [function (target) (function/c target)])
       (#:from [from from/c]
        #:value [value exact-nonnegative-integer?]
        #:gas [gas (or/c #f exact-positive-integer?)]
        #:block [block block-param/c]
        #:errors [errors errors/c]
        #:provider [provider provider/c])
       #:rest [args list?]
       any))

(define send/c
  (->i ([target target/c]
        [function (target) (function/c target)])
       (#:from [from from/c]
        #:value [value exact-nonnegative-integer?]
        #:gas [gas (or/c #f exact-positive-integer?)]
        #:errors [errors errors/c]
        #:provider [provider provider/c])
       #:rest [args list?]
       [_ receipt/c]))

(define estimate/c
  (->i ([target target/c]
        [function (target) (function/c target)])
       (#:from [from from/c]
        #:value [value exact-nonnegative-integer?]
        #:errors [errors errors/c]
        #:provider [provider provider/c])
       #:rest [args list?]
       [_ exact-nonnegative-integer?]))

(define (contract-instance abi address #:provider [provider #f] #:from [from #f])
  (raw-contract-instance abi (address->checksum-string address) provider from))

(define unset (string->uninterned-symbol "unset"))

(define (given v default)
  (if (eq? v unset) (default) v))

(define (raise-abi-error who fmt . args)
  (raise (exn:fail:web3:abi (format "~a: ~a" who (apply format fmt args))
                            (current-continuation-marks))))

(define (check-value who f value)
  (when (and (positive? value)
             (abi-function? f)
             (not (eq? (abi-function-state-mutability f) 'payable)))
    (raise-abi-error who "~a is ~a; #:value must be 0"
                     (abi-signature-name f) (abi-function-state-mutability f))))

(define (request who target function args from value errors provider)
  (define-values (address f sender errors* node)
    (match target
      [(struct* contract-instance-struct ([abi d] [address address] [provider p] [from f]))
       (values address
               (if (abi-signature? function)
                   function
                   (resolve-function who d function (length args)))
               (given from (lambda () (or f (current-account))))
               (append (contract-abi-errors d) errors)
               (given provider (lambda () (or p (current-provider)))))]
      [_ (values target
                 function
                 (given from current-account)
                 errors
                 (given provider current-provider))]))
  (check-value who f value)
  (values address f sender errors* node))

(define/who (contract-call who target function
                           #:from [from unset]
                           #:value [value 0]
                           #:gas [gas #f]
                           #:block [block "latest"]
                           #:errors [errors '()]
                           #:provider [provider unset]
                           . args)
  (define-values (address f sender errors* node)
    (request who target function args from value errors provider))
  (call-function who address f args
                 #:from sender #:value value #:gas gas #:block block #:errors errors*
                 #:provider node))

(define/who (contract-send who target function
                           #:from [from unset]
                           #:value [value 0]
                           #:gas [gas #f]
                           #:errors [errors '()]
                           #:provider [provider unset]
                           . args)
  (define-values (address f sender errors* node)
    (request who target function args from value errors provider))
  (send-function who address f args
                 #:from sender #:value value #:gas gas #:errors errors* #:provider node))

(define/who (contract-estimate-gas who target function
                                   #:from [from unset]
                                   #:value [value 0]
                                   #:errors [errors '()]
                                   #:provider [provider unset]
                                   . args)
  (define-values (address f sender errors* node)
    (request who target function args from value errors provider))
  (estimate-function who address f args
                     #:from sender #:value value #:errors errors* #:provider node))

(define (target-event who target event)
  (match* (target event)
    [((struct* contract-instance-struct ([abi d])) (or (? symbol?) (? string?) (? abi-event?)))
     (resolve-event who d event)]
    [((struct* contract-instance-struct ([abi d])) _)
     (resolve-event who d (spec->abi-event/who who event))]
    [(_ _) (spec->abi-event/who who event)]))

(define/who (contract-logs who target [event #f]
                           #:where [where (hasheq)]
                           #:from-block [from "earliest"]
                           #:to-block [to "latest"]
                           #:chunk [chunk #f]
                           #:provider [provider unset])
  (define e (and event (target-event who target event)))
  (when (and e (not ((flat-contract-predicate (event-where/c who e)) where)))
    (raise-abi-error who "#:where names no indexed parameter of ~a, or one twice: ~e"
                     (abi-event-name e) where))
  (define-values (address events node)
    (match target
      [(struct* contract-instance-struct ([abi d] [address address] [provider p]))
       (values address
               (if e (list e) (contract-abi-events d))
               (given provider (lambda () (or p (current-provider)))))]
      [_ (values target (if e (list e) '()) (given provider current-provider))]))
  (get-logs (log-filter #:address address #:event e #:where where #:from-block from #:to-block to)
            #:events events
            #:chunk chunk
            #:provider node))

(define (deploy-contract-instance d
                                  #:from [from (current-account)]
                                  #:value [value 0]
                                  #:gas [gas #f]
                                  #:errors [errors '()]
                                  #:provider [provider #f]
                                  . args)
  (define-values (address _receipt)
    (deploy-code 'deploy-contract-instance
                 (contract-abi-bytecode d)
                 (or (contract-abi-constructor d) "constructor()")
                 args
                 #:from from
                 #:value value
                 #:gas gas
                 #:errors (append (contract-abi-errors d) errors)
                 #:provider (or provider (current-provider))))
  (contract-instance d address #:provider provider))

(define (read-contract-abi [in (current-input-port)])
  (read-contract-abi/who 'read-contract-abi in))

(define (file->contract-abi path)
  (file->contract-abi/who 'file->contract-abi path))

(define/who (deploy-contract/receipt who init-code [constructor "constructor()"]
                                     #:from [from (current-account)]
                                     #:value [value 0]
                                     #:gas [gas #f]
                                     #:errors [errors '()]
                                     #:provider [provider (current-provider)]
                                     . args)
  (deploy-code who init-code constructor args
               #:from from #:value value #:gas gas #:errors errors #:provider provider))

(define/who (deploy-contract who init-code [constructor "constructor()"]
                             #:from [from (current-account)]
                             #:value [value 0]
                             #:gas [gas #f]
                             #:errors [errors '()]
                             #:provider [provider (current-provider)]
                             . args)
  (define-values (address _receipt)
    (deploy-code who init-code constructor args
                 #:from from #:value value #:gas gas #:errors errors #:provider provider))
  address)

(define (create-address sender nonce)
  (digest->address
   (keccak256 (rlp-encode (list (hex-string->bytes sender) (natural->rlp-scalar nonce))))))

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
