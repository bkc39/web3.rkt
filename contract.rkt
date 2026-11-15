#lang racket/base

(require racket/contract/base
         (only-in json jsexpr?)
         (only-in keccak keccak256)
         (only-in racket/match match match* struct*)
         (only-in racket/string string-contains? string-join)
         syntax/parse/define
         "abi.rkt"
         (submod "abi/description.rkt" private)
         (only-in (submod "abi/event.rkt" internal) event-where/c)
         (only-in (submod "abi/event-type.rkt" private) spec->abi-event/who)
         "address.rkt"
         (only-in "eth.rkt" block-param/c)
         "event.rkt"
         "rpc.rkt"
         "private/abi-json.rkt"
         "private/bindings.rkt"
         "private/exn.rkt"
         (only-in "private/hex.rkt" hex-data-string?)
         "private/transaction.rkt"
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

;;;; instances

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

;;;; contracts

(define signature/c (or/c string? abi-signature?))
(define errors/c (listof (or/c string? abi-signature?)))
(define init-code/c
  (or/c bytes? (flat-named-contract '(and/c string? #px"^0x(?:[0-9a-fA-F]{2})*$") hex-data-string?)))
(define salt/c
  (or/c (and/c bytes? (property/c bytes-length (=/c 32))) (integer-in 0 (sub1 (expt 2 256)))))
(define nonce/c (integer-in 0 (- (expt 2 64) 2)))
(define receipt/c (and/c hash? jsexpr?))
(define block/c (flat-named-contract 'block-param/c block-param/c))

(define target/c
  (flat-named-contract 'target/c (or/c (abi-value/c 'address) contract-instance?)))

(define (supplied? v)
  (not (unsupplied-arg? v)))

;; The function an instance's key names, or the reason there is none; any
;; other key as it is.
(define (target-function who target key n)
  (match* (target key)
    [((? contract-instance?) (or (? symbol?) (? string?)))
     (function-resolution who (contract-instance-abi target) key n)]
    [(_ _) key]))

(define (function-spec/c target args)
  (define (resolve key)
    (match* (target key)
      [((? contract-instance?) (or (? symbol?) (? string?) (? abi-signature?)))
       (target-function 'contract-call target key (length args))]
      [((? contract-instance?) _) "expected a symbol, a string or an abi-signature"]
      [(_ (? abi-signature?)) #t]
      [(_ (? string?)) #:when (string-contains? key "(") #t]
      [(_ (or (? symbol?) (? string?))) "a function name needs a contract-instance; pass a signature"]
      [(_ _) "expected a string or an abi-signature"]))
  (resolution/c 'function-spec/c resolve))

(define (no-value/c what mutability)
  (and/c exact-nonnegative-integer?
         (resolution/c 'value/c
                       (lambda (v) (or (zero? v) (format "~a is ~a; #:value must be 0" what mutability))))))

(define (value-ok/c target function args)
  (match (target-function 'contract-call target function (length args))
    [(? abi-function? f)
     #:when (not (eq? (abi-function-state-mutability f) 'payable))
     (no-value/c (abi-signature-name f) (abi-function-state-mutability f))]
    [_ exact-nonnegative-integer?]))

(define (event-spec/c target)
  (if (contract-instance? target)
      (resolution/c 'event-spec/c
                    (lambda (key)
                      (or (not key) (event-resolution 'contract-logs (contract-instance-abi target) key))))
      (or/c #f abi-event-spec/c)))

(define no-where/c (flat-named-contract 'no-where/c (lambda (v) (and (hash? v) (hash-empty? v)))))

(define (where-for/c target event)
  (match (and (supplied? event) event (target-event target event))
    [(? abi-event? e) (event-where/c 'contract-logs e)]
    [_ no-where/c]))

(define (constructor-value/c d)
  (match (and (contract-abi? d) (contract-abi-constructor d))
    [(? abi-constructor? c)
     #:when (eq? (abi-constructor-state-mutability c) 'payable)
     exact-nonnegative-integer?]
    [_ (no-value/c "the constructor" 'nonpayable)]))

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

(define (contract-instance abi address #:provider [provider #f] #:from [from #f])
  (raw-contract-instance abi (address->checksum-string address) provider from))

(define unset (string->uninterned-symbol "unset"))

(define (given v default)
  (if (eq? v unset) (default) v))

(define (resolved-function who d key n)
  (match (function-resolution who d key n)
    [(? string? reason) (raise-arguments-error who reason "function" key)]
    [f f]))

;; The address, function, sender, errors and provider of a request.
(define (request who target function args from errors provider)
  (match target
    [(struct* contract-instance-struct ([abi d] [address address] [provider p] [from f]))
     (values address
             (if (abi-signature? function) function (resolved-function who d function (length args)))
             (given from (lambda () (or f (current-account))))
             (append (contract-abi-errors d) errors)
             (given provider (lambda () (or p (current-provider)))))]
    [_ (values target function (given from current-account) errors (given provider current-provider))]))

(define/who (contract-call who target function
                           #:from [from unset]
                           #:value [value 0]
                           #:gas [gas #f]
                           #:block [block "latest"]
                           #:errors [errors '()]
                           #:provider [provider unset]
                           . args)
  (define-values (address f sender errors* node) (request who target function args from errors provider))
  (call-function who address f args
                 #:from sender #:value value #:gas gas #:block block #:errors errors* #:provider node))

(define/who (contract-send who target function
                           #:from [from unset]
                           #:value [value 0]
                           #:gas [gas #f]
                           #:errors [errors '()]
                           #:provider [provider unset]
                           . args)
  (define-values (address f sender errors* node) (request who target function args from errors provider))
  (send-function who address f args #:from sender #:value value #:gas gas #:errors errors* #:provider node))

(define/who (contract-estimate-gas who target function
                                   #:from [from unset]
                                   #:value [value 0]
                                   #:errors [errors '()]
                                   #:provider [provider unset]
                                   . args)
  (define-values (address f sender errors* node) (request who target function args from errors provider))
  (estimate-function who address f args #:from sender #:value value #:errors errors* #:provider node))

(define (target-event target event)
  (match* (target event)
    [((? contract-instance?) _) (event-resolution 'contract-logs (contract-instance-abi target) event)]
    [(_ _) (spec->abi-event/who 'contract-logs event)]))

(define (contract-logs target [event #f]
                       #:where [where (hasheq)]
                       #:from-block [from "earliest"]
                       #:to-block [to "latest"]
                       #:chunk [chunk #f]
                       #:provider [provider unset])
  (define e (and event (target-event target event)))
  (define-values (address events node)
    (match target
      [(struct* contract-instance-struct ([abi d] [address address] [provider p]))
       (values address (if e (list e) (contract-abi-events d)) (given provider (lambda () (or p (current-provider)))))]
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
    (deploy-code 'deploy-contract-instance (contract-abi-bytecode d) (or (contract-abi-constructor d) "constructor()")
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

;;;; deploying

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
