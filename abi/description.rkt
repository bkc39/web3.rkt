#lang racket/base

(require racket/contract/base
         (only-in racket/contract/combinator raise-blame-error)
         (only-in racket/list remove-duplicates)
         (only-in racket/match match match* struct*)
         (only-in racket/string string-contains? string-join string-prefix?)
         "event-type.rkt"
         (submod "event-type.rkt" private)
         "type.rkt"
         (submod "type.rkt" private)
         "../private/abi-item.rkt")

(provide
 abi-item-spec/c
 (contract-out
  [abi-parameter (-> (or/c #f abi-parameter-name/c) abi-type-spec/c abi-parameter?)]
  [abi-parameter? (-> any/c boolean?)]
  [abi-parameter-name (-> abi-parameter? (or/c #f string?))]
  [abi-parameter-type (-> abi-parameter? abi-type?)]

  [abi-function (->* (abi-identifier/c (listof parameter-spec/c) (listof parameter-spec/c))
                     (function-mutability/c)
                     abi-function?)]
  [abi-function? (-> any/c boolean?)]
  [abi-function-parameters (-> abi-function? (listof abi-parameter?))]
  [abi-function-returns (-> abi-function? (listof abi-parameter?))]
  [abi-function-state-mutability (-> abi-function? function-mutability/c)]
  [abi-error (-> abi-identifier/c (listof parameter-spec/c) abi-error?)]
  [abi-error? (-> any/c boolean?)]
  [abi-error-parameters (-> abi-error? (listof abi-parameter?))]
  [abi-constructor (->* ((listof parameter-spec/c)) (payability/c) abi-constructor?)]
  [abi-constructor? (-> any/c boolean?)]
  [abi-constructor-parameters (-> abi-constructor? (listof abi-parameter?))]
  [abi-constructor-state-mutability (-> abi-constructor? payability/c)]
  [abi-fallback (->* () (payability/c) abi-fallback?)]
  [abi-fallback? (-> any/c boolean?)]
  [abi-fallback-state-mutability (-> abi-fallback? payability/c)]
  [abi-receive (-> abi-receive?)]
  [abi-receive? (-> any/c boolean?)]

  [abi-item? (-> any/c boolean?)]
  [abi-item (-> abi-item-spec/c abi-item?)]
  [string->abi-item (-> string? abi-item?)]
  [sexp->abi-item (-> pair? abi-item?)]
  [abi-item->string (-> abi-item-spec/c string?)]
  [abi-item->sexp (-> abi-item-spec/c pair?)]
  [jsexpr->abi-item (-> any/c abi-item?)]
  [abi-item->jsexpr (-> abi-item-spec/c hash?)]

  [contract-abi (->* ((listof abi-item-spec/c)) (#:bytecode bytecode/c) contract-abi?)]
  [contract-abi? (-> any/c boolean?)]
  [contract-abi-items (-> contract-abi? (listof abi-item?))]
  [contract-abi-bytecode (-> contract-abi? (or/c #f bytes?))]
  [contract-abi-functions (-> contract-abi? (listof abi-function?))]
  [contract-abi-events (-> contract-abi? (listof abi-event?))]
  [contract-abi-errors (-> contract-abi? (listof abi-error?))]
  [contract-abi-constructor (-> contract-abi? (or/c #f abi-constructor?))]
  [contract-abi-fallback (-> contract-abi? (or/c #f abi-fallback?))]
  [contract-abi-receive (-> contract-abi? (or/c #f abi-receive?))]
  [contract-abi-function (->i ([d contract-abi?] [key (d n) (function-key/c d n)])
                              (#:arity [n exact-nonnegative-integer?])
                              [_ abi-function?])]
  [contract-abi-event (->i ([d contract-abi?] [key (d) (event-key/c d)]) [_ abi-event?])]
  [contract-abi-error (->i ([d contract-abi?] [key (d) (error-key/c d)]) [_ abi-error?])]
  [jsexpr->contract-abi (-> any/c contract-abi?)]
  [contract-abi->jsexpr (-> contract-abi? (listof hash?))]))

;;;; contracts

(define parameter-spec/c
  (flat-named-contract 'parameter-spec/c (lambda (v) (or (abi-parameter? v) (abi-type-spec/c v)))))

(define function-mutability/c
  (flat-named-contract 'function-mutability/c (or/c 'pure 'view 'nonpayable 'payable)))

(define payability/c (flat-named-contract 'payability/c (or/c 'nonpayable 'payable)))

(define (hex-string? v)
  (and (string? v) (regexp-match? #px"^0x(?:[0-9a-fA-F]{2})*$" v)))

(define bytecode/c
  (flat-named-contract 'bytecode/c (lambda (v) (or (not v) (bytes? v) (hex-string? v)))))

(define abi-item-spec/c
  (flat-named-contract 'abi-item-spec/c (lambda (v) (or (abi-item? v) (string? v) (pair? v)))))

;; A flat contract on a key that resolve turns into an item or into the
;; reason it names none.
(define (resolution/c name resolve)
  (flat-contract-with-explanation
   (lambda (key)
     (match (resolve key)
       [(? string? reason) (lambda (blame) (raise-blame-error blame key '("~a" given: "~e") reason key))]
       [_ #t]))
   #:name name))

(define (function-key/c d n)
  (resolution/c 'function-key/c
                (lambda (key) (function-resolution 'contract-abi-function d key (and (not (unsupplied-arg? n)) n)))))

(define (event-key/c d)
  (resolution/c 'event-key/c (lambda (key) (event-resolution 'contract-abi-event d key))))

(define (error-key/c d)
  (resolution/c 'error-key/c (lambda (key) (error-resolution 'contract-abi-error d key))))

;;;; items

(define (abi-parameter name type)
  (abi-parameter/who 'abi-parameter name type))

(define (abi-function name inputs outputs [mutability 'nonpayable])
  (abi-function/who 'abi-function name inputs outputs mutability))

(define (abi-error name inputs)
  (abi-error/who 'abi-error name inputs))

(define (abi-constructor inputs [mutability 'nonpayable])
  (abi-constructor/who 'abi-constructor inputs mutability))

(define (abi-fallback [mutability 'nonpayable])
  (raw-abi-fallback mutability))

(define (abi-receive)
  (raw-abi-receive))

(define (abi-item spec)
  (item/who 'abi-item spec))

(define (string->abi-item s)
  (string->abi-item/who 'string->abi-item s))

(define (sexp->abi-item x)
  (sexp->abi-item/who 'sexp->abi-item x))

(define (abi-item->string spec)
  (item->string (item/who 'abi-item->string spec)))

(define (abi-item->sexp spec)
  (item->sexp (item/who 'abi-item->sexp spec)))

(define (jsexpr->abi-item j)
  (jsexpr->abi-item/who 'jsexpr->abi-item j))

(define (abi-item->jsexpr spec)
  (item->jsexpr (item/who 'abi-item->jsexpr spec)))

;;;; JSON items

(define missing (string->uninterned-symbol "missing"))

(define (missing? v)
  (eq? v missing))

;; thunk's result; a type error it raises under who becomes one whose
;; input is j, its reason prefixed with the item's index when there is one.
(define (in-item/who who index j thunk)
  (define prefix (format "~a: " who))
  (define (rethrow e)
    (define message (exn-message e))
    (define reason (if (string-prefix? message prefix) (substring message (string-length prefix)) message))
    (raise (exn:fail:web3:abi:type (if index (format "~aitem ~a: ~a" prefix index reason) (string-append prefix reason))
                                   (exn-continuation-marks e)
                                   j
                                   #f)))
  (with-handlers ([exn:fail:web3:abi:type? rethrow])
    (thunk)))

(define (jsexpr->abi-item/who who j)
  (define (fail reason)
    (raise-abi-type-error who reason j #f))
  (define (field h key)
    (hash-ref h key missing))
  (define (json-type p)
    (define components (field p 'components))
    (match (field p 'type)
      [(pregexp #px"^tuple((?:\\[[0-9]*\\])*)$" (list _ suffixes))
       (define base
         (if (and (pair? components) (list? components))
             (abi-tuple (for/list ([c (in-list components)])
                          (if (hash? c) (json-type c) (fail "expected a component object"))))
             (fail "a tuple needs a non-empty list of components")))
       (string->type who (string-append (type->string base) suffixes))]
      [(? string? t)
       #:when (missing? components)
       (string->type who t)]
      [(? string?) (fail "components on a type that is not a tuple")]
      [t (fail (format "expected a type string, given ~s" t))]))
  (define (json-name p)
    (match (field p 'name)
      [(or "" (? missing?)) #f]
      [(? parameter-name? name) name]
      [name (fail (format "not a parameter name: ~s" name))]))
  (define (parameters key event?)
    (match (hash-ref j key '())
      [(? list? ps)
       (for/list ([p (in-list ps)])
         (cond
           [(not (hash? p)) (fail "expected a parameter object")]
           [event?
            (match (hash-ref p 'indexed #f)
              [(? boolean? indexed?) (abi-event-parameter (json-name p) (json-type p) indexed?)]
              [v (fail (format "indexed is a boolean, given ~s" v))])]
           [else (raw-abi-parameter (json-name p) (json-type p))]))]
      [v (fail (format "~a is a list, given ~s" key v))]))
  (define (name)
    (match (field j 'name)
      [(? identifier? s) s]
      [v (fail (format "expected a name, given ~s" v))]))
  (define (mutability)
    (match* ((field j 'stateMutability) (hash-ref j 'constant #f) (hash-ref j 'payable #f))
      [((? missing?) #t _) 'view]
      [((? missing?) #f #t) 'payable]
      [((? missing?) #f #f) 'nonpayable]
      [((and m (or "pure" "view" "nonpayable" "payable")) _ _) (string->symbol m)]
      [(m c p) (fail (format "bad mutability: ~s" (if (missing? m) (list c p) m)))]))
  (define (payability what)
    (match (mutability)
      [(and m (or 'nonpayable 'payable)) m]
      [_ (fail (format "a ~a is nonpayable or payable" what))]))
  (define (event)
    (define ps (parameters 'inputs #t))
    (define anonymous?
      (match (hash-ref j 'anonymous #f)
        [(? boolean? b) b]
        [v (fail (format "anonymous is a boolean, given ~s" v))]))
    (match (parameters-failure ps anonymous?)
      [#f (abi-event (name) ps anonymous?)]
      [(cons reason _) (fail reason)]))
  (in-item/who
   who #f j
   (lambda ()
     (match (and (hash? j) (hash-ref j 'type "function"))
       [#f (fail "expected an item object")]
       ["function" (abi-function/who who (name) (parameters 'inputs #f) (parameters 'outputs #f) (mutability))]
       ["event" (event)]
       ["error" (abi-error/who who (name) (parameters 'inputs #f))]
       ["constructor" (abi-constructor/who who (parameters 'inputs #f) (payability "constructor"))]
       ["fallback" (raw-abi-fallback (payability "fallback"))]
       ["receive" (if (eq? (mutability) 'payable) (raw-abi-receive) (fail "a receive is payable"))]
       [type (fail (format "unknown item type: ~s" type))]))))

(define (type->jsexpr name t #:indexed [indexed missing])
  (define-values (base suffixes)
    (let loop ([t t] [suffixes '()])
      (match t
        [(struct* abi-array ([element e])) (loop e (cons "[]" suffixes))]
        [(struct* abi-fixed-array ([element e] [length k])) (loop e (cons (format "[~a]" k) suffixes))]
        [_ (values t (apply string-append suffixes))])))
  (define named (hasheq 'name (or name "")))
  (define typed
    (match base
      [(struct* abi-tuple ([components cs]))
       (hash-set* named
                  'type (string-append "tuple" suffixes)
                  'components (map (lambda (c) (type->jsexpr #f c)) cs))]
      [_ (hash-set named 'type (type->string t))]))
  (if (missing? indexed) typed (hash-set typed 'indexed indexed)))

(define (parameters->jsexpr ps)
  (map (lambda (p) (type->jsexpr (abi-parameter-name p) (abi-parameter-type p))) ps))

(define (item->jsexpr i)
  (match i
    [(? abi-function?)
     (hasheq 'type "function"
             'name (abi-signature-name i)
             'inputs (parameters->jsexpr (abi-function-parameters i))
             'outputs (parameters->jsexpr (abi-function-returns i))
             'stateMutability (symbol->string (abi-function-state-mutability i)))]
    [(? abi-event?)
     (hasheq 'type "event"
             'name (abi-event-name i)
             'anonymous (abi-event-anonymous? i)
             'inputs (for/list ([p (in-list (abi-event-parameters i))])
                       (type->jsexpr (abi-event-parameter-name p) (abi-event-parameter-type p)
                                     #:indexed (abi-event-parameter-indexed? p))))]
    [(? abi-error?)
     (hasheq 'type "error" 'name (abi-signature-name i) 'inputs (parameters->jsexpr (abi-error-parameters i)))]
    [(? abi-constructor?)
     (hasheq 'type "constructor"
             'inputs (parameters->jsexpr (abi-constructor-parameters i))
             'stateMutability (symbol->string (abi-constructor-state-mutability i)))]
    [(? abi-fallback?)
     (hasheq 'type "fallback" 'stateMutability (symbol->string (abi-fallback-state-mutability i)))]
    [(? abi-receive?) (hasheq 'type "receive" 'stateMutability "payable")]))

;;;; descriptions

(define (item-label i)
  (match i
    [(? abi-function?) (format "function ~a" (abi-signature->string i))]
    [(? abi-error?) (format "error ~a" (abi-signature->string i))]
    [(? abi-event?) (format "event ~a" (abi-signature->string (abi-event->signature i)))]
    [(? abi-constructor?) "constructor"]
    [(? abi-fallback?) "fallback"]
    [(? abi-receive?) "receive"]))

;; #f, or (cons reason i) for the first item i that repeats an earlier one.
(define (contract-abi-failure items)
  (let loop ([items items] [i 0] [seen (hash)])
    (match items
      ['() #f]
      [(cons item rest)
       (define label (item-label item))
       (if (hash-ref seen label #f)
           (cons (format "duplicate ~a" label) i)
           (loop rest (add1 i) (hash-set seen label #t)))])))

(define (write-contract-abi d port _mode)
  (define (group label names)
    (and (pair? names) (format "~a: ~a" label (string-join (remove-duplicates names) ", "))))
  (define bytecode (contract-abi-bytecode d))
  (define groups
    (filter values
            (list (group "functions" (map abi-signature-name (contract-abi-functions d)))
                  (group "events" (map abi-event-name (contract-abi-events d)))
                  (group "errors" (map abi-signature-name (contract-abi-errors d)))
                  (and (contract-abi-constructor d) "constructor")
                  (and (contract-abi-fallback d) "fallback")
                  (and (contract-abi-receive d) "receive")
                  (and bytecode (format "bytecode: ~a bytes" (bytes-length bytecode))))))
  (fprintf port "#<contract-abi~a>" (if (null? groups) "" (string-append " " (string-join groups "; ")))))

(struct contract-abi (items bytecode) #:transparent
  #:name contract-abi-struct
  #:constructor-name raw-contract-abi
  #:property prop:custom-write write-contract-abi)

(define (hex->bytes s)
  (apply bytes (for/list ([i (in-range 0 (string-length s) 2)])
                 (string->number (substring s i (+ i 2)) 16))))

(define (bytecode->bytes b)
  (match b
    [(or #f #"" "0x") #f]
    [(? bytes?) b]
    [_ (hex->bytes (substring b 2))]))

(define (contract-abi/who who specs inputs bytecode)
  (define items (for/list ([spec (in-list specs)]) (item/who who spec)))
  (match (contract-abi-failure items)
    [#f (raw-contract-abi items (bytecode->bytes bytecode))]
    [(cons reason i) (raise-abi-type-error who reason (list-ref inputs i) #f)]))

(define (contract-abi specs #:bytecode [bytecode #f])
  (contract-abi/who 'contract-abi specs specs bytecode))

(define ((items-of pred) d)
  (filter pred (contract-abi-items d)))

(define contract-abi-functions (items-of abi-function?))
(define contract-abi-events (items-of abi-event?))
(define contract-abi-errors (items-of abi-error?))

(define ((item-of pred) d)
  (findf pred (contract-abi-items d)))

(define contract-abi-constructor (item-of abi-constructor?))
(define contract-abi-fallback (item-of abi-fallback?))
(define contract-abi-receive (item-of abi-receive?))

(define (artifact-bytecode who j)
  (define (fail reason)
    (raise-abi-type-error who reason j #f))
  (define (lookup path)
    (for/fold ([v j]) ([key (in-list path)])
      (and (hash? v) (hash-ref v key #f))))
  (define hex
    (for*/first ([path (in-list '((bytecode object) (bytecode) (evm bytecode object) (bin)))]
                 [v (in-value (lookup path))]
                 #:when (string? v))
      (if (string-prefix? v "0x") (substring v 2) v)))
  (cond
    [(not hex) #f]
    [(string-contains? hex "__") (fail "bytecode has unlinked library references")]
    [(regexp-match? #px"^(?:[0-9a-fA-F]{2})*$" hex) (bytecode->bytes (hex->bytes hex))]
    [else (fail "bytecode is not hex")]))

(define (jsexpr->contract-abi/who who j)
  (define (fail reason)
    (raise-abi-type-error who reason j #f))
  (define-values (abi bytecode)
    (match j
      [(? list?) (values j #f)]
      [(? hash?)
       (match (hash-ref j 'abi #f)
         [(? list? abi) (values abi (artifact-bytecode who j))]
         [_ (fail "an artifact's abi is a list of items")])]
      [_ (fail "expected a list of items, or an artifact with an abi")]))
  (define items
    (for/list ([x (in-list abi)]
               [i (in-naturals)])
      (in-item/who who i x (lambda () (jsexpr->abi-item/who who x)))))
  (match (contract-abi-failure items)
    [#f (raw-contract-abi items bytecode)]
    [(cons reason i) (raise-abi-type-error who (format "item ~a: ~a" i reason) (list-ref abi i) #f)]))

(define (jsexpr->contract-abi j)
  (jsexpr->contract-abi/who 'jsexpr->contract-abi j))

(define (contract-abi->jsexpr d)
  (map item->jsexpr (contract-abi-items d)))

;;;; lookups

(define (count-of n what)
  (format "~a ~a~a" n what (if (= n 1) "" "s")))

(define (or-list ns)
  (match ns
    [(list n) (format "~a" n)]
    [_ (format "~a or ~a"
               (string-join (map number->string (reverse (cdr (reverse ns)))) ", ")
               (car (reverse ns)))]))

(define (signature-of i)
  (abi-signature->string (if (abi-event? i) (abi-event->signature i) i)))

;; The item of items that key names, or the reason there is none.  A
;; string with ( is parsed by parse and matched by canonical signature.
(define (resolution what items key n parse hint)
  (define (by-signature signature)
    (or (findf (lambda (i) (equal? (signature-of i) signature)) items)
        (format "no ~a ~a in this ABI" what signature)))
  (define (by-name name)
    (define candidates (filter (lambda (i) (equal? (item-name i) name)) items))
    (define (signatures is)
      (string-join (map signature-of is) ", "))
    (define (inputs i)
      (length (abi-signature-inputs i)))
    (match* (candidates n)
      [('() _) (format "no ~a ~a in this ABI" what name)]
      [((list i) _) i]
      [(_ #f)
       (format "ambiguous: ~a has ~a overloads; ~a: ~a" name (length candidates) hint (signatures candidates))]
      [(_ n)
       (match (filter (lambda (i) (= (inputs i) n)) candidates)
         [(list i) i]
         ['()
          (format "no overload of ~a takes ~a; it takes ~a" name (count-of n "argument")
                  (or-list (sort (remove-duplicates (map inputs candidates)) <)))]
         [same
          (format "ambiguous: ~a has ~a overloads taking ~a: ~a; pass a signature"
                  name (length same) (count-of n "argument") (signatures same))])]))
  (match key
    [(? symbol?) (by-name (symbol->string key))]
    [(? string?) #:when (string-contains? key "(") (by-signature (signature-of (parse key)))]
    [(? string?) (by-name key)]
    [(or (? abi-signature?) (? abi-event?)) (by-signature (signature-of key))]
    [_ (format "expected a name, a signature or an item, given ~e" key)]))

(define (item-name i)
  (if (abi-event? i) (abi-event-name i) (abi-signature-name i)))

(define (function-resolution who d key n)
  (if (abi-event? key)
      (format "expected a function, given ~e" key)
      (resolution "function" (contract-abi-functions d) key n
                  (lambda (s) (string->signature who s)) "pass #:arity or a signature")))

(define (event-resolution who d key)
  (if (abi-signature? key)
      (format "expected an event, given ~e" key)
      (resolution "event" (contract-abi-events d) key #f
                  (lambda (s) (string->abi-event/who who s)) "pass a signature")))

(define (error-resolution who d key)
  (if (abi-event? key)
      (format "expected an error, given ~e" key)
      (resolution "error" (contract-abi-errors d) key #f
                  (lambda (s) (string->signature who s)) "pass a signature")))

(define (contract-abi-function d key #:arity [n #f])
  (function-resolution 'contract-abi-function d key n))

(define (contract-abi-event d key)
  (event-resolution 'contract-abi-event d key))

(define (contract-abi-error d key)
  (error-resolution 'contract-abi-error d key))

;; The descriptions' readers and lookups under their caller's who, for
;; web3/contract and define-contract.
(module* private #f
  (provide bytecode/c
           contract-abi-failure
           contract-abi/who
           error-resolution
           event-resolution
           function-resolution
           jsexpr->contract-abi/who
           raw-contract-abi
           resolution/c))
