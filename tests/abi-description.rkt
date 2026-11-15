#lang racket/base

;; ABI items and contract descriptions: the four forms, canonical strings,
;; rejections, lookups, printing and the module's dependencies.

(module+ test
  (require (for-syntax racket/base
                       syntax/parse
                       web3/abi/description
                       web3/abi/type)
           (only-in racket/contract exn:fail:contract:blame?)
           racket/list
           racket/match
           rackunit
           syntax/parse/define
           web3
           "abi-gen.rkt"
           "bounded.rkt"
           "modules.rkt"
           "util.rkt")

  (struct form-row (string sexp) #:prefab)
  (struct canonical-row (input canonical) #:prefab)
  (struct position-row (input position) #:prefab)


  (define-syntax-parse-rule (check-type-error expr)
    (check-pred exn:fail:web3:abi:type? (raised-type-error (lambda () expr)) (format "~s" 'expr)))

  (define form-rows
    '(#s(form-row "function balanceOf(address owner) view returns (uint256)"
                  (function balanceOf ([owner address]) #:view #:returns ([#f uint256])))
      #s(form-row "function transfer(address to, uint256 value) returns (bool)"
                  (function transfer ([to address] [value uint256]) #:returns ([#f bool])))
      #s(form-row "function mint(address to, uint256 value)"
                  (function mint ([to address] [value uint256])))
      #s(form-row "function deposit(string memo) payable"
                  (function deposit ([memo string]) #:payable))
      #s(form-row "function info() view returns (string, string, uint8, uint256)"
                  (function info () #:view #:returns ([#f string] [#f string] [#f uint8] [#f uint256])))
      #s(form-row "function get((uint256,string) p) pure returns ((uint256,string)[] all)"
                  (function get ([p (tuple uint256 string)]) #:pure
                            #:returns ([all (array (tuple uint256 string))])))
      #s(form-row "error InsufficientBalance(uint256 have, uint256 want)"
                  (error InsufficientBalance ([have uint256] [want uint256])))
      #s(form-row "error Unauthorized()" (error Unauthorized ()))
      #s(form-row "constructor(string name_, string symbol_, uint256 supply)"
                  (constructor ([name_ string] [symbol_ string] [supply uint256])))
      #s(form-row "constructor() payable" (constructor () #:payable))
      #s(form-row "fallback()" (fallback))
      #s(form-row "fallback() payable" (fallback #:payable))
      #s(form-row "receive() payable" (receive))
      #s(form-row "event Transfer(address indexed from, address indexed to, uint256 value)"
                  (event Transfer ([from address #:indexed] [to address #:indexed] [value uint256])))))

  (test-case "the string, s-expression, struct and JSON forms of an item agree"
    (for ([row (in-list form-rows)])
      (match-define (struct* form-row ([string s] [sexp x])) row)
      (define item (string->abi-item s))
      (check-equal? (sexp->abi-item x) item s)
      (check-equal? (abi-item s) item s)
      (check-equal? (abi-item x) item s)
      (check-equal? (abi-item item) item s)
      (check-equal? (abi-item->string x) s s)
      (check-equal? (abi-item->sexp s) x s)
      (check-equal? (jsexpr->abi-item (abi-item->jsexpr s)) item s)))

  (test-case "the struct forms are the items the strings denote"
    (check-equal? (abi-function "balanceOf" (list (abi-parameter "owner" 'address)) '(uint256) 'view)
                  (string->abi-item "function balanceOf(address owner) view returns (uint256)"))
    (check-equal? (abi-function "mint" (list (abi-parameter "to" 'address) (abi-parameter "value" "uint256"))
                                '())
                  (string->abi-item "function mint(address to, uint256 value)"))
    (check-equal? (abi-error "Unauthorized" '()) (string->abi-item "error Unauthorized()"))
    (check-equal? (abi-constructor '() 'payable) (string->abi-item "constructor() payable"))
    (check-equal? (abi-fallback) (string->abi-item "fallback()"))
    (check-equal? (abi-fallback 'payable) (string->abi-item "fallback() payable"))
    (check-equal? (abi-receive) (string->abi-item "receive() payable"))
    (check-equal? (abi-parameter-type (abi-parameter #f "uint")) (abi-uint 256))
    (check-equal? (abi-function-state-mutability (abi-function "f" '() '())) 'nonpayable)
    (check-equal? (abi-constructor-state-mutability (abi-constructor '())) 'nonpayable))

  (test-case "abi-item->jsexpr writes exactly the Solidity JSON keys"
    (check-equal? (abi-item->jsexpr "function balanceOf(address owner) view returns (uint256)")
                  (hasheq 'type "function" 'name "balanceOf"
                          'inputs (list (hasheq 'name "owner" 'type "address"))
                          'outputs (list (hasheq 'name "" 'type "uint256"))
                          'stateMutability "view"))
    (check-equal? (abi-item->jsexpr "function get((uint256,string) p) pure returns ((uint256,string)[] all)")
                  (hasheq 'type "function" 'name "get"
                          'inputs (list (hasheq 'name "p" 'type "tuple"
                                                'components (list (hasheq 'name "" 'type "uint256")
                                                                  (hasheq 'name "" 'type "string"))))
                          'outputs (list (hasheq 'name "all" 'type "tuple[]"
                                                 'components (list (hasheq 'name "" 'type "uint256")
                                                                   (hasheq 'name "" 'type "string"))))
                          'stateMutability "pure"))
    (check-equal? (abi-item->jsexpr "constructor(uint256 supply) payable")
                  (hasheq 'type "constructor" 'inputs (list (hasheq 'name "supply" 'type "uint256"))
                          'stateMutability "payable"))
    (check-equal? (abi-item->jsexpr "fallback()") (hasheq 'type "fallback" 'stateMutability "nonpayable"))
    (check-equal? (abi-item->jsexpr "receive() payable") (hasheq 'type "receive" 'stateMutability "payable"))
    (check-equal? (abi-item->jsexpr "event Note(address indexed sender, uint256) anonymous")
                  (hasheq 'type "event" 'name "Note" 'anonymous #t
                          'inputs (list (hasheq 'name "sender" 'type "address" 'indexed #t)
                                        (hasheq 'name "" 'type "uint256" 'indexed #f))))
    (check-equal? (hash-ref (first (hash-ref (abi-item->jsexpr "function g((uint256,bool)[2][] x)") 'inputs))
                            'type)
                  "tuple[2][]"))

  (test-case "strings with modifiers, locations and spacing read as their canonical form"
    (for ([row (in-list '(#s(canonical-row
                             "function balanceOf(address owner) external view returns (uint256)"
                             "function balanceOf(address owner) view returns (uint256)")
                          #s(canonical-row
                             "  function  f(uint a , string memory s)  public pure returns(uint)"
                             "function f(uint256 a, string s) pure returns (uint256)")
                          #s(canonical-row "function g(tuple(uint256 id, string label)[] calldata ps)"
                                           "function g((uint256,string)[] ps)")
                          #s(canonical-row "function h() payable external" "function h() payable")
                          #s(canonical-row "constructor(uint) public payable" "constructor(uint256) payable")
                          #s(canonical-row "fallback() external payable" "fallback() payable")
                          #s(canonical-row "receive() external payable" "receive() payable")
                          #s(canonical-row "event Note(address indexed sender, uint value) anonymous"
                                           "event Note(address indexed sender, uint256 value) anonymous")))])
      (match-define (struct* canonical-row ([input input] [canonical canonical])) row)
      (check-equal? (abi-item->string input) canonical input)))

  (test-case "a string that is not an item raises at the offending token"
    (for ([row (in-list '(#s(position-row "" 0)
                          #s(position-row "transfer(address,uint256)" 0)
                          #s(position-row "Function f()" 0)
                          #s(position-row "function" 8)
                          #s(position-row "function 1f()" 9)
                          #s(position-row "function f" 10)
                          #s(position-row "function f(uint7 a)" 11)
                          #s(position-row "function f(uint256 a, uint256 a)" 22)
                          #s(position-row "function f(uint256 a) returns (uint256 a)" 31)
                          #s(position-row "function f() veiw" 13)
                          #s(position-row "function f() internal" 13)
                          #s(position-row "function f() view view" 18)
                          #s(position-row "function f() view pure" 18)
                          #s(position-row "function f() returns" 20)
                          #s(position-row "function f() returns ()" 21)
                          #s(position-row "function f() view returns (uint256);" 35)
                          #s(position-row "function f() view returns (uint256) external" 36)
                          #s(position-row "function f(uint256\ta)" 18)
                          #s(position-row "function f(uint256 calldata calldata a)" 28)
                          #s(position-row "error E(uint256 a) view" 19)
                          #s(position-row "constructor(uint256) view" 21)
                          #s(position-row "receive()" 9)
                          #s(position-row "receive(uint256) payable" 8)
                          #s(position-row "fallback() view" 11)
                          #s(position-row "event Transfer" 14)))])
      (match-define (struct* position-row ([input input] [position position])) row)
      (define e (raised-type-error (lambda () (string->abi-item input))))
      (check-pred exn:fail:web3:abi:type? e input)
      (when e
        (check-equal? (exn:fail:web3:abi:type-input e) input input)
        (check-equal? (exn:fail:web3:abi:type-position e) position input)
        (check-regexp-match #rx"^string->abi-item: " (exn-message e)))))

  (test-case "a string's errors say what was found where something else was expected"
    (for ([row (in-list '(#s(canonical-row "function f(uint256" "expected , or \\), found end of input")
                          #s(canonical-row "function f(uint256 a;" "expected , or \\), found ;")
                          #s(canonical-row "function f uint256)" "expected [(], found u")
                          #s(canonical-row "fallback(]" "expected [)], found ]")
                          #s(canonical-row "function 1f()" "expected a function name, found 1")
                          #s(canonical-row "constructor() view"
                                           "unknown modifier view; expected public or payable")))])
      (match-define (struct* canonical-row ([input input] [canonical message])) row)
      (check-regexp-match (string-append "^string->abi-item: " message)
                          (exn-message (raised-type-error (lambda () (string->abi-item input))))
                          input)))

  (define (nested-tuple depth)
    (for/fold ([p (hasheq 'name "x" 'type "uint256")])
              ([_ (in-range depth)])
      (hasheq 'name "" 'type "tuple[2][]" 'components (list p))))

  (test-case "deeply nested JSON tuples are read, printed and written in linear time"
    (define j (list (hasheq 'type "function" 'name "f" 'inputs (list (nested-tuple 2000)) 'outputs '())))
    (match (call-bounded (lambda ()
                           (define d (jsexpr->contract-abi j))
                           (define f (contract-abi-function d 'f))
                           (list (string-length (abi-item->string f))
                                 (equal? (jsexpr->contract-abi (contract-abi->jsexpr d)) d)
                                 (equal? (sexp->abi-item (abi-item->sexp f)) f)
                                 (string-length (format "~v" f))))
                         #:seconds 2
                         #:megabytes 256)
      [(list 'ok (list length round-trip? sexp-round-trip? printed))
       (check-equal? length (+ (string-length "function f(uint256)") (* 2000 (string-length "()[2][]"))))
       (check-true round-trip?)
       (check-true sexp-round-trip?)
       (check-true (> printed 2000))]
      [other (fail (format "~s" other))]))

  (test-case "an s-expression that is not an item raises, naming the subform"
    (for ([x (in-list '((function) (function 1f ()) (function f) (function "f" ()) (function f (uint256))
                        (function f ([a uint7])) (function f () #:view #:pure) (function f () #:returns)
                        (function f () #:returns () #:returns ()) (function f () #:constant)
                        (function f ([a uint256] [a bool])) (error E () #:view)
                        (constructor ([a uint256]) #:view) (fallback #:view) (receive #:payable)
                        (funktion f ())))])
      (define e (raised-type-error (lambda () (sexp->abi-item x))))
      (check-pred exn:fail:web3:abi:type? e (format "~s" x))
      (when e
        (check-false (exn:fail:web3:abi:type-position e) (format "~s" x))
        (check-regexp-match #rx"^sexp->abi-item: " (exn-message e))))
    (check-equal? (sexp->abi-item '(function f () #:returns ())) (sexp->abi-item '(function f ())))
    (check-regexp-match #rx"form: uint7" (exn-message (raised-type-error (lambda () (sexp->abi-item
                                                                               '(function f ([a uint7]))))))))

  (test-case "JSON items: legacy mutability, defaults, tuples, and ignored keys"
    (define (fn . kvs)
      (jsexpr->abi-item (apply hasheq 'type "function" 'name "f" 'inputs '() kvs)))
    (check-equal? (abi-function-state-mutability (fn 'stateMutability "pure" 'constant #t 'payable #t)) 'pure)
    (check-equal? (abi-function-state-mutability (fn 'constant #t 'payable #t)) 'view)
    (check-equal? (abi-function-state-mutability (fn 'constant #f 'payable #t)) 'payable)
    (check-equal? (abi-function-state-mutability (fn 'payable #f)) 'nonpayable)
    (check-equal? (jsexpr->abi-item (hasheq 'name "f" 'inputs '())) (abi-function "f" '() '()))
    (check-equal? (jsexpr->abi-item (hasheq 'type "function" 'name "f"
                                            'inputs (list (hasheq 'name "" 'type "uint256")
                                                          (hasheq 'type "bool"))
                                            'internalType "x" 'gas 5 'id 1))
                  (abi-function "f" '(uint256 bool) '()))
    (define tupled
      (jsexpr->abi-item
       (hasheq 'name "g"
               'inputs (list (hasheq 'name "a" 'type "tuple[2][]" 'internalType "struct S[2][]"
                                     'components (list (hasheq 'name "id" 'type "uint256")
                                                       (hasheq 'name "inner" 'type "tuple[]"
                                                               'components (list (hasheq 'name "s"
                                                                                         'type "string")))))
                             (hasheq 'name "b" 'type "tuple"
                                     'components (list (hasheq 'name "" 'type "bool")))))))
    (check-equal? (abi-item->string tupled) "function g((uint256,(string)[])[2][] a, (bool) b)")
    (define rejected
      (list (hasheq 'type "struct" 'name "f" 'inputs '())
            (hasheq 'type "function" 'inputs '())
            (hasheq 'name "1f" 'inputs '())
            (hasheq 'name "f" 'inputs 5)
            (hasheq 'name "f" 'inputs (list (hasheq 'name "a" 'type "uint7")))
            (hasheq 'name "f" 'inputs (list (hasheq 'name "a" 'type "tuple")))
            (hasheq 'name "f" 'inputs (list (hasheq 'name "a" 'type "uint256"
                                                    'components (list (hasheq 'type "bool")))))
            (hasheq 'name "f" 'inputs (list (hasheq 'name "a" 'type "tuple" 'components '())))
            (hasheq 'name "f" 'inputs (list (hasheq 'name "a" 'type "tuple[0]"
                                                    'components (list (hasheq 'type "bool")))))
            (hasheq 'name "f" 'inputs (list (hasheq 'name "a" 'type "tuple[01]"
                                                    'components (list (hasheq 'type "bool")))))
            (hasheq 'name "f" 'inputs '() 'stateMutability "constant")
            (hasheq 'type "receive" 'stateMutability "nonpayable")
            (hasheq 'type "constructor" 'inputs '() 'stateMutability "view")
            (hasheq 'type "event" 'name "E" 'inputs (list (hasheq 'name "a" 'type "bool" 'indexed "yes")))
            42
            'null
            (hasheq 'name 5 'inputs '())))
    (for ([j (in-list rejected)])
      (define e (raised-type-error (lambda () (jsexpr->abi-item j))))
      (check-pred exn:fail:web3:abi:type? e (format "~s" j))
      (when e
        (check-equal? (exn:fail:web3:abi:type-input e) j (format "~s" j))
        (check-false (exn:fail:web3:abi:type-position e)))))

  (test-case "generated items survive every form, and canonical forms are fixed points"
    (define prng (seeded 6 1 4 2 2 1))
    (define items
      (append (for/list ([_ (in-range 300)]) (random-abi-item prng))
              (append* (for/list ([_ (in-range 100)]) (contract-abi-items (random-contract-abi prng))))))
    (for ([i (in-list items)])
      (define s (abi-item->string i))
      (check-equal? (string->abi-item s) i s)
      (check-equal? (sexp->abi-item (abi-item->sexp i)) i s)
      (check-equal? (jsexpr->abi-item (abi-item->jsexpr i)) i s)
      (check-equal? (abi-item->string s) s s)
      (check-equal? (abi-item->sexp (abi-item->sexp i)) (abi-item->sexp i) s)
      (check-equal? (abi-item->jsexpr (jsexpr->abi-item (abi-item->jsexpr i))) (abi-item->jsexpr i) s)))

  (test-case "a description refuses two items with one signature, and two constructors"
    (for ([items (in-list (list '("function f(uint256)" "function f(uint)")
                                '("function f()" "function f()")
                                '("function f(uint256)" (function f ([#f uint256]) #:view))
                                '("error E(uint256 a)" "error E(uint256 b)")
                                '("event E(uint256 indexed a)" "event E(uint256 indexed b)")
                                '("event E(uint256 a)" "event E(uint256 a) anonymous"
                                  "event E(uint256 b) anonymous")
                                '("constructor()" "constructor(uint256)")
                                '("fallback()" "fallback() payable")
                                '("receive() payable" "receive() external payable")))])
      (define e (raised-type-error (lambda () (contract-abi items))))
      (check-pred exn:fail:web3:abi:type? e (format "~s" items))
      (when e
        (check-equal? (exn:fail:web3:abi:type-input e) (last items))
        (check-false (exn:fail:web3:abi:type-position e))))
    (check-regexp-match #rx"duplicate function f[(]uint256[)]"
                        (exn-message (raised-type-error (lambda () (contract-abi '("function f(uint256)"
                                                                            "function f(uint)"))))))
    (check-pred contract-abi? (contract-abi '("function f(uint256)" "function f(address)")))
    (check-pred contract-abi?
                (contract-abi '("event Transfer(address indexed from, address indexed to, uint256 value)"
                                "event Transfer(address indexed from, address indexed to, uint256 indexed id, bytes data)")))
    (check-equal? (contract-abi-items (contract-abi '("event E(uint256 indexed a)" "event E(uint256 b)")))
                  (list (string->abi-event "E(uint256 indexed a)") (string->abi-event "E(uint256 b)")))
    (check-false (contract-abi-bytecode (contract-abi '() #:bytecode #"")))
    (check-false (contract-abi-bytecode (contract-abi '() #:bytecode "0x")))
    (check-equal? (contract-abi-bytecode (contract-abi '() #:bytecode "0x60aB")) #"\x60\xab"))

  (define overloaded
    (contract-abi '("function safeTransferFrom(address from, address to, uint256 id)"
                    "function safeTransferFrom(address from, address to, uint256 id, bytes data)"
                    "function f(uint256)"
                    "function f(address)"
                    "function g()"
                    "event Transfer(address indexed from, address indexed to, uint256 value)"
                    "event Transfer(address indexed from, address indexed to, uint256 value, bytes data)")))

  (define (lookup-message thunk)
    (with-handlers ([exn:fail:web3:abi? (lambda (e) (and (not (exn:fail:web3:abi:type? e))
                                                         (exn-message e)))])
      (thunk)
      #f))

  (test-case "names resolve by arity, signatures by their inputs, and failed lookups raise"
    (define (signature f)
      (abi-signature->string f))
    (check-equal? (signature (contract-abi-function overloaded 'safeTransferFrom #:arity 3))
                  "safeTransferFrom(address,address,uint256)")
    (check-equal? (signature (contract-abi-function overloaded 'safeTransferFrom #:arity 4))
                  "safeTransferFrom(address,address,uint256,bytes)")
    (check-equal? (lookup-message (lambda () (contract-abi-function overloaded 'safeTransferFrom)))
                  (string-append "contract-abi-function: ambiguous: safeTransferFrom has 2 overloads;"
                                 " pass #:arity or a signature: "
                                 "safeTransferFrom(address,address,uint256), "
                                 "safeTransferFrom(address,address,uint256,bytes)"))
    (check-equal? (lookup-message (lambda () (contract-abi-function overloaded 'safeTransferFrom
                                                                    #:arity 2)))
                  (string-append "contract-abi-function: no overload of safeTransferFrom takes"
                                 " 2 arguments; it takes 3 or 4"))
    (for ([key (list "safeTransferFrom(address,address,uint256)"
                     "safeTransferFrom(address, address, uint)"
                     "safeTransferFrom(address,address,uint256)(bool)")])
      (check-equal? (signature (contract-abi-function overloaded key))
                    "safeTransferFrom(address,address,uint256)" key))
    (check-equal? (lookup-message (lambda () (contract-abi-function overloaded 'f #:arity 1)))
                  (string-append "contract-abi-function: ambiguous: f has 2 overloads taking 1 argument:"
                                 " f(uint256), f(address); pass a signature"))
    (check-equal? (signature (contract-abi-function overloaded "f(address)")) "f(address)")
    (for ([thunk (list (lambda () (contract-abi-function overloaded 'g))
                       (lambda () (contract-abi-function overloaded "g"))
                       (lambda () (contract-abi-function overloaded 'g #:arity 5)))])
      (check-equal? (signature (thunk)) "g()"))
    (check-equal? (lookup-message (lambda () (contract-abi-function overloaded 'nope)))
                  "contract-abi-function: no function nope in this ABI")
    (check-equal? (lookup-message (lambda () (contract-abi-function overloaded "nope(uint256)")))
                  "contract-abi-function: no function nope(uint256) in this ABI")
    (check-type-error (contract-abi-function overloaded "f("))
    (check-regexp-match #rx"^contract-abi-event: ambiguous: Transfer has 2 overloads; pass a signature"
                        (lookup-message (lambda () (contract-abi-event overloaded 'Transfer))))
    (check-equal? (contract-abi-event overloaded
                                      "Transfer(address indexed from, address indexed to, uint256 value)")
                  (string->abi-event "Transfer(address indexed from, address indexed to, uint256 value)"))
    (define token-errors (contract-abi '("error InsufficientBalance(uint256 have, uint256 want)")))
    (check-equal? (contract-abi-error token-errors 'InsufficientBalance)
                  (abi-error "InsufficientBalance" (list (abi-parameter "have" 'uint256)
                                                         (abi-parameter "want" 'uint256))))
    (check-equal? (contract-abi-error token-errors "InsufficientBalance(uint256,uint256)")
                  (contract-abi-error token-errors "InsufficientBalance"))
    (check-equal? (lookup-message (lambda () (contract-abi-error token-errors 'Nope)))
                  "contract-abi-error: no error Nope in this ABI")
    (check-blames-here (contract-abi-event overloaded 5))
    (check-blames-here (contract-abi-function overloaded (string->abi-event "E()"))))

  (define erc20+721
    (contract-abi
     '("event Transfer(address indexed from, address indexed to, uint256 value)"
       "event Transfer(address indexed from, address indexed to, uint256 indexed id)")))

  (test-case "events with one signature and different indexed parameters are both kept"
    (define-values (erc20 erc721) (apply values (contract-abi-events erc20+721)))
    (check-equal? (abi-event->string erc721)
                  "event Transfer(address indexed from, address indexed to, uint256 indexed id)")
    (define both
      (string-append "event Transfer(address indexed from, address indexed to, uint256 value), "
                     "event Transfer(address indexed from, address indexed to, uint256 indexed id)"))
    (check-equal? (lookup-message (lambda () (contract-abi-event erc20+721 'Transfer)))
                  (string-append "contract-abi-event: ambiguous: Transfer has 2 overloads;"
                                 " pass a signature: " both))
    (check-equal? (lookup-message (lambda () (contract-abi-event erc20+721
                                                                 "Transfer(address,address,uint256)")))
                  (string-append "contract-abi-event: ambiguous: 2 events share the signature"
                                 " Transfer(address,address,uint256): " both))
    (check-equal? (contract-abi-event erc20+721 (abi-event->string erc721)) erc721)
    (check-equal? (contract-abi-event erc20+721 erc20) erc20))

  (test-case "functions, errors and constructors are signatures"
    (define prng (seeded 6 6 1 2 9 9))
    (define items
      (append (contract-abi-items (contract-abi (list "function balanceOf(address owner) view returns (uint256)"
                                                      "error InsufficientBalance(uint256 have, uint256 want)"
                                                      "constructor(string name_)")))
              (append* (for/list ([_ (in-range 50)]) (contract-abi-items (random-contract-abi prng))))))
    (for ([f (in-list items)]
          #:when (or (abi-function? f) (abi-error? f)))
      (define name (abi-signature->string f))
      (check-pred abi-signature? f name)
      (define params (if (abi-function? f) (abi-function-parameters f) (abi-error-parameters f)))
      (check-equal? (abi-signature-inputs f) (map abi-parameter-type params) name)
      (check-equal? (abi-signature-outputs f)
                    (and (abi-function? f) (map abi-parameter-type (abi-function-returns f)))
                    name)
      (check-equal? (function-selector f) (function-selector name) name)
      (define vs (for/list ([t (in-list (abi-signature-inputs f))]) (random-abi-value prng t)))
      (check-equal? (apply encode-call f vs) (apply encode-call name vs) name))
    (check-equal? (abi-signature-name (abi-constructor '())) "constructor")
    (check-false (abi-signature-outputs (abi-constructor '(uint256))))
    (check-false (equal? (abi-function "f" '() '()) (string->abi-signature "f()()"))))

  (test-case "items and descriptions print as written"
    (check-equal? (format "~v" (string->abi-item "function balanceOf(address owner) view returns (uint256)"))
                  (string-append "(abi-function \"balanceOf\" (list (abi-parameter \"owner\" (abi-address)))"
                                 " (list (abi-parameter #f (abi-uint 256))) 'view)"))
    (check-equal? (format "~a" (string->abi-item "function balanceOf(address owner) view returns (uint256)"))
                  (format "~v" (string->abi-item "function balanceOf(address owner) view returns (uint256)")))
    (check-equal? (format "~v" (abi-receive)) "(abi-receive)")
    (check-equal? (format "~v" (abi-fallback 'payable)) "(abi-fallback 'payable)")
    (check-equal? (format "~v" (contract-abi '())) "#<contract-abi>")
    (check-equal? (format "~v" (contract-abi '("function f(uint256)" "function f(address)" "receive() payable"
                                               "fallback()")))
                  "#<contract-abi functions: f; fallback; receive>")
    (check-equal? (format "~a" (contract-abi '("function f()") #:bytecode #"\x60\x80"))
                  "#<contract-abi functions: f; bytecode: 2 bytes>")
    (check-equal? (format "~a" (contract-abi '() #:bytecode #"\x60")) "#<contract-abi bytecode: 1 byte>"))

  (test-case "malformed arguments are contract violations blaming the caller"
    (check-blames-here (abi-parameter "indexed" 'uint256))
    (check-blames-here (abi-parameter 'a 'uint256))
    (check-blames-here (abi-function "1f" '() '()))
    (check-blames-here (abi-function "f" '() '() 'constant))
    (check-blames-here (abi-constructor '() 'view))
    (check-blames-here (abi-fallback 'view))
    (check-blames-here (contract-abi "function f()"))
    (check-blames-here (contract-abi '() #:bytecode "602a"))
    (check-blames-here (contract-abi-function 'x 'f))
    (check-blames-here (string->abi-item 'f))
    (check-blames-here (sexp->abi-item "f"))
    (check-type-error (abi-function "f" '("uint7") '()))
    (check-type-error (abi-function "f" (list (abi-parameter "a" 'bool)) (list (abi-parameter "a" 'bool))))
    (check-type-error (abi-error "E" (list (abi-parameter "a" 'bool) (abi-parameter "a" 'bool))))
    (check-type-error (abi-function "f" '((tuple)) '())))

  (test-case "error selectors that collide are refused before any request"
    (define colliding (contract-abi '("error burn(uint256)" "error collate_propagate_storage(bytes16)")))
    (define node (mock-provider (hash "eth_call" "0x")))
    (check-type-error (contract-call (contract-instance colliding zero-address) (string->abi-signature "f()()")
                                     #:provider node))
    (check-equal? (mock-provider-calls node) '()))

  (define-syntax (item-signature stx)
    (syntax-parse stx
      [(_ s:str)
       (datum->syntax stx (abi-signature->string (string->abi-item (syntax-e #'s)) #:outputs? #t))]))

  (test-case "items can be read when a macro expands"
    (check-equal? (item-signature "function balanceOf(address owner) external view returns (uint256)")
                  "balanceOf(address)(uint256)"))

  (test-case "web3/abi/description needs no JSON, FFI, network or keccak module"
    (define names (filter values (map module-name (import-closure 'web3/abi/description))))
    (check-equal? (filter (lambda (n) (member (collection n) '("ffi" "net" "json" "file" "openssl" "keccak")))
                          names)
                  '())
    (define web3-modules
      '("web3/abi/description" "web3/abi/event-type" "web3/abi/type" "web3/private/abi-item"
        "web3/private/binding-rules" "web3/private/exn" "web3/private/who"))
    (check-equal? (sort (remove-duplicates (filter (lambda (n) (equal? (collection n) "web3")) names))
                        string<?)
                  web3-modules)
    (define allowed
      (append '("racket/base" "racket/contract/base" "racket/contract/combinator" "racket/list"
                "racket/match" "racket/string" "racket/struct")
              web3-modules))
    (for ([name (in-list (filter values (map module-name (direct-imports
                                                          (module-path-index-resolve
                                                           (module-path-index-join
                                                            'web3/abi/description #f))))))])
      (check-not-false (member name allowed) name)))

  (test-case "the manual's examples give what it shows"
    (define balance-of (abi-function "balanceOf" (list (abi-parameter "owner" 'address)) '(uint256) 'view))
    (check-equal? (abi-signature->string balance-of #:outputs? #t) "balanceOf(address)(uint256)")
    (check-equal? (bytes->hex-string (function-selector balance-of)) "0x70a08231")
    (define veiw (raised-type-error (lambda () (string->abi-item "function balanceOf(address owner) veiw returns (uint256)"))))
    (check-regexp-match (string-append "^string->abi-item: unknown modifier veiw; expected external,"
                                       " public, view, pure, payable or returns")
                        (exn-message veiw))
    (check-regexp-match #rx"at: position 34" (exn-message veiw))
    (check-equal? (abi-item->string '(function transfer ([to address] [amount uint256]) #:returns ([#f bool])))
                  "function transfer(address to, uint256 amount) returns (bool)")
    (check-equal? (abi-item->sexp "constructor(string name_, uint256 supply) payable")
                  '(constructor ([name_ string] [supply uint256]) #:payable))
    (check-equal? (abi-item->jsexpr "error InsufficientBalance(uint256 have, uint256 want)")
                  #hasheq((type . "error") (name . "InsufficientBalance")
                          (inputs . (#hasheq((name . "have") (type . "uint256"))
                                     #hasheq((name . "want") (type . "uint256"))))))
    (define erc20
      (contract-abi (list "function balanceOf(address owner) view returns (uint256)"
                          "function transfer(address to, uint256 amount) returns (bool)"
                          "event Transfer(address indexed from, address indexed to, uint256 value)")))
    (check-equal? (format "~v" erc20) "#<contract-abi functions: balanceOf, transfer; events: Transfer>")
    (check-equal? (bytes->hex-string (encode-call (contract-abi-function erc20 'transfer)
                                                  "0x70997970C51812dc3A010C7d01b50e0d17dc79C8" 250))
                  (string-append "0xa9059cbb00000000000000000000000070997970c51812dc3a010c7d01b50e0d17dc79c8"
                                 "00000000000000000000000000000000000000000000000000000000000000fa"))
    (define nft (contract-abi (list "function safeTransferFrom(address from, address to, uint256 id)"
                                    "function safeTransferFrom(address from, address to, uint256 id, bytes data)")))
    (check-equal? (abi-item->string (contract-abi-function nft 'safeTransferFrom #:arity 4))
                  "function safeTransferFrom(address from, address to, uint256 id, bytes data)")
    (check-equal? (abi-item->string (contract-abi-function nft "safeTransferFrom(address,address,uint256)"))
                  "function safeTransferFrom(address from, address to, uint256 id)")
    (check-equal? (contract-abi->jsexpr (contract-abi '((function count () #:view #:returns ([#f uint256])))))
                  (list #hasheq((type . "function") (name . "count") (inputs . ())
                                (outputs . (#hasheq((name . "") (type . "uint256"))))
                                (stateMutability . "view"))))))
