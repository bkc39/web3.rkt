#lang racket/base

;; Describing events: the string, s-expression and struct forms.

(module+ test
  (require racket/list
           racket/match
           rackunit
           syntax/parse/define
           web3/abi/event-type
           web3/abi/type
           "abi-gen.rkt"
           "modules.rkt"
           "util.rkt"
           (for-syntax racket/base
                       syntax/parse
                       web3/abi/event-type
                       web3/abi/type))

  (struct forms (string sexp structure canonical) #:prefab)
  (struct rejection (input position) #:prefab)

  (define-namespace-anchor anchor)
  (define ns (namespace-anchor->namespace anchor))

  (define three-forms
    '(#s(forms "Transfer(address indexed from, address indexed to, uint256 value)"
               (event Transfer ([from address #:indexed] [to address #:indexed] [value uint256]))
               (abi-event "Transfer" (list (abi-event-parameter "from" 'address #t)
                                           (abi-event-parameter "to" 'address #t)
                                           (abi-event-parameter "value" 'uint256 #f))
                          #f)
               "event Transfer(address indexed from, address indexed to, uint256 value)")
      #s(forms "event Deposit(address indexed from, uint256 value, string memo)"
               (event Deposit ([from address #:indexed] [value uint256] [memo string]))
               (abi-event "Deposit" (list (abi-event-parameter "from" 'address #t)
                                          (abi-event-parameter "value" 'uint256 #f)
                                          (abi-event-parameter "memo" 'string #f))
                          #f)
               "event Deposit(address indexed from, uint256 value, string memo)")
      #s(forms "Note(address indexed sender, uint value) anonymous"
               (event Note ([sender address #:indexed] [value uint256]) #:anonymous)
               (abi-event "Note" (list (abi-event-parameter "sender" 'address #t)
                                       (abi-event-parameter "value" 'uint256 #f))
                          #t)
               "event Note(address indexed sender, uint256 value) anonymous")
      #s(forms "Bare(uint256, string)"
               (event Bare ([#f uint256] [#f string]))
               (abi-event "Bare" (list (abi-event-parameter #f 'uint256 #f)
                                       (abi-event-parameter #f 'string #f))
                          #f)
               "event Bare(uint256, string)")
      #s(forms "Paired((uint256,string) indexed pair, tuple(uint256 id, string label) plain)"
               (event Paired ([pair (tuple uint256 string) #:indexed] [plain (tuple uint256 string)]))
               (abi-event "Paired" (list (abi-event-parameter "pair" "(uint256,string)" #t)
                                         (abi-event-parameter "plain" "(uint256,string)" #f))
                          #f)
               "event Paired((uint256,string) indexed pair, (uint256,string) plain)")
      #s(forms "Nested(uint256[][] indexed grid, (uint256,string)[] indexed pairs)"
               (event Nested ([grid (array (array uint256)) #:indexed]
                              [pairs (array (tuple uint256 string)) #:indexed]))
               (abi-event "Nested" (list (abi-event-parameter "grid" "uint256[][]" #t)
                                         (abi-event-parameter "pairs" "(uint256,string)[]" #t))
                          #f)
               "event Nested(uint256[][] indexed grid, (uint256,string)[] indexed pairs)")
      #s(forms "Paid(address payableTo, (address payableBy, uint256 x) indexed y)"
               (event Paid ([payableTo address] [y (tuple address uint256) #:indexed]))
               (abi-event "Paid" (list (abi-event-parameter "payableTo" 'address #f)
                                       (abi-event-parameter "y" "(address,uint256)" #t))
                          #f)
               "event Paid(address payableTo, (address,uint256) indexed y)")
      #s(forms "Nothing()" (event Nothing ()) (abi-event "Nothing" '() #f) "event Nothing()")))

  (define (generated-events n)
    (define prng (seeded 6 5 4 3 2 1))
    (for/list ([_ (in-range n)]) (random-abi-event prng)))

  (test-case "the string, s-expression and struct forms agree and have canonical forms"
    (for ([row (in-list three-forms)])
      (match-define (struct* forms ([string s] [sexp x] [structure expr] [canonical c])) row)
      (define e (eval expr ns))
      (check-equal? (string->abi-event s) e s)
      (check-equal? (sexp->abi-event x) e s)
      (check-equal? (abi-event->string s) c s)
      (check-equal? (abi-event->string x) c s)
      (check-equal? (abi-event->string e) c s)
      (check-equal? (abi-event->sexp s) x s)
      (check-equal? (abi-event->sexp e) x s)))

  (test-case "the canonical forms read back and are fixed points"
    (for ([e (in-list (generated-events 300))])
      (define s (abi-event->string e))
      (define x (abi-event->sexp e))
      (check-equal? (string->abi-event s) e s)
      (check-equal? (sexp->abi-event x) e s)
      (check-equal? (abi-event->string s) s)
      (check-equal? (abi-event->sexp x) x s)))

  (define string-rejections
    '(#s(rejection "" 0)
      #s(rejection "Transfer" 8)
      #s(rejection "event" 5)
      #s(rejection "event " 6)
      #s(rejection "1E()" 0)
      #s(rejection "E(uint7 a)" 2)
      #s(rejection "E(address payable a)" 10)
      #s(rejection "E((address payable, uint256) a)" 11)
      #s(rejection "E(address indexed indexed)" 18)
      #s(rejection "E(uint256 uint8)" 10)
      #s(rejection "E(address from to)" 15)
      #s(rejection "E(uint256 a, uint256 a)" 13)
      #s(rejection "E(uint256 a,)" 12)
      #s(rejection "E(uint256 [] a)" 10)
      #s(rejection "E(uint256 a);" 12)
      #s(rejection "E(uint256 a) anonymous x" 23)
      #s(rejection "E(uint256 a) anon" 13)
      #s(rejection "E(uint8 indexed a, uint8 indexed b, uint8 indexed c, uint8 indexed d)" 53)
      #s(rejection "E(uint8 indexed a,uint8 indexed b,uint8 indexed c,uint8 indexed d,uint8 indexed e) anonymous"
                   66)
      #s(rejection "E(uint256\ta)" 9)
      #s(rejection "E((uint256 a b) x)" 13)))

  (define (type-error thunk)
    (with-handlers ([exn:fail:web3:abi:type? values])
      (thunk)
      #f))

  (test-case "malformed event strings raise at the first character of the offending token"
    (for ([row (in-list string-rejections)])
      (match-define (struct* rejection ([input s] [position position])) row)
      (define e (type-error (lambda () (string->abi-event s))))
      (check-pred exn:fail:web3:abi:type? e s)
      (when e
        (check-equal? (exn:fail:web3:abi:type-input e) s s)
        (check-equal? (exn:fail:web3:abi:type-position e) position s)
        (check-regexp-match #rx"^string->abi-event: " (exn-message e) s)))
    (check-true (abi-event-anonymous?
                 (string->abi-event
                  "E(uint8 indexed a, uint8 indexed b, uint8 indexed c, uint8 indexed d) anonymous")))
    (check-exn #rx"too many indexed parameters: 4 [(]at most 3[)]"
               (lambda () (string->abi-event "E(uint8 indexed a, uint8 indexed b, uint8 indexed c, uint8 indexed d)")))
    (check-exn #rx"duplicate parameter name: a"
               (lambda () (string->abi-event "E(uint256 a, uint256 a)"))))

  (define sexp-rejections
    '((event) (event 1E ()) (event E) (event E ([a])) (event E ([a uint7]))
      (event E ([indexed uint256])) (event E ([a uint256 #:indexd])) (event E ([a uint256] [a bool]))
      (event E () #:anon) (evnt E ()) (event "E" ()) (event E ([a "uint256"])) (event E (a uint256))
      (event E ([a uint8 #:indexed] [b uint8 #:indexed] [c uint8 #:indexed] [d uint8 #:indexed]))))

  (test-case "malformed event s-expressions raise without a position"
    (for ([x (in-list sexp-rejections)])
      (define e (type-error (lambda () (sexp->abi-event x))))
      (check-pred exn:fail:web3:abi:type? e (format "~s" x))
      (when e
        (check-false (exn:fail:web3:abi:type-position e) (format "~s" x))
        (check-regexp-match #rx"^sexp->abi-event: " (exn-message e)))))

  (test-case "the struct constructors check the event rules and convert types"
    (define (indexed name) (abi-event-parameter name 'uint8 #t))
    (for ([args (in-list (list (list (map indexed '("a" "b" "c" "d")) #f)
                               (list (map indexed '("a" "b" "c" "d" "e")) #t)
                               (list (list (indexed "a") (abi-event-parameter "a" 'bool #f)) #f)))])
      (define e (type-error (lambda () (apply abi-event "E" args))))
      (check-pred exn:fail:web3:abi:type? e)
      (when e
        (check-equal? (exn:fail:web3:abi:type-input e) "E")
        (check-false (exn:fail:web3:abi:type-position e))))
    (check-equal? (length (abi-event-parameters (abi-event "E" (map indexed '("a" "b" "c" "d")) #t))) 4)
    (check-equal? (abi-event-parameters
                   (abi-event "E" (list (abi-event-parameter #f 'uint8 #f) (abi-event-parameter #f 'bool #f)) #f))
                  (list (abi-event-parameter #f (abi-uint 8) #f) (abi-event-parameter #f (abi-bool) #f)))
    (check-equal? (abi-event-parameter "a" "uint" #f) (abi-event-parameter "a" (abi-uint 256) #f)))

  (test-case "an event's signature is its name and parameter types"
    (for ([e (in-list (append (map forms-string three-forms) (generated-events 300)))])
      (define parsed (if (abi-event? e) e (string->abi-event e)))
      (check-equal? (abi-event->signature e)
                    (abi-signature (abi-event-name parsed)
                                   (map abi-event-parameter-type (abi-event-parameters parsed))
                                   #f)))
    (check-equal? (abi-signature->string (abi-event->signature (forms-string (first three-forms))))
                  "Transfer(address,address,uint256)")
    (check-equal? (abi-event->signature "E(uint256 indexed a, string b) anonymous")
                  (abi-event->signature "E(uint256 x, string)")))

  (define-syntax-parse-rule (event-signature s:str)
    #:with signature
    (datum->syntax #'s (abi-signature->string (abi-event->signature (syntax-e #'s))))
    signature)

  (test-case "events are described at phase 1, without keccak"
    (check-equal? (event-signature "event Transfer(address indexed from, address indexed to, uint indexed v)")
                  "Transfer(address,address,uint256)")
    (define names (filter values (map module-name (import-closure 'web3/abi/event-type))))
    (check-equal? (filter (lambda (n) (member (collection n) '("ffi" "net" "json" "file" "keccak")))
                          names)
                  '())
    (define web3-modules
      '("web3/abi/event-type" "web3/abi/type" "web3/private/exn" "web3/private/who"))
    (check-equal? (sort (remove-duplicates (filter (lambda (n) (equal? (collection n) "web3")) names))
                        string<?)
                  web3-modules)
    (define allowed
      (append '("racket/base" "racket/contract/base" "racket/list" "racket/match" "racket/string"
                "racket/syntax" "syntax/parse" "syntax/parse/define")
              web3-modules))
    (for* ([module (in-list web3-modules)]
           [name (in-list (filter values
                                  (map module-name
                                       (direct-imports
                                        (module-path-index-resolve
                                         (module-path-index-join (string->symbol module) #f) #t)))))])
      (check-not-false (member name allowed) (format "~a requires ~a" module name))))

  (test-case "the event constructors and conversions blame their caller"
    (check-blames-here (string->abi-event 'E))
    (check-blames-here (sexp->abi-event "E()"))
    (check-blames-here (abi-event "1E" '() #f))
    (check-blames-here (abi-event "E" '(1) #f))
    (check-blames-here (abi-event "E" '() 'no))
    (check-blames-here (abi-event-parameter "indexed" 'uint256 #f))
    (check-blames-here (abi-event-parameter "" 'uint256 #f))
    (check-blames-here (abi-event-parameter "a" 'uint256 'yes))
    (check-blames-here (abi-event-parameter 'a 'uint256 #f))
    (check-pred exn:fail:web3:abi:type? (type-error (lambda () (abi-event-parameter "a" "uint7" #f))))))
