#lang racket/base

;; define-contract: the names it binds, the traffic of the procedures it
;; binds, and its errors, at definition, at the use site and at run time.

(module+ test
  (require compiler/cm
           racket/file
           racket/list
           racket/match
           racket/port
           racket/pretty
           racket/string
           rackunit
           web3
           (only-in web3/private/define-contract solidity-name->racket-name syntax->abi-item)
           "../examples/common.rkt"
           "abi-gen.rkt"
           (rename-in "helpers/mock.rkt" [token token-address])
           "modules.rkt"
           "helpers/temporary.rkt"
           "util.rkt")

  (define-namespace-anchor anchor)

  (define-contract token #:file "../examples/contracts/out/Token.sol/Token.json")
  (define-contract counter #:file "../examples/contracts/out/Counter.sol/Counter.json")
  (define-contract events #:file "../examples/contracts/out/Events.sol/Events.json")
  (define-contract nft
    "function safeTransferFrom(address from, address to, uint256 id)"
    "function safeTransferFrom(address from, address to, uint256 id, bytes data)"
    "function f(uint256)"
    "function f(address)"
    #:rename (["f(address)" f-address]))
  (define-contract c "function f(uint256 a) view returns (uint256)")
  (define-contract mover #:file "../examples/contracts/out/Mover.sol/Mover.json")

  (define token-path (simplify-path (path->complete-path (artifact-path "Token"))))
  (define token-errors (contract-abi-errors token))
  (define created (hash-set mined 'contractAddress (string-downcase token-address)))

  (define (fresh-namespace)
    (define ns (make-base-namespace))
    (namespace-attach-module (namespace-anchor->namespace anchor) 'web3 ns)
    (parameterize ([current-namespace ns])
      (namespace-require 'web3))
    ns)

  (define (syntax-error-of ns . forms)
    (with-handlers ([exn:fail:syntax? values])
      (for ([form (in-list forms)])
        (eval form ns))
      #f))

  (define (exports-of module-name)
    (define-values (variables syntaxes) (module->exports module-name))
    (sort (for*/list ([phase+names (in-list (append variables syntaxes))]
                      #:when (eqv? (car phase+names) 0)
                      [name (in-list (cdr phase+names))])
            (car name))
          symbol<?))

  (define (module-exports datum)
    (define ns (fresh-namespace))
    (parameterize ([current-namespace ns])
      (eval datum)
      (exports-of `',(cadr datum))))

  (define token-names
    '(token token-balance-of token-balances-of token-burn token-decimals token-deploy token-deposit
            token-deposit-event token-deposits token-info token-mint token-name token-owner token-symbol
            token-total-supply token-transfer token-transfer-event))

  (test-case "a binding from an artifact names every function, event and the deploy"
    (check-equal? token (jsexpr->contract-abi (artifact "Token")))
    (check-true (andmap procedure? (list token-balance-of token-balances-of token-burn token-decimals
                                         token-deposit token-deposits token-info token-mint token-name
                                         token-owner token-symbol token-total-supply token-transfer
                                         token-deploy)))
    (check-equal? token-transfer-event (contract-abi-event token 'Transfer))
    (check-equal? token-deposit-event (contract-abi-event token 'Deposit))
    (check-equal? (module-exports `(module m racket/base
                                     (require web3)
                                     (provide (contract-bindings-out token))
                                     (define-contract token #:file ,(path->string token-path))))
                  token-names)
    (check-true (andmap procedure? (list events-bare events-count events-flags events-lists events-nested
                                         events-note events-paired events-tag events-transfer-nft
                                         events-deploy)))
    (check-true (andmap abi-event? (list events-bare-event events-counted-event events-flags-event
                                         events-four-event events-lists-event events-nested-event
                                         events-note-event events-paired-event events-tagged-event
                                         events-transfer-event)))
    (check-true (andmap procedure? (list counter-count counter-increment counter-deploy)))
    (with-temporary-directory (dir)
      (define bare (build-path dir "bare.json"))
      (call-with-output-file bare (lambda (out) (write-string "[{\"type\":\"function\",\"name\":\"f\",\"inputs\":[]}]" out)))
      (check-equal? (module-exports `(module m racket/base
                                       (require web3)
                                       (provide (contract-bindings-out b))
                                       (define-contract b #:file ,(path->string bare))))
                    '(b b-f))
      (check-equal? (module-exports `(module m racket/base
                                       (require web3)
                                       (provide (contract-bindings-out b))
                                       (define-contract b #:file ,(path->string bare) #:bytecode #"\x60\x80")))
                    '(b b-deploy b-f))
      (check-equal? (module-exports `(module m racket/base
                                       (require web3)
                                       (provide (all-defined-out))
                                       (define-contract b "function f()" "event E()"
                                         #:rename (["f" g] ["event E" h]))))
                    '(b g h))))

  (struct name-row (solidity racket) #:prefab)

  (define name-rows
    '(#s(name-row "balanceOf" "balance-of") #s(name-row "totalSupply" "total-supply")
      #s(name-row "name" "name") #s(name-row "tokenURI" "token-uri")
      #s(name-row "getURIForToken" "get-uri-for-token") #s(name-row "DOMAIN_SEPARATOR" "domain-separator")
      #s(name-row "onERC721Received" "on-erc721-received") #s(name-row "ERC20Permit" "erc20-permit")
      #s(name-row "IPFSHash" "ipfs-hash") #s(name-row "ABCDef" "abc-def") #s(name-row "v2Swap" "v2-swap")
      #s(name-row "get2X" "get2-x") #s(name-row "uint256Max" "uint256-max") #s(name-row "a1b2" "a1b2")
      #s(name-row "set_owner" "set-owner") #s(name-row "foo__bar" "foo-bar")
      #s(name-row "__ERC20_init" "erc20-init") #s(name-row "name_" "name")
      #s(name-row "balanceOf$" "balance-of$") #s(name-row "A" "a")))

  (test-case "Solidity names become kebab-case Racket names"
    (for ([row (in-list name-rows)])
      (match-define (struct* name-row ([solidity s] [racket r])) row)
      (check-equal? (solidity-name->racket-name s) r s))
    (define ns (fresh-namespace))
    (eval `(define-contract nm ,@(for/list ([row (in-list name-rows)]
                                            #:unless (equal? (name-row-solidity row) "name_"))
                                   (format "function ~a()" (name-row-solidity row))))
          ns)
    (for ([row (in-list name-rows)]
          #:unless (equal? (name-row-solidity row) "name_"))
      (check-pred procedure? (eval (string->symbol (string-append "nm-" (name-row-racket row))) ns)))
    (eval '(define-contract nm2 "function name_()") ns)
    (check-pred procedure? (eval 'nm2-name ns))
    (define prng (seeded 6 0 0 0 1 9))
    (define alphabet (string->list "abcXYZ019_$"))
    (for ([_ (in-range 1000)])
      (define s (list->string (cons (list-ref (string->list "aB_$") (random 4 prng))
                                    (for/list ([_ (in-range (random 12 prng))])
                                      (list-ref alphabet (random (length alphabet) prng))))))
      (define r (with-handlers ([exn:fail? (lambda (_) #f)]) (solidity-name->racket-name s)))
      (when r
        (check-regexp-match #px"^[a-z0-9$]+(-[a-z0-9$]+)*$" r s)
        (check-equal? (string-replace r "-" "") (string-replace (string-downcase s) "_" "") s))))

  (define (address-traffic handler f args #:from [from #f])
    (run-mock handler
              (lambda ()
                (define signature (abi-signature->string f #:outputs? #t))
                (if (memq (abi-function-state-mutability f) '(view pure))
                    (apply contract-call token-address signature #:from from #:errors token-errors args)
                    (apply contract-send token-address signature #:from from #:errors token-errors args)))))

  (test-case "every bound procedure sends what the address form sends"
    (define prng (seeded 6 0 3 0 1 9))
    (define procedures
      (list (cons token-balance-of "balanceOf") (cons token-balances-of "balancesOf")
            (cons token-burn "burn") (cons token-decimals "decimals") (cons token-deposits "deposits")
            (cons token-info "info") (cons token-mint "mint") (cons token-name "name")
            (cons token-owner "owner") (cons token-symbol "symbol") (cons token-total-supply "totalSupply")
            (cons token-transfer "transfer") (cons token-deposit "deposit")))
    (for ([entry (in-list procedures)])
      (match-define (cons procedure name) entry)
      (define f (contract-abi-function token (string->symbol name)))
      (define outputs (abi-signature-outputs f))
      (define handler
        (hash-set (sending) "eth_call"
                  (bytes->hex-string
                   (abi-encode outputs (for/list ([t (in-list outputs)]) (random-abi-value prng t))))))
      (define args (for/list ([t (in-list (abi-signature-inputs f))]) (random-abi-value prng t)))
      (define-values (expected expected-calls) (address-traffic handler f args #:from anvil-alice))
      (define-values (result calls)
        (run-mock handler (lambda () (apply procedure token-address #:from anvil-alice args))))
      (check-equal? (list result calls) (list expected expected-calls) name))
    (define-values (_balance balance-calls)
      (run-mock (hash "eth_call" (hex-word 1)) (lambda () (token-balance-of token-address anvil-alice))))
    (check-equal? balance-calls
                  (list (list "eth_call"
                              (hasheq 'to token-address
                                      'data (bytes->hex-string (encode-call "balanceOf(address)" anvil-alice)))
                              "latest")))
    (define-values (_paid paid-calls)
      (run-mock (sending) (lambda () (token-deposit token-address "hi" #:value 1000 #:from anvil-alice))))
    (check-equal? (hash-ref (second (first paid-calls)) 'value) "0x3e8")
    (define-values (deployed deploy-calls)
      (run-mock (sending #:receipt created)
                (lambda () (token-deploy "Token" "TOK" 1000000 #:from anvil-alice))))
    (define-values (_instance instance-calls)
      (run-mock (sending #:receipt created)
                (lambda () (deploy-contract-instance token "Token" "TOK" 1000000 #:from anvil-alice))))
    (check-equal? deploy-calls instance-calls)
    (check-equal? deployed (list token-address)))

  (test-case "bound procedures return values, receipts and addresses"
    (define info (abi-encode '(string string uint8 uint256) '("Token" "TOK" 18 1000000)))
    (define-values (values-result _info-calls)
      (run-mock (hash "eth_call" (bytes->hex-string info)) (lambda () (token-info token-address))))
    (check-equal? values-result '("Token" "TOK" 18 1000000))
    (define-values (receipt _mint-calls)
      (run-mock (sending) (lambda () (token-mint token-address anvil-bob 1 #:from anvil-alice))))
    (check-eq? (receipt->jsexpr (first receipt)) mined)
    (check-equal? (format "~a" token-transfer) "#<procedure:token-transfer>"))

  (test-case "reverts are decoded with the ABI's errors, under the bound name"
    (define-values (reverted _calls)
      (run-mock (sending #:estimate (reverting "eth_estimateGas"))
                (lambda () (token-transfer token-address anvil-bob 1 #:from anvil-alice))))
    (check-equal? (abi-revert-name (exn:fail:web3:rpc:revert-revert reverted)) "InsufficientBalance")
    (define-values (mined-failure _mined-calls)
      (run-mock (sending #:receipt failed #:replay (reverting "eth_call"))
                (lambda () (token-transfer token-address anvil-bob 1 #:from anvil-alice #:gas 100000))))
    (check-regexp-match #rx"^token-transfer: transaction" (exn-message mined-failure))
    (check-equal? (abi-revert-name (exn:fail:web3:transaction-revert mined-failure)) "InsufficientBalance")
    (define-values (no-sender no-sender-calls)
      (run-mock (sending) (lambda () (token-transfer token-address anvil-bob 1))))
    (check-equal? (exn-message no-sender) "token-transfer: no sender; pass #:from or set current-account")
    (check-equal? no-sender-calls '())
    (define-values (out-of-range _range-calls)
      (run-mock (sending) (lambda () (token-transfer token-address anvil-bob -1 #:from anvil-alice))))
    (check-regexp-match #rx"^token-transfer: out of range" (exn-message out-of-range)))

  (test-case "overloads share a procedure that picks one by arity, and #:rename binds one apart"
    (define (selector-sent thunk)
      (define-values (_result calls) (run-mock (sending) thunk))
      (substring (hash-ref (second (first calls)) 'data) 0 10))
    (parameterize ([current-account anvil-alice])
      (check-equal? (selector-sent (lambda () (nft-safe-transfer-from token-address anvil-alice anvil-bob 7)))
                    (bytes->hex-string (function-selector "safeTransferFrom(address,address,uint256)")))
      (check-equal? (selector-sent (lambda () (nft-safe-transfer-from token-address anvil-alice anvil-bob 7 #"")))
                    (bytes->hex-string (function-selector "safeTransferFrom(address,address,uint256,bytes)")))
      (check-equal? (selector-sent (lambda () (nft-f token-address 1)))
                    (bytes->hex-string (function-selector "f(uint256)")))
      (check-equal? (selector-sent (lambda () (f-address token-address anvil-alice)))
                    (bytes->hex-string (function-selector "f(address)"))))
    (check-exn exn:fail:contract:arity? (lambda () (apply nft-safe-transfer-from token-address '(1)))))

  (test-case "editing the JSON file recompiles the module that reads it"
    (with-temporary-directory (dir)
      (define json (build-path dir "abi.json"))
      (define m (build-path dir "m.rkt"))
      (call-with-output-file json (lambda (out) (write-string "[{\"name\":\"f\",\"inputs\":[]}]" out)))
      (call-with-output-file m
        (lambda (out)
          (write '(module m racket/base
                    (require web3)
                    (provide (contract-bindings-out c))
                    (define-contract c #:file "abi.json"))
                 out)))
      (define (compiled-exports)
        (parameterize ([current-namespace (make-base-namespace)])
          (managed-compile-zo m)
          (module-declared? m #t)
          (define-values (variables syntaxes) (module->exports m))
          (for*/list ([phase+names (in-list (append variables syntaxes))]
                      [name (in-list (cdr phase+names))])
            (car name))))
      (define before (compiled-exports))
      (check-not-false (memq 'c-f before))
      (check-false (memq 'c-g before))
      (call-with-output-file json #:exists 'truncate
        (lambda (out) (write-string "[{\"name\":\"f\",\"inputs\":[]},{\"name\":\"g\",\"inputs\":[]}]" out)))
      (file-or-directory-modify-seconds json (+ (current-seconds) 5))
      (check-not-false (memq 'c-g (compiled-exports)))
      (define dep (file->string (build-path dir "compiled" "m_rkt.dep")))
      (check-true (string-contains? dep (path->string (simplify-path json))))))

  (struct syntax-row (clauses message needle nth) #:prefab)

  (define syntax-rows
    '(#s(syntax-row ("function f(uint7 a)") "not an ABI type" "uint7" 0)
      #s(syntax-row ("function f() veiw")
                    "unknown modifier veiw; expected external, public, view, pure, payable or returns"
                    "veiw" 0)
      #s(syntax-row ("transfer(address,uint256)") "starts with function, event, error" "transfer" 0)
      #s(syntax-row ("function f(uint256\ta)") "" "\"function f(uint256\\ta)\"" 0)
      #s(syntax-row ((function f ([a uint7]))) "not an ABI type" "uint7" 0)
      #s(syntax-row ((function f () #:view #:pure)) "" "(function f () #:view #:pure)" 0)
      #s(syntax-row ((funktion f ())) "expected one of these literal symbols" "funktion" 0)
      #s(syntax-row ("function balanceOf(address) view returns (uint256)"
                     "function balance_of(address) view returns (uint256)")
                    "two bindings are named c-balance-of"
                    "\"function balance_of(address) view returns (uint256)\"" 0)
      #s(syntax-row ("function burn()" "function _burn()") "c-burn" "\"function _burn()\"" 0)
      #s(syntax-row ("function f(uint256)" "function f(address)") "both take 1 argument; use #:rename"
                    "\"function f(address)\"" 0)
      #s(syntax-row ("function f(uint256)" "function f(uint)") "duplicate function f[(]uint256[)]"
                    "\"function f(uint)\"" 0)
      #s(syntax-row ("event Transfer(address indexed from, address indexed to, uint256 value)"
                     "function transferEvent()")
                    "c-transfer-event" "\"function transferEvent()\"" 0)
      #s(syntax-row ("function deploy()" #:bytecode #"\x60") "c-deploy" "\"function deploy()\"" 0)
      #s(syntax-row ("function _()") "no Racket name for _; use #:rename" "\"function _()\"" 0)
      #s(syntax-row ("function __()") "no Racket name for __; use #:rename" "\"function __()\"" 0)
      #s(syntax-row ("event _(uint256 a)") "no Racket name for _; use #:rename"
                    "\"event _(uint256 a)\"" 0)
      #s(syntax-row ((event _ ([a uint256]))) "no Racket name for _; use #:rename"
                    "(event _ ((a uint256)))" 0)
      #s(syntax-row ("event T(uint256 a)" "event T(address a)" #:rename (["event T" t]))
                    "event T is overloaded; use \"event T[(]…[)]\"" "\"event T\"" 0)
      #s(syntax-row ("event E(uint256 indexed a)" "event E(uint256 b)")
                    "two events share the signature E[(]uint256[)], so they cannot be bound"
                    "\"event E(uint256 b)\"" 0)
      #s(syntax-row ("function f()" #:rename (["g" h])) "#:rename: no function or event g" "\"g\"" 0)
      #s(syntax-row (#:file "nope.json") "cannot read /.*/nope.json" "\"nope.json\"" 0)
      #s(syntax-row (#:file "") "expected a path" "\"\"" 0)
      #s(syntax-row (#:file "bad.json") "not JSON" "\"bad.json\"" 0)
      #s(syntax-row (#:file "item.json") "item 0: not an ABI type" "\"item.json\"" 0)
      #s(syntax-row (#:file "x.json" "function f()") "#:file excludes inline items" "\"function f()\"" 0)
      #s(syntax-row (#:bytecode #"" #:bytecode #"") "too many #:bytecode"
                    "(define-contract c #:bytecode #\"\" #:bytecode #\"\")" 0)
      #s(syntax-row () "expected #:file or at least one item" "(define-contract c)" 0)))

  (define clause-message-rows
    '(#s(syntax-row (#:file "x.json" #:file "x.json") "too many #:file clauses" "#:file" 1)
      #s(syntax-row (#:file "x.json" "function f()") "#:file excludes inline items" "\"function f()\"" 0)
      #s(syntax-row ("function f()" #:bytecode #"" "function g()") "inline items come together"
                    "\"function g()\"" 0)
      #s(syntax-row ("function f()" #:frob 1)
                    #<<MESSAGE
expected an item string; expected (function name (parameter ...) option ...); expected (error ...), (event ...), (constructor ...), (fallback ...) or (receive); or expected one of these literals: #:bytecode, #:rename, or #:file
MESSAGE
                    "#:frob" 0)
      #s(syntax-row ("function f()" #:rename x) "expected ([key rename-id] ...) after #:rename" "x" 0)
      #s(syntax-row ("function f()" #:rename ([1 f-1])) "expected a key string" "1" 0)))

  (define (expand-program forms #:dir [dir #f])
    (define text
      (with-output-to-string
        (lambda () (pretty-write `(module m racket/base (require web3) ,@forms)))))
    (define source (if dir (build-path dir "m.rkt") 'm))
    (when dir
      (call-with-output-file source #:exists 'truncate (lambda (out) (write-string text out))))
    (define in (open-input-string text))
    (port-count-lines! in)
    (define stx (read-syntax source in))
    (define e
      (with-handlers ([exn:fail:syntax? values])
        (parameterize ([current-namespace (fresh-namespace)])
          (expand stx))
        #f))
    (values e text))

  (define (check-srcloc e text needle nth message)
    (define label (format "~a ~s" needle message))
    (check-pred exn:fail:syntax? e label)
    (when e
      (check-regexp-match (regexp message) (exn-message e) label)
      (define start (car (list-ref (regexp-match-positions* (regexp-quote needle) text) nth)))
      (define before (substring text 0 start))
      (define line (add1 (length (regexp-match-positions* #rx"\n" before))))
      (define column (- start (match (regexp-match-positions* #rx"\n" before)
                                ['() 0]
                                [newlines (cdr (last newlines))])))
      (match-define (list stx _ ...) (exn:fail:syntax-exprs e))
      (check-equal? (list (syntax-position stx) (syntax-line stx) (syntax-column stx) (syntax-span stx))
                    (list (add1 start) line column (string-length needle))
                    label)))

  (test-case "mistakes in a definition are syntax errors at the offending syntax"
    (with-temporary-directory (dir)
      (call-with-output-file (build-path dir "bad.json") (lambda (out) (write-string "[1," out)))
      (call-with-output-file (build-path dir "x.json") (lambda (out) (write-string "[]" out)))
      (call-with-output-file (build-path dir "item.json")
        (lambda (out) (write-string "[{\"name\":\"f\",\"inputs\":[{\"name\":\"a\",\"type\":\"uint7\"}]}]" out)))
      (for ([row (in-list syntax-rows)])
        (match-define (struct* syntax-row ([clauses clauses] [message message] [needle needle] [nth nth])) row)
        (define-values (e text) (expand-program (list `(define-contract c ,@clauses)) #:dir dir))
        (check-srcloc e text needle nth (string-append "define-contract: .*" message)))
      (define-values (missing _text) (expand-program '((define-contract c #:file "nope.json")) #:dir dir))
      (check-equal? (cadr (regexp-match #rx"cannot read ([^:]*): " (exn-message missing)))
                    (path->string (build-path dir "nope.json")))
      (define-values (e text) (expand-program '((define-contract "c" "function f()"))))
      (check-srcloc e text "\"c\"" 0 "expected identifier")))

  (test-case "misplaced, repeated and unknown clauses say what define-contract takes"
    (with-temporary-directory (dir)
      (call-with-output-file (build-path dir "x.json") (lambda (out) (write-string "[]" out)))
      (for ([row (in-list clause-message-rows)])
        (match-define (struct* syntax-row ([clauses clauses] [message message] [needle needle] [nth nth])) row)
        (define-values (e text) (expand-program (list `(define-contract c ,@clauses)) #:dir dir))
        (check-srcloc e text needle nth (string-append ": define-contract: " (regexp-quote message) "\n")))))

  (struct use-row (form message) #:prefab)

  (define use-definition
    '(define-contract c
       "function f(uint256) view returns (uint256)"
       "function transfer(address to, uint256 amount) returns (bool)"
       "function safeTransferFrom(address, address, uint256)"
       "function safeTransferFrom(address, address, uint256, bytes)"
       "function g() view returns (uint256)"
       "constructor()"
       #:bytecode #"\x60\x80"))

  (test-case "mistakes where a bound procedure is applied are syntax errors there"
    (define ns (fresh-namespace))
    (eval use-definition ns)
    (eval `(define A ,token-address) ns)
    (for ([row (in-list '(#s(use-row (c-f) "^c-f: the address is missing; f takes it, then 1 argument\n")
                          #s(use-row (c-g) "^c-g: the address is missing; g takes it, then 0 arguments\n")
                          #s(use-row (c-g A 1) "^c-g: g takes 0 arguments after the address; given 1\n")
                          #s(use-row (c-f A 1 2) "given 2")
                          #s(use-row (c-safe-transfer-from A 1 2)
                                     "takes 3 or 4 arguments after the address; given 2")
                          #s(use-row (c-transfer A A 1 #:value 1) "transfer is nonpayable; #:value is not allowed")
                          #s(use-row (c-transfer A A 1 #:block 5) "#:block is not allowed")
                          #s(use-row (c-f A 1 #:valu 1)
                                     "unknown keyword #:valu; allowed: #:from #:block #:gas #:errors #:provider")
                          #s(use-row (c-deploy #:value 1) "not payable")
                          #s(use-row (c-deploy 1) "^c-deploy: takes 0 arguments; given 1")
                          #s(use-row (c-f A 1 #:from)
                                     "^c-f: expected more terms starting with a value after the keyword")
                          #s(use-row (c-f A . 5) "^c-f: bad syntax\n")
                          #s(use-row (set! c-f 5) "cannot mutate")))])
      (match-define (struct* use-row ([form form] [message message])) row)
      (define e (syntax-error-of ns form))
      (check-pred exn:fail:syntax? e (format "~s" form))
      (when e
        (check-regexp-match (regexp message) (exn-message e) (format "~s" form))))
    (define-values (e text) (expand-program (list use-definition `(c-transfer ,token-address "x" 1 #:value 1))))
    (check-srcloc e text "#:value" 0 "#:value is not allowed")
    (define-values (improper improper-text) (expand-program (list use-definition `(c-f ,token-address . 5))))
    (check-srcloc improper improper-text (format "(c-f ~s . 5)" token-address) 0 ": c-f: bad syntax\n"))

  (test-case "bad values given to a bound procedure blame the module applying it"
    (check-blames-here (c-f 42 1))
    (check-blames-here (c-f token-address 1 #:gas 0))
    (check-blames-here (c-f token-address 1 #:from "alice"))
    (define ns (fresh-namespace))
    (parameterize ([current-namespace ns])
      (eval '(module L racket/base
               (require web3)
               (provide (contract-bindings-out c))
               (define-contract c "function f(uint256 a) view returns (uint256)")))
      (eval '(module M racket/base
               (require 'L)
               (provide run)
               (define (run) (c-f 42 1))))
      (check-exn #rx"blaming: [^\n]*M" (dynamic-require ''M 'run))))

  (test-case "a deploy procedure without bytecode raises before it sends anything"
    (define ns (fresh-namespace))
    (for ([definition (list '(define-contract d "function f()" #:bytecode #f)
                            '(define-contract d "function f()" #:bytecode "0x")
                            '(define-contract d "function f()" #:bytecode #"")
                            `(define-contract d #:file ,(path->string token-path) #:bytecode #f))]
          [arguments (list '() '() '() '("Token" "TOK" 1))])
      (eval definition ns)
      (define-values (e calls)
        (run-mock (sending) (lambda () (eval `(d-deploy ,@arguments #:from ,anvil-alice) ns))))
      (check-pred exn:fail:web3? e (format "~s" definition))
      (check-equal? (and (exn? e) (exn-message e)) "d-deploy: no bytecode" (format "~s" definition))
      (check-equal? calls '() (format "~s" definition))))

  (test-case "an artifact listing an error and an event twice binds each once"
    (check-equal? (module-exports `(module m racket/base
                                     (require web3)
                                     (provide (contract-bindings-out mover))
                                     (define-contract mover
                                       #:file ,(path->string (path->complete-path
                                                              (artifact-path "Mover"))))))
                  '(mover mover-deploy mover-move-left mover-move-right mover-moved-event mover-owner))
    (check-equal? mover-moved-event (string->abi-event "Moved(uint256 x)"))
    (check-equal? (contract-abi-errors mover) (list (string->abi-item "error Unauthorized(address who)")))
    (define-values (moved calls)
      (run-mock (sending) (lambda () (mover-move-left token-address 7 #:from anvil-alice))))
    (check-eq? (receipt->jsexpr (first moved)) mined)
    (check-equal? (hash-ref (second (first calls)) 'data)
                  (bytes->hex-string (encode-call "moveLeft(uint256)" 7))))

  (test-case "an item written as syntax is the item its datum or string describes"
    (define prng (seeded 6 0 6 0 2 3))
    (for ([_ (in-range 500)])
      (define i (random-abi-item prng))
      (define sexp (abi-item->sexp i))
      (define string (abi-item->string i))
      (check-equal? (syntax->abi-item (datum->syntax #f sexp)) (sexp->abi-item sexp) string)
      (check-equal? (syntax->abi-item (datum->syntax #f string)) (string->abi-item string) string)))

  (test-case "expanding define-contract loads no keccak, FFI or network module"
    (define names (filter values (map module-name (import-closure 'web3/private/define-contract))))
    (check-equal? (filter (lambda (n) (member (collection n) '("ffi" "net" "openssl" "keccak"))) names) '()))

  (test-case "define-contract at the REPL"
    (define ns (fresh-namespace))
    (eval '(define-contract c "function f(uint256 a) view returns (uint256)") ns)
    (define node (mock-provider (hash "eth_call" (hex-word 9))))
    (check-equal? (eval `(c-f ,token-address 1 #:provider ,node) ns) 9)
    (check-regexp-match #rx"^c-f: the address is missing; f takes it, then 1 argument"
                        (exn-message (syntax-error-of ns '(c-f))))
    (with-temporary-directory (dir)
      (call-with-output-file (build-path dir "x.json")
        (lambda (out) (write-string "[{\"name\":\"g\",\"inputs\":[]}]" out)))
      (parameterize ([current-directory dir])
        (eval '(define-contract x #:file "x.json") ns))
      (check-pred procedure? (eval 'x-g ns))))

  (define bezzle-items
    '((function balanceOf ([#f address]) #:view #:returns ([#f uint256]))
      (function transfer ([#f address] [#f uint256]) #:returns ([#f bool]))
      (function name () #:view #:returns ([#f string]))
      (event Transfer ([from address #:indexed] [to address #:indexed] [value uint256]))
      (error BezzleError ([#f uint256]))
      (error BezzleTypeError ([#f uint256]))))

  (test-case "a compiler's item data builds a description, an instance and a binding"
    (define d (contract-abi bezzle-items))
    (check-equal? (map abi-item->sexp (contract-abi-items d)) bezzle-items)
    (define handler (hash "eth_call" (hex-word 1000)))
    (define-values (result calls)
      (run-mock handler (lambda () (contract-call (contract-instance d token-address) 'balanceOf anvil-alice))))
    (define-values (expected expected-calls)
      (run-mock handler (lambda () (contract-call token-address "balanceOf(address)(uint256)" anvil-alice))))
    (check-equal? (list result calls) (list expected expected-calls))
    (define ns (fresh-namespace))
    (eval `(define-contract bz ,@bezzle-items) ns)
    (check-pred procedure? (eval 'bz-balance-of ns))
    (check-pred abi-event? (eval 'bz-transfer-event ns))))
