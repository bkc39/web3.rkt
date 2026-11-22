#lang racket/base

;; The provider interface, through mock providers and a user-defined one.
;; The last test needs anvil and is skipped without it.

(module+ test
  (require openssl
           racket/contract
           racket/generic
           rackunit
           web3
           (only-in "util.rkt" check-blames-here raised))

  (define (rpc-error thunk)
    (with-handlers ([exn:fail:web3:rpc? (lambda (e)
                                          (list (exn:fail:web3:rpc-method e)
                                                (exn:fail:web3:rpc-code e)
                                                (exn:fail:web3:rpc-rpc-message e)))])
      (thunk)
      #f))

  (test-case "mock from a hash: values, procedures of the params, method not found"
    (define p (mock-provider (hash "eth_chainId" "0x7a69"
                                   "eth_mining" #f
                                   "web3_sha3" (lambda (params) (car params)))))
    (check-true (provider? p))
    (check-true (mock-provider? p))
    (check-equal? (rpc-call "eth_chainId" '() #:provider p) "0x7a69")
    (check-equal? (rpc-call "eth_mining" '() #:provider p) #f)
    (check-equal? (rpc-call "web3_sha3" '("0xab") #:provider p) "0xab")
    (check-equal? (rpc-error (lambda () (rpc-call "eth_nope" '(1) #:provider p)))
                  '("eth_nope" -32601 "Method not found"))
    (check-equal? (mock-provider-calls p)
                  '(("eth_chainId") ("eth_mining") ("web3_sha3" "0xab") ("eth_nope" 1))))

  (test-case "mock from a procedure, which may raise an RPC error"
    (define p (mock-provider (lambda (method params)
                               (if (equal? method "eth_call")
                                   (raise-rpc-error method 3 "execution reverted" "0x08c3")
                                   (list method params)))))
    (check-equal? (provider-request p "eth_accounts" '()) '("eth_accounts" ()))
    (define e (with-handlers ([exn:fail:web3:rpc? values])
                (rpc-call "eth_call" '() #:provider p)))
    (check-equal? (exn-message e) "eth_call: execution reverted")
    (check-equal? (exn:fail:web3:rpc-data e) "0x08c3")
    (check-equal? (map car (mock-provider-calls p)) '("eth_accounts" "eth_call")))

  (test-case "mock handlers are checked"
    (check-exn exn:fail:contract? (lambda () (mock-provider 'nope)))
    (check-exn exn:fail:contract? (lambda () (mock-provider (lambda (params) params))))
    (check-exn exn:fail:contract?
               (lambda () (rpc-call "m" '() #:provider (mock-provider (hash "m" (void)))))))

  (struct counting (inner [count #:auto #:mutable])
    #:auto-value 0
    #:methods gen:provider
    [(define/generic inner-request provider-request)
     (define (provider-request p method params)
       (set-counting-count! p (add1 (counting-count p)))
       (inner-request (counting-inner p) method params))])

  (test-case "a user-defined provider wrapping another"
    (define inner (mock-provider (hash "eth_blockNumber" "0x10")))
    (define p (counting inner))
    (check-true (provider? p))
    (check-equal? (with-provider p (eth-block-number)) 16)
    (check-equal? (eth-block-number #:provider p) 16)
    (check-equal? (counting-count p) 2)
    (check-equal? (length (mock-provider-calls inner)) 2)
    (check-equal? (provider-close p) (void)))

  (define (blames-caller? thunk)
    (with-handlers ([exn:fail:contract:blame?
                     (lambda (e) (regexp-match? #rx"tests/provider[.]rkt" (exn-message e)))])
      (thunk)
      #f))

  (test-case "providers are required where a provider goes"
    (check-true (blames-caller? (lambda () (current-provider 'nope))))
    (check-true (blames-caller? (lambda () (parameterize ([current-provider 5]) (void)))))
    (check-true (blames-caller? (lambda () (with-provider "http://localhost" (void)))))
    (check-blames-here (eth-chain-id #:provider 'nope))
    (check-exn exn:fail:contract? (lambda () (rpc-call "eth_chainId" '() #:provider 'nope)))
    (check-exn exn:fail:contract? (lambda () (provider-request 'nope "eth_chainId" '())))
    (check-exn exn:fail:contract? (lambda () (provider-close 'nope)))
    (check-exn #rx"no provider"
               (lambda () (with-provider #f (eth-chain-id)))))

  (test-case "#:ssl-context takes a client context, for an https URL only"
    (define unverified (ssl-make-client-context 'auto))
    (check-true (blames-caller? (lambda ()
                                  (url->http-provider "http://127.0.0.1:8545"
                                                      #:ssl-context unverified))))
    (for ([context (list 'secure #f (ssl-make-server-context))])
      (check-true (blames-caller? (lambda ()
                                    (url->http-provider "https://127.0.0.1:8545"
                                                        #:ssl-context context)))))
    (for ([url '("https://127.0.0.1:8545" "HTTPS://127.0.0.1:8545")])
      (check-true (http-provider? (url->http-provider url #:ssl-context unverified)))))

  (test-case "#:ssl-context refused for an http URL without showing the URL"
    (define e (raised (lambda ()
                        (url->http-provider "http://node.example/v3/s3cr3t-key?apikey=hunter2"
                                            #:ssl-context (ssl-make-client-context 'auto)))))
    (check-pred exn:fail:contract:blame? e)
    (check-regexp-match #rx"#:ssl-context needs an https URL" (exn-message e))
    (check-false (regexp-match? #rx"s3cr3t-key|hunter2|node[.]example" (exn-message e))))

  (test-case "a provider must implement provider-request"
    (define ns (make-base-namespace))
    (parameterize ([current-namespace ns])
      (namespace-require 'racket/generic)
      (namespace-require 'web3))
    (check-exn #rx"provider-request: required method is not implemented"
               (lambda ()
                 (eval '(struct closer ()
                          #:methods gen:provider
                          [(define (provider-close p) (void))])
                       ns))))

  (test-case "an endpoint's result contract must be flat"
    (define ns (make-base-namespace))
    (parameterize ([current-namespace ns])
      (namespace-require 'racket/contract)
      (namespace-require 'web3))
    (check-exn #rx"^eth-loose: contract violation\n  expected: flat-contract\\?"
               (lambda ()
                 (eval '(define-jsonrpc-endpoint eth_loose (-> (hash/c symbol? any/c))) ns))))

  (struct sloppy ()
    #:methods gen:provider
    [(define (provider-request p method params) (void))
     (define (provider-close p) 'closed)])

  (test-case "what a provider returns is checked"
    (define e (raised (lambda () (rpc-call "eth_chainId" '() #:provider (sloppy)))))
    (check-pred exn:fail:web3? e)
    (check-regexp-match #rx"^eth_chainId: .*#<void>" (exn-message e))
    (check-pred exn:fail:web3? (raised (lambda () (provider-request (sloppy) "eth_chainId" '()))))
    (check-equal? (provider-close (sloppy)) (void)))

  (test-case "endpoints convert parameters and results"
    (define p (mock-provider
               (hash "eth_getBalance" "0xff"
                     "eth_getStorageAt" "0x2a"
                     "eth_getBlockTransactionCountByHash" 'null
                     "eth_getBlockTransactionCountByNumber" "0x3"
                     "eth_getBlockByNumber" 'null
                     "net_peerCount" "0x0")))
    (define me "0x00000000000000000000000000000000000000aa")
    (define block-hash (string-append "0x" (make-string 64 #\b)))
    (with-provider p
      (check-equal? (eth-get-balance me 5) 255)
      (check-equal? (eth-get-balance me "latest") 255)
      (check-equal? (eth-get-storage-at me 16 "pending") "0x2a")
      (check-equal? (eth-get-block-transaction-count-by-hash block-hash) 0)
      (check-equal? (eth-get-block-transaction-count-by-number 255) 3)
      (check-false (eth-get-block-by-number "latest" #:full-transactions? #t))
      (check-equal? (net-peer-count) 0))
    (check-equal? (mock-provider-calls p)
                  `(("eth_getBalance" ,me "0x5")
                    ("eth_getBalance" ,me "latest")
                    ("eth_getStorageAt" ,me "0x10" "pending")
                    ("eth_getBlockTransactionCountByHash" ,block-hash)
                    ("eth_getBlockTransactionCountByNumber" "0xff")
                    ("eth_getBlockByNumber" "latest" #t)
                    ("net_peerCount"))))

  (test-case "the manual's mock-provider block gives what it shows"
    (define p (mock-provider (hash "eth_chainId" "0x1"
                                   "eth_getBalance" "0xff")))
    (define alice "0x00000000000000000000000000000000000a11ce")
    (check-equal? (eth-get-balance alice 5 #:provider p) 255)
    (check-equal? (mock-provider-calls p)
                  '(("eth_getBalance" "0x00000000000000000000000000000000000a11ce" "0x5"))))

  (test-case "a result the node should not have sent raises exn:fail:web3"
    (define p (mock-provider (hash "eth_chainId" 7
                                   "eth_blockNumber" "0xzz"
                                   "eth_sendRawTransaction" 42
                                   "eth_accounts" "0xaa"
                                   "eth_gasPrice" "10")))
    (define (unexpected thunk)
      (with-handlers ([exn:fail:web3? exn-message]) (thunk)))
    (check-equal? (unexpected (lambda () (eth-chain-id #:provider p)))
                  "eth_chainId: unexpected result 7")
    (check-equal? (unexpected (lambda () (eth-block-number #:provider p)))
                  "eth_blockNumber: unexpected result \"0xzz\"")
    (check-equal? (unexpected (lambda () (eth-send-raw-transaction "0x01" #:provider p)))
                  "eth_sendRawTransaction: unexpected result 42")
    (check-equal? (unexpected (lambda () (eth-accounts #:provider p)))
                  "eth_accounts: malformed account from the node: \"0xaa\"")
    (check-equal? (unexpected (lambda () (eth-gas-price #:provider p)))
                  "eth_gasPrice: unexpected result \"10\""))

  (define-jsonrpc-endpoint eth_getUncleCountByBlockNumber
    (-> [exact-nonnegative-integer? number->hex-string] exact-nonnegative-integer?)
    #:result-transform hex-string->number)

  (test-case "a defined endpoint converts its arguments and its result"
    (define p (mock-provider (hash "eth_getUncleCountByBlockNumber" "0x2")))
    (check-equal? (eth-get-uncle-count-by-block-number 16 #:provider p) 2)
    (check-equal? (with-provider p (eth-get-uncle-count-by-block-number 0)) 2)
    (check-equal? (mock-provider-calls p)
                  '(("eth_getUncleCountByBlockNumber" "0x10")
                    ("eth_getUncleCountByBlockNumber" "0x0"))))

  (define named-failure (exn:fail:web3 "eth_getUncleCountByBlockHash: malformed count"
                                       (current-continuation-marks)))

  (define-jsonrpc-endpoint eth_getUncleCountByBlockHash
    (-> hex-string? exact-nonnegative-integer?)
    #:result-transform (lambda (v) (if (equal? v "0x") (raise named-failure) (hex-string->number v))))

  (test-case "a transform's exn:fail:web3 stands, and any other failure is an unexpected result"
    (define (failure answer)
      (define node (mock-provider (hash "eth_getUncleCountByBlockHash" answer)))
      (raised (lambda () (eth-get-uncle-count-by-block-hash "0xbb" #:provider node))))
    (check-eq? (failure "0x") named-failure)
    (define unexpected (failure 'null))
    (check-pred exn:fail:web3? unexpected)
    (check-equal? (exn-message unexpected) "eth_getUncleCountByBlockHash: unexpected result 'null"))

  (define-jsonrpc-endpoint eth_getUncleByBlockNumberAndIndex
    (-> [exact-nonnegative-integer? number->hex-string] [exact-nonnegative-integer? number->hex-string]
        (list/c string? string? exact-nonnegative-integer? exact-nonnegative-integer?))
    #:result-reader list)

  (define-jsonrpc-endpoint eth_getUncleByBlockHashAndIndex
    (-> hex-string? [exact-nonnegative-integer? number->hex-string] (list/c string?))
    #:result-transform list)

  (test-case "a reader sees the method, the result and the arguments before conversion"
    (define p (mock-provider (hash "eth_getUncleByBlockNumberAndIndex" "0xaa")))
    (check-equal? (eth-get-uncle-by-block-number-and-index 16 3 #:provider p)
                  '("eth_getUncleByBlockNumberAndIndex" "0xaa" 16 3))
    (check-equal? (mock-provider-calls p) '(("eth_getUncleByBlockNumberAndIndex" "0x10" "0x3"))))

  (test-case "a transform sees only the result"
    (define p (mock-provider (hash "eth_getUncleByBlockHashAndIndex" "0xaa")))
    (check-equal? (eth-get-uncle-by-block-hash-and-index "0xbb" 1 #:provider p) '("0xaa")))

  (define-jsonrpc-endpoint eth_getUncleByBlockNumberAndHash
    (-> [exact-nonnegative-integer? number->hex-string] #:full? [boolean? #:default #f] hex-string?
        (list/c string? string? exact-nonnegative-integer? boolean? string?))
    #:result-reader list)

  (test-case "a keyword spec is an optional keyword argument, sent at its position"
    (define p (mock-provider (hash "eth_getUncleByBlockNumberAndHash" "0xaa")))
    (with-provider p
      (check-equal? (eth-get-uncle-by-block-number-and-hash 16 "0xbb")
                    '("eth_getUncleByBlockNumberAndHash" "0xaa" 16 #f "0xbb"))
      (check-equal? (eth-get-uncle-by-block-number-and-hash 16 "0xbb" #:full? #t)
                    '("eth_getUncleByBlockNumberAndHash" "0xaa" 16 #t "0xbb"))
      (check-blames-here (eth-get-uncle-by-block-number-and-hash 16 "0xbb" #:full? 'yes))
      (check-pred exn:fail:contract:arity?
                  (raised (lambda () (eth-get-uncle-by-block-number-and-hash 16 #t "0xbb")))))
    (check-equal? (mock-provider-calls p)
                  '(("eth_getUncleByBlockNumberAndHash" "0x10" #f "0xbb")
                    ("eth_getUncleByBlockNumberAndHash" "0x10" #t "0xbb"))))

  (test-case "a keyword spec cannot be #:provider"
    (check-exn #rx"every endpoint takes #:provider"
               (lambda ()
                 (parameterize ([current-namespace (make-base-namespace)])
                   (eval '(module provider-keyword racket/base
                            (require web3/rpc)
                            (define-jsonrpc-endpoint eth_foo
                              (-> #:provider [boolean? #:default #f] string?))))))))

  (test-case "a keyword spec appears once, and the error points at the repeat"
    (define refused
      (raised (lambda ()
                (parameterize ([current-namespace (make-base-namespace)])
                  (eval '(module twice racket/base
                           (require web3/rpc)
                           (define-jsonrpc-endpoint eth_foo
                             (-> #:full? [boolean? #:default #f]
                                 #:full? [boolean? #:default #t]
                                 string?))))))))
    (check-pred exn:fail:syntax? refused)
    (check-regexp-match #rx"duplicate keyword spec" (exn-message refused))
    (check-equal? (map syntax->datum (exn:fail:syntax-exprs refused)) '(#:full?)))

  (test-case "an endpoint takes a result transform or a result reader, not both"
    (check-exn exn:fail:syntax?
               (lambda ()
                 (parameterize ([current-namespace (make-base-namespace)])
                   (eval '(module two-transforms racket/base
                            (require web3/rpc)
                            (define-jsonrpc-endpoint eth_foo (-> string?)
                              #:result-transform values
                              #:result-reader values)))))))

  (define endpoint-modules
    '((module endpoints racket/base
        (require web3)
        (provide eth-get-uncle-count-by-block-number forward immediate local node)
        (define node (mock-provider (lambda (method params) "0x2")))
        (define (forward) (eth-get-uncle-count-by-block-number 1 #:provider node))
        (define-jsonrpc-endpoint eth_getUncleCountByBlockNumber
          (-> [exact-nonnegative-integer? number->hex-string] exact-nonnegative-integer?)
          #:result-transform hex-string->number)
        (define immediate (eth-get-uncle-count-by-block-number 0 #:provider node))
        (define local
          (let ()
            (define-jsonrpc-endpoint eth_getUncleCountByBlockHash
              (-> hex-string? exact-nonnegative-integer?)
              #:result-transform hex-string->number)
            (list (eth-get-uncle-count-by-block-hash "0xbb" #:provider node)
                  (map (lambda (hash) (eth-get-uncle-count-by-block-hash hash #:provider node))
                       '("0x01" "0x02"))))))
      (module caller racket/base
        (require 'endpoints)
        (provide refused)
        (define (refused)
          (with-handlers ([exn:fail:contract? exn-message])
            (eth-get-uncle-count-by-block-number -1 #:provider node))))))

  (define (endpoints-namespace)
    (define ns (make-base-namespace))
    (for ([form (in-list endpoint-modules)])
      (eval form ns))
    (eval '(require web3 'caller 'endpoints) ns)
    ns)

  (test-case "a module calls its own endpoint before and after the definition and in a body"
    (define ns (endpoints-namespace))
    (check-equal? (eval '(list immediate (forward) local) ns) '(2 2 (2 (2 2))))
    (check-equal? (eval '(map car (mock-provider-calls node)) ns)
                  '("eth_getUncleCountByBlockNumber"
                    "eth_getUncleCountByBlockHash"
                    "eth_getUncleCountByBlockHash"
                    "eth_getUncleCountByBlockHash"
                    "eth_getUncleCountByBlockNumber")))

  (test-case "a defined endpoint blames the module that calls it"
    (define message (eval '(refused) (endpoints-namespace)))
    (check-regexp-match #rx"contract from: '?endpoints\n" message)
    (check-regexp-match #rx"blaming: caller\n" message))

  (test-case "an endpoint cannot be assigned"
    (check-exn exn:fail:syntax?
               (lambda () (eval '(set! eth-get-uncle-count-by-block-number 0)
                                (endpoints-namespace)))))

  (test-case "closing a mock does nothing, and it answers after"
    (define p (mock-provider (hash "eth_chainId" "0x7a69")))
    (check-equal? (provider-close p) (void))
    (check-equal? (eth-chain-id #:provider p) 31337))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping provider anvil test: anvil not found (set WEB3_ANVIL or use `nix develop`)")]
    [else
     (with-anvil ()
       (test-case "an HTTP provider reconnects after provider-close"
         (define p (current-provider))
         (check-equal? (eth-chain-id) 31337)
         (check-equal? (provider-close p) (void))
         (check-equal? (eth-chain-id) 31337)
         (provider-close p)
         (provider-close p)
         (check-pred exact-nonnegative-integer? (eth-block-number))))]))
