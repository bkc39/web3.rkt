#lang racket/base

;; The provider interface, through mock providers and a user-defined one.
;; The last test needs anvil and is skipped without it.

(module+ test
  (require openssl
           racket/contract
           racket/generic
           rackunit
           web3
           (only-in "util.rkt" raised))

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
    (check-exn exn:fail:contract? (lambda () (eth-chain-id #:provider 'nope)))
    (check-exn exn:fail:contract? (lambda () (rpc-call "eth_chainId" '() #:provider 'nope)))
    (check-exn exn:fail:contract? (lambda () (provider-request 'nope "eth_chainId" '())))
    (check-exn exn:fail:contract? (lambda () (provider-close 'nope)))
    (check-exn #rx"no provider"
               (lambda () (with-provider #f (eth-chain-id))))
    (check-true (blames-caller? (lambda ()
                                  (url->http-provider "http://127.0.0.1:8545"
                                                      #:ssl-context
                                                      (ssl-make-client-context 'auto)))))
    (for ([context (list 'secure #f (ssl-make-server-context))])
      (check-true (blames-caller? (lambda ()
                                    (url->http-provider "https://127.0.0.1:8545"
                                                        #:ssl-context context)))))
    (check-true (http-provider? (url->http-provider "https://127.0.0.1:8545"
                                                    #:ssl-context
                                                    (ssl-make-client-context 'auto)))))

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
    (with-provider p
      (check-equal? (eth-get-balance me 5) 255)
      (check-equal? (eth-get-balance me "latest") 255)
      (check-equal? (eth-get-storage-at me 16 "pending") "0x2a")
      (check-equal? (eth-get-block-transaction-count-by-hash "0xbb") 0)
      (check-equal? (eth-get-block-transaction-count-by-number 255) 3)
      (check-equal? (eth-get-block-by-number "latest" #t) 'null)
      (check-equal? (net-peer-count) 0))
    (check-equal? (mock-provider-calls p)
                  `(("eth_getBalance" ,me "0x5")
                    ("eth_getBalance" ,me "latest")
                    ("eth_getStorageAt" ,me "0x10" "pending")
                    ("eth_getBlockTransactionCountByHash" "0xbb")
                    ("eth_getBlockTransactionCountByNumber" "0xff")
                    ("eth_getBlockByNumber" "latest" #t)
                    ("net_peerCount"))))

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
                  "eth_accounts: unexpected result \"0xaa\"")
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
