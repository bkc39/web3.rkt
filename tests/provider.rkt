#lang racket/base

;; The provider interface, through mock providers and a user-defined one.
;; The last test needs anvil and is skipped without it.

(module+ test
  (require racket/generic
           racket/list
           rackunit
           web3)

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

  (test-case "mock records calls from several threads"
    (define p (mock-provider (lambda (_method params) (car params))))
    (define workers
      (for/list ([t (in-range 8)])
        (thread (lambda ()
                  (for ([i (in-range 50)])
                    (provider-request p (format "t~a" t) (list i)))))))
    (for-each thread-wait workers)
    (define calls (mock-provider-calls p))
    (check-equal? (length calls) 400)
    (for ([t (in-range 8)])
      (define method (format "t~a" t))
      (check-equal? (map cadr (filter (lambda (c) (equal? (car c) method)) calls))
                    (range 50))))

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

  (test-case "providers are required where a provider goes"
    (check-exn exn:fail:contract? (lambda () (current-provider 'nope)))
    (check-exn exn:fail:contract? (lambda () (with-provider "http://localhost" (void))))
    (check-exn exn:fail:contract? (lambda () (eth-chain-id #:provider 'nope)))
    (check-exn exn:fail:contract? (lambda () (rpc-call "eth_chainId" '() #:provider 'nope)))
    (check-exn exn:fail:contract? (lambda () (provider-request 'nope "eth_chainId" '())))
    (check-exn exn:fail:contract? (lambda () (provider-close 'nope)))
    (check-exn #rx"no provider"
               (lambda () (with-provider #f (eth-chain-id)))))

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

  (test-case "endpoints check results"
    (define p (mock-provider (hash "eth_chainId" 7 "eth_accounts" "0xaa")))
    (check-exn exn:fail:contract? (lambda () (eth-chain-id #:provider p)))
    (check-exn exn:fail:contract? (lambda () (eth-accounts #:provider p))))

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
