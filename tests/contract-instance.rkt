#lang racket/base

;; Contract instances against a mock provider: calls, sends, estimates,
;; deploys and logs by name send what the address forms send.

(module+ test
  (require racket/list
           racket/match
           rackunit
           web3
           "../examples/common.rkt"
           "abi-gen.rkt"
           "helpers/event-fixtures.rkt"
           "helpers/mock.rkt"
           "util.rkt")

  (define token-abi (jsexpr->contract-abi (artifact "Token")))
  (define instance (contract-instance token-abi token))
  (define token-errors (contract-abi-errors token-abi))
  (define balance-data
    "0x70a08231000000000000000000000000f39fd6e51aad88f6f4ce6ab8827279cfffb92266")
  (define transfer-tx
    (hasheq 'from anvil-alice 'to token
            'data (bytes->hex-string (encode-call "transfer(address,uint256)" anvil-bob 10))))
  (define created (hash-set mined 'contractAddress (string-downcase token)))

  (define (traffic handler thunk)
    (define-values (result calls) (run-mock handler thunk))
    (list result calls))

  (test-case "a call by name sends what the address form sends"
    (define-values (result calls)
      (run-mock (hash "eth_call" (hex-word 1000))
                (lambda () (contract-call instance 'balanceOf anvil-alice #:block 7))))
    (check-equal? result '(1000))
    (check-equal? calls (list (list "eth_call" (hasheq 'to token 'data balance-data) "0x7")))
    (check-equal? (traffic (hash "eth_call" (hex-word 1000))
                           (lambda () (contract-call token "balanceOf(address)(uint256)" anvil-alice
                                                     #:block 7)))
                  (list result calls)))

  (test-case "calls and sends by name or signature send what the address forms send"
    (define prng (seeded 6 2 4 0 0 1))
    (for ([_ (in-range 100)])
      (define d (random-contract-abi prng))
      (define i (contract-instance d token))
      (define errors (contract-abi-errors d))
      (for ([f (in-list (contract-abi-functions d))])
        (define args (for/list ([t (in-list (abi-signature-inputs f))]) (random-abi-value prng t)))
        (define outputs (abi-signature-outputs f))
        (define handler
          (hash-set (sending) "eth_call"
                    (bytes->hex-string
                     (abi-encode outputs (for/list ([t (in-list outputs)]) (random-abi-value prng t))))))
        (define signature (abi-signature->string f #:outputs? #t))
        (define call? (memq (abi-function-state-mutability f) '(view pure)))
        (define (run target key)
          (traffic handler
                   (lambda ()
                     (if call?
                         (apply contract-call target key #:errors (if (string? target) errors '()) args)
                         (apply contract-send target key #:from anvil-alice
                                #:errors (if (string? target) errors '()) args)))))
        (define expected (run token signature))
        (define same-arity
          (filter (lambda (g) (and (equal? (abi-signature-name g) (abi-signature-name f))
                                   (= (length (abi-signature-inputs g)) (length args))))
                  (contract-abi-functions d)))
        (when (= (length same-arity) 1)
          (check-equal? (run i (string->symbol (abi-signature-name f))) expected signature))
        (check-equal? (run i (abi-signature->string f)) expected signature))))

  (test-case "sends, estimates and deploys from an instance"
    (check-equal? (traffic (sending) (lambda () (contract-send instance 'transfer anvil-bob 10
                                                               #:from anvil-alice)))
                  (list (list mined)
                        (list (list "eth_estimateGas" transfer-tx)
                              (list "eth_sendTransaction" (hash-set transfer-tx 'gas "0x5208"))
                              (list "eth_getTransactionReceipt" tx-hash))))
    (check-equal? (traffic (sending) (lambda () (contract-estimate-gas instance 'transfer anvil-bob 10
                                                                       #:from anvil-alice)))
                  (list '(21000) (list (list "eth_estimateGas" transfer-tx))))
    (check-equal? (traffic (sending) (lambda () (contract-estimate-gas token "transfer(address,uint256)"
                                                                       anvil-bob 10)))
                  (list '(21000) (list (list "eth_estimateGas" (hash-remove transfer-tx 'from)))))
    (define-values (result calls)
      (run-mock (sending #:receipt created)
                (lambda () (deploy-contract-instance token-abi "Token" "TOK" 1000000 #:from anvil-alice))))
    (match-define (list deployed) result)
    (check-equal? (hash-ref (second (first calls)) 'data)
                  (bytes->hex-string (bytes-append (contract-abi-bytecode token-abi)
                                                   (abi-encode '(string string uint256)
                                                               '("Token" "TOK" 1000000)))))
    (check-equal? (contract-instance-address deployed) token)
    (check-eq? (contract-instance-abi deployed) token-abi)
    (check-false (contract-instance-from deployed)))

  (test-case "an instance's sender and provider are the defaults, and keywords override them"
    (define (senders-and-node thunk)
      (define here (mock-provider (sending)))
      (define there (mock-provider (sending)))
      (define elsewhere (mock-provider (sending)))
      (parameterize ([current-provider elsewhere])
        (thunk here there))
      (list (map length (map mock-provider-calls (list here there elsewhere)))
            (for*/list ([node (list here there elsewhere)]
                        [call (in-list (mock-provider-calls node))]
                        #:when (hash? (second call)))
              (hash-ref (second call) 'from))))
    (check-equal? (senders-and-node
                   (lambda (here _there)
                     (contract-send (contract-instance token-abi token #:from anvil-bob #:provider here)
                                    'transfer anvil-carol 1)))
                  (list '(3 0 0) (list anvil-bob anvil-bob)))
    (check-equal? (senders-and-node
                   (lambda (here there)
                     (contract-send (contract-instance token-abi token #:from anvil-bob #:provider here)
                                    'transfer anvil-carol 1 #:from anvil-carol #:provider there)))
                  (list '(0 3 0) (list anvil-carol anvil-carol)))
    (check-equal? (senders-and-node
                   (lambda (_here _there)
                     (parameterize ([current-account anvil-carol])
                       (contract-send instance 'transfer anvil-bob 1))))
                  (list '(0 0 3) (list anvil-carol anvil-carol)))
    (define handler (hash-set (sending) "eth_call" (hex-word 1)))
    (parameterize ([current-account anvil-alice])
      (check-equal? (traffic handler (lambda () (contract-call instance 'balanceOf anvil-bob)))
                    (traffic handler (lambda ()
                                       (contract-call instance 'balanceOf anvil-bob
                                                      #:from (current-account) #:value 0 #:gas #f
                                                      #:block "latest" #:errors '()
                                                      #:provider (current-provider)))))
      (check-equal? (traffic handler (lambda () (contract-send instance 'transfer anvil-bob 1)))
                    (traffic handler (lambda ()
                                       (contract-send instance 'transfer anvil-bob 1
                                                      #:from (current-account) #:value 0 #:gas #f
                                                      #:errors '() #:provider (current-provider)))))))

  (define (revert-of e)
    (match e
      [(? exn:fail:web3:rpc:revert?) (exn:fail:web3:rpc:revert-revert e)]
      [(? exn:fail:web3:transaction?) (exn:fail:web3:transaction-revert e)]))

  (test-case "an instance decodes reverts with its ABI's errors"
    (define (raised-by handler thunk)
      (define-values (result _calls) (run-mock handler thunk))
      result)
    (for ([e (list (raised-by (hash "eth_call" (reverting "eth_call"))
                              (lambda () (contract-call instance 'transfer anvil-bob 1)))
                   (raised-by (sending #:estimate (reverting "eth_estimateGas"))
                              (lambda () (contract-send instance 'transfer anvil-bob 1 #:from anvil-alice)))
                   (raised-by (sending #:estimate (reverting "eth_estimateGas"))
                              (lambda () (contract-send instance 'transfer anvil-bob 1 #:from anvil-alice
                                                        #:errors '("Foo(uint256)")))))])
      (check-pred exn:fail:web3:rpc:revert? e)
      (check-equal? (abi-revert-name (revert-of e)) "InsufficientBalance")
      (check-equal? (abi-revert-arguments (revert-of e)) '(0 5000)))
    (define mined-failure
      (raised-by (sending #:receipt failed #:replay (reverting "eth_call"))
                 (lambda () (contract-send instance 'transfer anvil-bob 1 #:from anvil-alice #:gas 100000))))
    (check-pred exn:fail:web3:transaction? mined-failure)
    (check-equal? (abi-revert-name (revert-of mined-failure)) "InsufficientBalance")
    (check-false (abi-revert-name
                  (revert-of (raised-by (hash "eth_call" (reverting "eth_call"))
                                        (lambda () (contract-call token "transfer(address,uint256)(bool)"
                                                                  anvil-bob 1)))))))

  (define transfer-log
    (let-values ([(topics data) (topics+data transfer (list anvil-alice anvil-bob 10))])
      (log-jsexpr #:topics topics #:data data)))
  (define deposit-log
    (let-values ([(topics data) (topics+data deposit (list anvil-alice 5 "memo"))])
      (log-jsexpr #:topics topics #:data data #:index 1)))
  (define other-log (log-jsexpr #:topics (list (filled-hash #\c)) #:index 2))

  (test-case "logs by event name, by declaration, and all of an instance's events"
    (define handler (hash "eth_getLogs" (list transfer-log deposit-log other-log)))
    (define-values (named calls)
      (run-mock (hash "eth_getLogs" (list transfer-log))
                (lambda () (contract-logs instance 'Transfer #:where (hasheq 'to anvil-bob)))))
    (check-equal? calls
                  (list (list "eth_getLogs"
                              (hasheq 'address token 'fromBlock "earliest" 'toBlock "latest"
                                      'topics (list transfer-topic 'null (topic-word anvil-bob))))))
    (check-equal? (map event-log-name (first named)) '("Transfer"))
    (define-values (all all-calls) (run-mock handler (lambda () (contract-logs instance))))
    (check-false (hash-has-key? (second (first all-calls)) 'topics))
    (match-define (list (list t d raw)) all)
    (check-equal? (event-log-name t) "Transfer")
    (check-equal? (event-log-ref d 'memo) "memo")
    (check-false (event-log? raw))
    (define-values (declared _declared-calls)
      (run-mock (hash "eth_getLogs" (list transfer-log))
                (lambda () (contract-logs token (string-append "event " transfer)))))
    (check-equal? (event-log-ref (first (first declared)) 'value) 10)
    (define-values (bare _bare-calls) (run-mock handler (lambda () (contract-logs token))))
    (check-false (ormap event-log? (first bare))))

  (test-case "instances print their address and what their ABI holds"
    (check-equal? (format "~v" instance)
                  "#<contract-instance 0x5FbDB2315678afecb367f032d93F642f64180aa3 13 functions, 2 events, 2 errors>")
    (check-equal? (format "~a" (contract-instance (contract-abi '("error E()" "function f()"))
                                                  (string-downcase token)))
                  "#<contract-instance 0x5FbDB2315678afecb367f032d93F642f64180aa3 1 function, 1 error>")
    (check-equal? (format "~v" (contract-instance (contract-abi '()) token))
                  "#<contract-instance 0x5FbDB2315678afecb367f032d93F642f64180aa3>"))

  (test-case "names an instance cannot resolve, and values its functions refuse, blame the caller"
    (define node (mock-provider (sending)))
    (parameterize ([current-provider node])
      (check-blames-here (contract-call token 'balanceOf anvil-alice))
      (check-blames-here (contract-call token "balanceOf" anvil-alice))
      (check-blames-here (contract-call instance 'nope))
      (check-blames-here (contract-call instance 'deposit "x" #:value -1))
      (check-blames-here (contract-send instance 'transfer anvil-bob 1 #:value 5))
      (check-blames-here (contract-estimate-gas instance 'transfer anvil-bob 1 #:value 5))
      (check-blames-here (contract-logs instance 'Transfer #:where (hasheq 'value 1)))
      (check-blames-here (contract-logs instance 'Nope))
      (check-blames-here (contract-logs token #:where (hasheq 'to anvil-bob)))
      (check-blames-here (deploy-contract-instance (contract-abi '())))
      (check-blames-here (deploy-contract-instance token-abi "a" "b" 1 #:value 1))
      (check-blames-here (contract-instance 'x token))
      (check-blames-here (contract-instance token-abi "0x12")))
    (check-equal? (mock-provider-calls node) '()))

  (test-case "the manual's examples give what it shows"
    (define erc20
      (contract-abi (list "function balanceOf(address owner) view returns (uint256)"
                          "function transfer(address to, uint256 amount) returns (bool)"
                          "error InsufficientBalance(uint256 have, uint256 want)")))
    (define node (mock-provider (hash "eth_call" (bytes->hex-string (abi-encode '(uint256) '(1000))))))
    (define gold (contract-instance erc20 "0x5fbdb2315678afecb367f032d93f642f64180aa3" #:provider node))
    (check-equal? (format "~v" gold)
                  "#<contract-instance 0x5FbDB2315678afecb367f032d93F642f64180aa3 2 functions, 1 error>")
    (check-equal? (contract-call gold 'balanceOf anvil-alice) 1000)
    (check-equal? (mock-provider-calls node)
                  (list (list "eth_call" (hasheq 'to token 'data balance-data) "latest")))
    (define refusing
      (mock-provider (lambda (method _params)
                       (raise-rpc-error method 3 "execution reverted"
                                        (bytes->hex-string (encode-call insufficient-balance 0 5))))))
    (check-exn #rx"^eth_estimateGas: reverted with InsufficientBalance[(]0, 5[)]"
               (lambda () (contract-send gold 'transfer anvil-bob 5 #:from anvil-alice #:provider refusing)))))
