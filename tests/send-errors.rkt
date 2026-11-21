#lang racket/base

(module+ test
  (require (only-in racket/contract exn:fail:contract:blame? flat-contract-predicate)
           racket/list
           racket/match
           rackunit
           (only-in libsecp256k1 integer->secret-key)
           web3
           (only-in web3/private/send fill-reason/c)
           "helpers/mock.rkt"
           (only-in "helpers/secrets.rkt" check-no-leak marker marker-hex marker-integer)
           "helpers/signers.rkt"
           (only-in "util.rkt" anvil-alice anvil-bob check-blames-here filled-hash
                    insufficient-balance))

  (define procedures (list fill-transaction send-signed-transaction send-signed-transaction/receipt))
  (define senders (rest procedures))

  (define (run procedure handler fields #:signer [signer (watched)] #:to [to anvil-bob])
    (define-values (result calls)
      (run-mock handler (lambda () (call-with-fields procedure fields signer to))))
    (values result (map first calls)))

  (define (check-refusal e procedure methods detail name)
    (check-pred exn? e name)
    (when (exn? e)
      (check-equal? (exn-message e) (format "~a: ~a" (object-name procedure) detail) name)
      (check-no-leak (exn-message e) name))
    (check-false (member "eth_sendRawTransaction" methods) name))

  (define conflict-rows
    '(((#:type 0 #:access-list ()) "#:type 0 takes no #:access-list")
      ((#:type 0 #:max-fee-per-gas 1 #:access-list ()) "#:type 0 takes no #:max-fee-per-gas")
      ((#:type 1 #:max-priority-fee-per-gas 1) "#:type 1 takes no #:max-priority-fee-per-gas")
      ((#:type 2 #:gas-price 1) "#:type 2 takes no #:gas-price")
      ((#:gas-price 1 #:max-fee-per-gas 2)
       "#:gas-price and #:max-fee-per-gas are for different transaction types")
      ((#:gas-price 1 #:max-priority-fee-per-gas 1 #:max-fee-per-gas 2)
       "#:gas-price and #:max-priority-fee-per-gas are for different transaction types")
      ((#:max-priority-fee-per-gas 3 #:max-fee-per-gas 2)
       "#:max-priority-fee-per-gas is more than #:max-fee-per-gas")))

  (test-case "F8, F16: fields of two types are refused before any request"
    (for* ([procedure (in-list procedures)]
           [row (in-list conflict-rows)])
      (match-define (list fields text) row)
      (define name (format "~a ~s" (object-name procedure) fields))
      (define-values (e methods) (run procedure (filling) fields))
      (check-true (and (exn:fail:contract? e) (not (exn:fail:contract:blame? e))) name)
      (check-refusal e procedure methods text name)
      (check-equal? methods '() name)))

  (define fill-rows
    `((chain-id (#:chain-id 1) () ("eth_chainId")
                "wrong chain: #:chain-id is 1, but the node's chain is 31337")
      (no-base-fee (#:type 2) (#:block ,legacy-block)
                   ("eth_chainId" "eth_estimateGas" "eth_getTransactionCount" "eth_getBlockByNumber")
                   ,(string-append "no base fee: the latest block has no base fee, so a type 2 "
                                   "transaction needs #:max-fee-per-gas"))
      (fee-cap (#:max-fee-per-gas 1000) ()
               ("eth_chainId" "eth_estimateGas" "eth_getTransactionCount" "eth_maxPriorityFeePerGas")
               ,(string-append "fee cap too low: #:max-fee-per-gas is 1000 wei, below the node's "
                               "priority fee of 1000000000 wei"))))

  (test-case "F9, F16, F20: what the node's answers cannot complete is refused unsigned"
    (for* ([procedure (in-list procedures)]
           [row (in-list fill-rows)])
      (match-define (list reason fields overrides expected-methods detail) row)
      (define name (format "~a ~a" (object-name procedure) reason))
      (define signer (watched))
      (define-values (e methods)
        (run procedure (call-with-fields filling overrides) fields #:signer signer))
      (check-pred exn:fail:web3:fill? e name)
      (check-pred exn:fail:web3? e name)
      (check-eq? (exn:fail:web3:fill-reason e) reason name)
      (check-true ((flat-contract-predicate fill-reason/c) reason) name)
      (check-refusal e procedure methods detail name)
      (check-regexp-match (string-append "^(fill-transaction|send-signed-transaction"
                                         "|send-signed-transaction/receipt): (wrong chain"
                                         "|no base fee|fee cap too low): ")
                          (exn-message e)
                          name)
      (check-true (< (string-length (exn-message e)) 200) name)
      (check-equal? methods expected-methods name)
      (check-false (memq 'digest (watched-calls signer)) name)))

  (define (hex n)
    (number->hex-string n))

  (define malformed-rows
    `(((#:chain-id "0x") "chain id" "0x")
      ((#:chain-id 5) "chain id" 5)
      ((#:chain-id ,(hex (expt 2 64))) "chain id" ,(hex (expt 2 64)))
      ((#:count "0x") "transaction count" "0x")
      ((#:count ,(hex (expt 2 64))) "transaction count" ,(hex (expt 2 64)))
      ((#:estimate ,(hex (expt 2 64))) "gas estimate" ,(hex (expt 2 64)))
      ((#:block null) "latest block" null)
      ((#:block "x") "latest block" "x")
      ((#:block #hasheq((baseFeePerGas . "0xzz"))) "base fee" "0xzz")
      ((#:block ,legacy-block #:gas-price "1") "gas price" "1")
      ((#:priority ,(hex (expt 2 256))) "priority fee" ,(hex (expt 2 256)))))

  (define (plain-web3? e)
    (and (exn:fail:web3? e)
         (not (ormap (lambda (subtype?) (subtype? e))
                     (list exn:fail:web3:rpc? exn:fail:web3:fill? exn:fail:web3:transaction?
                           exn:fail:web3:abi? exn:fail:web3:signer?)))))

  (test-case "F10, F16: malformed node data is refused, and nothing is sent"
    (for* ([procedure (in-list procedures)]
           [row (in-list (cons '((#:estimate "0x0") #f "0x0") malformed-rows))])
      (match-define (list overrides what answer) row)
      (define name (format "~a ~s" (object-name procedure) row))
      (define-values (e methods) (run procedure (call-with-fields filling overrides) '()))
      (check-true (plain-web3? e) name)
      (check-refusal e procedure methods
                     (if what
                         (format "malformed ~a from the node: ~e" what answer)
                         "the node estimated 0 gas")
                     name)))

  (test-case "F11: a reverting estimate raises the decoded revert, and nothing is signed"
    (for ([procedure (in-list procedures)])
      (define signer (watched))
      (define-values (e methods)
        (run procedure (filling #:estimate (reverting "eth_estimateGas"))
             `(#:errors (,insufficient-balance))
             #:signer signer))
      (check-pred exn:fail:web3:rpc:revert? e)
      (check-equal? (abi-revert-arguments (exn:fail:web3:rpc:revert-revert e)) '(0 5000))
      (check-equal? methods '("eth_chainId" "eth_estimateGas"))
      (check-equal? (watched-calls signer) '(address))))

  (define rejections
    (append (for/list ([message (in-list '("nonce too low"
                                           "replacement transaction underpriced"
                                           "Insufficient funds for gas * price + value"
                                           "max fee per gas less than block base fee"
                                           "transaction already imported"))])
              (cons -32003 message))
            (list (cons -32000 "intrinsic gas too low"))))

  (define (raised-rpc-error method code message)
    (with-handlers ([exn:fail:web3:rpc? values])
      (raise-rpc-error method code message)))

  (test-case "F12: the node's refusals of the raw bytes pass through unchanged"
    (for* ([procedure (in-list senders)]
           [rejection (in-list rejections)])
      (match-define (cons code message) rejection)
      (define refusal (raised-rpc-error "eth_sendRawTransaction" code message))
      (define-values (e _methods)
        (run procedure (filling #:raw (lambda (_params) (raise refusal))) '()))
      (check-eq? e refusal message)))

  (define transfer `(#:value ,(ether->wei "1.5")))

  (test-case "F13: an answer other than the transaction's hash is refused"
    (for ([procedure (in-list senders)])
      (for ([answer (list (filled-hash #\a) 42)])
        (define-values (e _methods) (run procedure (filling #:raw answer) transfer))
        (check-pred exn:fail:web3? e)
        (check-equal? (exn-message e)
                      (format "~a: sent transaction ~a, but the node answered ~e"
                              (object-name procedure) eip1559-hash answer))
        (check-no-leak (exn-message e)))
      (define upper (string-append "0x" (string-upcase (substring eip1559-hash 2))))
      (define-values (result _methods) (run procedure (filling #:raw upper) transfer))
      (check-false (exn? (first result)))))

  (test-case "F14, F16: a signer's failure sends nothing"
    (define unavailable (raised-rpc-error "eth_signTransaction" -32601 "Method not found"))
    (for ([procedure (in-list senders)])
      (define who (object-name procedure))
      (define-values (lie lie-methods) (run procedure (filling) '() #:signer (liar marker key0)))
      (check-pred exn:fail:web3:signer? lie)
      (check-regexp-match (format "^~a: wrong signer: " who) (exn-message lie))
      (check-no-leak (exn-message lie))
      (define-values (offline offline-methods)
        (run procedure (hash-set (filling) "eth_signTransaction" (lambda (_) (raise unavailable)))
             '()
             #:signer (node-signer anvil-alice)))
      (check-eq? offline unavailable)
      (define-values (unsupported unsupported-methods)
        (run procedure (filling) '() #:signer (message-only)))
      (check-eq? (exn:fail:web3:signer-reason unsupported) 'unsupported)
      (check-regexp-match (format "^~a: not supported: " who) (exn-message unsupported))
      (for ([methods (list lie-methods offline-methods unsupported-methods)])
        (check-false (member "eth_sendRawTransaction" methods)))))

  (test-case "F23: a signer whose address changes after the fill sends nothing"
    (for ([procedure (in-list senders)])
      (define who (symbol->string (object-name procedure)))
      (define-values (e methods) (run procedure (filling) '() #:signer (flipper)))
      (check-pred exn:fail:web3:signer? e who)
      (when (exn:fail:web3:signer? e)
        (check-eq? (exn:fail:web3:signer-reason e) 'wrong-signer who)
        (check-equal? (exn-message e)
                      (format (string-append "~a: wrong signer: the transaction's sender is ~a, "
                                             "not the address it was filled for ~a")
                              who anvil-bob anvil-alice)
                      who)
        (check-no-leak (exn-message e)))
      (check-false (member "eth_sendRawTransaction" methods) who)))

  (test-case "F24: a revert at the raw send is decoded with #:errors"
    (for ([procedure (in-list senders)])
      (define-values (e _methods)
        (run procedure (filling #:raw (reverting "eth_sendRawTransaction"))
             `(#:errors (,insufficient-balance))))
      (check-pred exn:fail:web3:rpc:revert? e)
      (when (exn:fail:web3:rpc:revert? e)
        (check-equal? (abi-revert-arguments (exn:fail:web3:rpc:revert-revert e)) '(0 5000)))))

  (define huge (string-append "0x" (make-string 10000000 #\f)))

  (define huge-rows
    `(((#:chain-id ,huge) "chain id")
      ((#:estimate ,huge) "gas estimate")
      ((#:count ,huge) "transaction count")
      ((#:block ,(hasheq 'baseFeePerGas huge)) "base fee")
      ((#:priority ,huge) "priority fee")
      ((#:block ,legacy-block #:gas-price ,huge) "gas price")))

  (test-case "F25: a 10 MB quantity is refused as malformed at once"
    (define start (current-inexact-milliseconds))
    (for ([row (in-list huge-rows)])
      (match-define (list overrides what) row)
      (define-values (e _methods) (run fill-transaction (call-with-fields filling overrides) '()))
      (check-true (plain-web3? e) what)
      (when (exn? e)
        (check-regexp-match (format "^fill-transaction: malformed ~a from the node: \"0xfff" what)
                            (exn-message e))
        (check-true (< (string-length (exn-message e)) 400) what)))
    (define elapsed (- (current-inexact-milliseconds) start))
    (check-true (< elapsed 1000.0) (format "~a ms" elapsed)))

  (test-case "F15: a node signer signs at the send's provider"
    (define-values (filled _fill-methods) (run fill-transaction (filling) '()))
    (match-define (list tx) filled)
    (define signed (bytes->hex-string (signed-transaction->bytes (sign-transaction key0 tx))))
    (define answers (hash-set (filling) "eth_signTransaction" signed))
    (define sign-call `("eth_signTransaction" ,(transaction->jsexpr tx #:from anvil-alice)))
    (define node (mock-provider answers))
    (define other (mock-provider answers))
    (parameterize ([current-provider other])
      (send-signed-transaction (node-signer anvil-alice) anvil-bob #:provider node))
    (define calls (mock-provider-calls node))
    (check-equal? (map first calls)
                  '("eth_chainId" "eth_estimateGas" "eth_getTransactionCount"
                    "eth_getBlockByNumber" "eth_maxPriorityFeePerGas" "eth_signTransaction"
                    "eth_sendRawTransaction"))
    (check-equal? (list-ref calls 5) sign-call)
    (check-equal? (mock-provider-calls other) '())
    (define signing (mock-provider answers))
    (send-signed-transaction (node-signer anvil-alice #:provider signing) anvil-bob
                             #:provider node)
    (check-equal? (mock-provider-calls signing) (list sign-call)))

  (define marker-key (integer->secret-key marker-integer))

  (test-case "F19: wrong arguments blame the caller, name their kind and show no key"
    (parameterize ([current-provider (mock-provider (filling))])
      (for* ([procedure (in-list procedures)]
             [bad (list marker-hex marker-key (make-bytes 20 1) #f)])
        (define kind (if (eq? procedure fill-transaction) "an address or a signer" "a signer"))
        (define e (with-handlers ([exn:fail? values]) (procedure bad anvil-bob)))
        (check-pred exn:fail:contract:blame? e)
        (check-regexp-match (regexp-quote (format "expected: ~a" kind)) (exn-message e))
        (check-no-leak (exn-message e)))
      (for* ([procedure (in-list procedures)]
             [call (list (lambda () (procedure signer0 marker-hex))
                         (lambda () (procedure signer0 anvil-bob #:gas 1))
                         (lambda () (procedure signer0))
                         (lambda () (procedure signer0 anvil-bob #:data "0x"))
                         (lambda () (procedure signer0 anvil-bob #:type 3)))])
        (check-blames-here (call))
        (check-no-leak (exn-message (with-handlers ([exn:fail? values]) (call))))))))
