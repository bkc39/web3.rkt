#lang racket/base

(module+ test
  (require racket/list
           rackunit
           web3
           (only-in "helpers/accounts.rkt" anvil-keys)
           (only-in "helpers/halts.rkt" designated-invalid)
           (only-in "helpers/programs.rkt" echo-init)
           (only-in "util.rkt" anvil-alice raised word))

  (define data (word 7))
  (define refused (bytes-append (bytes #xff) (make-bytes 31 7)))
  (define halting (integer->address #xe5f1fe))

  (define (echo-cases echo deployed)
    (test-case "A1: the estimate of a call to echo is the gas the same send then uses"
      (define estimate (estimate-transaction anvil-alice echo #:data data))
      (check-true (> estimate (max (intrinsic-gas data) (floor-gas data))))
      (define receipt (send-transaction/receipt anvil-alice echo #:data data #:gas-limit 100000))
      (check-equal? estimate (receipt-gas-used receipt)))

    (test-case "A1: the estimate of a creation is the gas its deploy used"
      (define estimate (estimate-transaction anvil-alice #f #:data echo-init))
      (check-true (> estimate (intrinsic-gas echo-init #:creation? #t)))
      (check-equal? estimate (receipt-gas-used deployed)))

    (test-case "A2: #:block is honoured: before echo was deployed, the address holds no code"
      (define before (sub1 (receipt-block-number deployed)))
      (check-equal? (estimate-transaction anvil-alice echo #:data data #:block before)
                    (floor-gas data)))

    (test-case "A3: a signer's estimate is its address's, not the zero address's"
      (define signer (local-signer (hex-string->secret-key (first anvil-keys))))
      (define value (ether->wei 1))
      (check-equal? (signer-address signer) anvil-alice)
      (check-pred exn:fail:web3:rpc?
                  (raised (lambda () (estimate-transaction #f echo #:data data #:value value))))
      (check-equal? (estimate-transaction signer echo #:data data #:value value)
                    (estimate-transaction anvil-alice echo #:data data #:value value)))

    (test-case "A4: a call that reverts raises exn:fail:web3:rpc:revert with the revert data"
      (define e (raised (lambda () (estimate-transaction anvil-alice echo #:data refused))))
      (check-pred exn:fail:web3:rpc:revert? e)
      (check-equal? (exn:fail:web3:rpc-method e) "eth_estimateGas")
      (check-equal? (abi-revert-data (exn->abi-revert e)) refused))

    (test-case "A5: a #:gas-limit given is the estimate's allowance"
      (define needed (estimate-transaction anvil-alice echo #:data data))
      (check-equal? (estimate-transaction anvil-alice echo #:data data #:gas-limit (* 2 needed))
                    needed)
      (define e
        (raised (lambda ()
                  (estimate-transaction anvil-alice echo #:data data #:gas-limit (sub1 needed)))))
      (check-pred exn:fail:web3:rpc? e)
      (check-false (exn:fail:web3:rpc:revert? e))
      (check-eq? (exn->halt e) 'out-of-gas))

    (test-case "A6: code that halts raises an exn:fail:web3:rpc whose halt exn->halt names"
      (anvil-set-code halting (hex-string->bytes (string-append "0x" designated-invalid)))
      (define e (raised (lambda () (estimate-transaction anvil-alice halting))))
      (check-pred exn:fail:web3:rpc? e)
      (check-false (exn:fail:web3:rpc:revert? e))
      (check-equal? (exn:fail:web3:rpc-method e) "eth_estimateGas")
      (check-eq? (exn->halt e) 'invalid-opcode)))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping estimate anvil tests: anvil not found")]
    [else
     (with-anvil ()
       (define-values (echo deployed) (deploy-contract/receipt echo-init #:from anvil-alice))
       (echo-cases echo deployed))]))
