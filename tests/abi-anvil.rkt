#lang racket/base

;; Reverts as anvil reports them.  Skipped when anvil is not available.

(module+ test
  (require racket/format
           racket/match
           rackunit
           web3
           "revert-cases.rkt")

  ;; CALLDATASIZE PUSH0 PUSH0 CALLDATACOPY CALLDATASIZE PUSH0 REVERT
  (define echo-revert "0x365f5f37365ffd")
  (define reverter "0x0000000000000000000000000000000000000bad")

  (define (revert-exn payload)
    (with-handlers ([exn:fail:web3:rpc? values])
      (eth-call (hasheq 'to reverter 'data (bytes->hex-string payload)) "latest")
      #f))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping anvil revert tests: anvil not found (set WEB3_ANVIL or use `nix develop`)")]
    [else
     (with-anvil ()
       (anvil-set-code reverter echo-revert)

       (test-case "a revert from anvil decodes as its payload does"
         (for ([row (in-list reverts)])
           (match-define (list payload errors _ _ string) row)
           (define e (revert-exn payload))
           (check-pred exn:fail:web3:rpc? e string)
           (check-equal? (rpc-exn->revert e #:errors errors) (decode-revert payload #:errors errors)
                         string))
         (check-equal? (abi-revert->string (rpc-exn->revert (revert-exn #""))) "no revert data"))

       (test-case "anvil describes panics as panic-description does"
         (for ([row (in-list panics)])
           (match-define (list code _) row)
           (define e (revert-exn (encode-call "Panic(uint256)" code)))
           (check-equal? (exn:fail:web3:rpc-rpc-message e)
                         (format "execution reverted: panic: ~a (0x~a)"
                                 (panic-description code)
                                 (~r code #:base 16 #:min-width 2 #:pad-string "0"))))))]))
