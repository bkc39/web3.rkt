#lang racket/base

(module+ test
  (require racket/match
           rackunit
           web3
           (only-in (submod web3/eth/receipt internal) receipt-fields)
           (only-in web3/private/record field-row field-row-provided?)
           "../examples/common.rkt"
           (only-in "cast.rkt" cast-path run-cast)
           (only-in "util.rkt" anvil-alice anvil-bob))

  (define (cast-text key v)
    (match* (key v)
      [(_ #f) ""]
      [('status 1) "true"]
      [('status 0) "false"]
      [(_ (? exact-integer?)) (number->string v)]
      [(_ (? bytes?)) (bytes->hex-string v)]
      [(_ (? string?)) v]))

  (define (check-cast-reads receipt url)
    (define fields (struct->vector receipt))
    (for ([row (in-list receipt-fields)]
          [index (in-naturals 1)]
          #:when (field-row-provided? row))
      (match-define (field-row field key _ _ _) row)
      (check-equal? (run-cast "receipt" (receipt-transaction-hash receipt) (symbol->string key)
                              "--rpc-url" url)
                    (cast-text key (vector-ref fields index))
                    (format "~a ~a" field (receipt-transaction-hash receipt)))))

  (cond
    [(not cast-path) (displayln "skipping receipt cast tests: cast not found")]
    [(not (current-anvil-path)) (displayln "skipping receipt cast tests: anvil not found")]
    [else
     (with-anvil ()
       (parameterize ([current-account anvil-alice])
         (define url (http-provider-url (current-provider)))
         (define-values (token deployed)
           (deploy-contract/receipt (artifact-bytecode "Token") "constructor(string,string,uint256)"
                                    "Token" "TOK" 1000))
         (define sent (contract-send token "transfer(address,uint256)" anvil-bob 250))
         (test-case "R15: cast reads each receipt field as its accessor does"
           (check-cast-reads deployed url)
           (check-cast-reads sent url))))]))
