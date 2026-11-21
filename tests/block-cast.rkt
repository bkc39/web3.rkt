#lang racket/base

(module+ test
  (require racket/match
           rackunit
           web3
           (only-in (submod web3/eth/block internal) block-fields)
           (only-in (submod web3/eth/transaction-info internal) transaction-info-fields)
           (only-in web3/private/record
                    field-kind-name field-row field-row-field field-row-kind field-row-provided?)
           "../examples/common.rkt"
           (only-in "cast.rkt" cast-path run-cast)
           (only-in "util.rkt" anvil-alice anvil-bob))

  (define numeric-kinds '(uint64 lenient-uint64 quantity))

  (define cast-lacks
    (hash "block" '(withdrawals-root parent-beacon-block-root)
          "tx" '(v)))

  (define (cast-arguments command id key)
    (match command
      ["block" (list "block" id "--field" key)]
      ["tx" (list "tx" id key)]))

  (define (cast-value kind text)
    (cond
      [(not (memq kind numeric-kinds)) text]
      [(regexp-match? #rx"^0x" text) (hex-string->number text)]
      [else (string->number text)]))

  (define (compared-rows command rows record)
    (define fields (struct->vector record))
    (define lacking (hash-ref cast-lacks command))
    (for/list ([row (in-list rows)]
               [index (in-naturals 1)]
               #:when (and (field-row-provided? row)
                           (memq (field-kind-name (field-row-kind row)) (cons 'hash numeric-kinds))
                           (not (memq (field-row-field row) lacking))
                           (vector-ref fields index)))
      (list row (vector-ref fields index))))

  (define (check-cast-reads command id rows record url)
    (for ([compared (in-list (compared-rows command rows record))])
      (match-define (list (field-row field key kind _ _) value) compared)
      (define text (apply run-cast (append (cast-arguments command id (symbol->string key))
                                           (list "--rpc-url" url))))
      (check-equal? (cast-value (field-kind-name kind) text) value
                    (format "~a ~a ~a" command field id))))

  (cond
    [(not cast-path) (displayln "skipping block cast tests: cast not found")]
    [(not (current-anvil-path)) (displayln "skipping block cast tests: anvil not found")]
    [else
     (with-anvil ()
       (parameterize ([current-account anvil-alice])
         (define url (http-provider-url (current-provider)))
         (define-values (token deployed)
           (deploy-contract/receipt (artifact-bytecode "Token") "constructor(string,string,uint256)"
                                    "Token" "TOK" 1000))
         (define sent (contract-send token "transfer(address,uint256)" anvil-bob 250))
         (test-case "B11: cast reads each block and transaction field as its accessor does"
           (for ([receipt (in-list (list deployed sent))])
             (define number (receipt-block-number receipt))
             (define hash (receipt-transaction-hash receipt))
             (check-cast-reads "block" (number->string number) block-fields
                               (eth-get-block-by-number number #f) url)
             (check-cast-reads "tx" hash transaction-info-fields
                               (eth-get-transaction-by-hash hash) url)))))]))
