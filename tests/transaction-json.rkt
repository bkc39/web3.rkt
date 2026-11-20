#lang racket/base

(module+ test
  (require (only-in json jsexpr?)
           racket/list
           racket/match
           rackunit
           (only-in web3/address address->checksum-string)
           (only-in web3/rpc bytes->hex-string)
           web3/transaction
           (only-in (submod web3/transaction internal) transaction-difference)
           (only-in web3/units ether->wei gwei->wei)
           (only-in "abi-gen.rkt" random-address)
           (only-in "helpers/secrets.rkt" check-no-leak marker-hex)
           "helpers/transactions.rkt"
           "modules.rkt"
           (only-in "util.rkt" anvil-alice anvil-bob check-blames-here pick raised seeded))

  (define eip-155
    (legacy-transaction #:chain-id 1 #:nonce 9 #:gas-price (gwei->wei 20) #:gas-limit 21000
                        #:to "0x3535353535353535353535353535353535353535"
                        #:value (ether->wei 1)))

  (define pinned
    `((,eip-155 "0x9d8A62f656a8d1615C1294fd71e9CFb3E4855A4F"
                #hasheq((type . "0x0") (from . "0x9d8A62f656a8d1615C1294fd71e9CFb3E4855A4F")
                        (to . "0x3535353535353535353535353535353535353535") (nonce . "0x9")
                        (gas . "0x5208") (gasPrice . "0x4a817c800")
                        (value . "0xde0b6b3a7640000") (data . "0x") (chainId . "0x1")))
      (,example-eip1559 ,anvil-alice
                        #hasheq((type . "0x2") (from . ,anvil-alice) (to . ,anvil-bob)
                                (nonce . "0x0") (gas . "0x5208")
                                (maxPriorityFeePerGas . "0x77359400")
                                (maxFeePerGas . "0x6fc23ac00") (value . "0x14d1120d7b160000")
                                (data . "0x") (chainId . "0x7a69") (accessList . ())))
      (,(second example-transactions) ,anvil-alice
       #hasheq((type . "0x1") (from . ,anvil-alice) (to . ,anvil-bob) (nonce . "0x0")
               (gas . "0x7530") (gasPrice . "0x6fc23ac00") (value . "0x14d1120d7b160000")
               (data . "0x") (chainId . "0x7a69")
               (accessList
                . (#hasheq((address . ,anvil-bob)
                           (storageKeys
                            . (,(string-append "0x00000000000000000000000000000000"
                                               "00000000000000000000000000000001"))))))))
      (,(legacy-transaction #:chain-id #f #:nonce 9 #:gas-price 1 #:gas-limit 21000 #:to #f
                            #:data #"\x60\x80")
       ,anvil-alice
       #hasheq((type . "0x0") (from . ,anvil-alice) (nonce . "0x9") (gas . "0x5208")
               (gasPrice . "0x1") (value . "0x0") (data . "0x6080")))))

  (test-case "J1: pinned objects; no chainId before EIP-155, no to for a creation"
    (for ([row (in-list pinned)])
      (match-define (list tx from expected) row)
      (check-equal? (transaction->jsexpr tx #:from from) expected (format "~a" tx))))

  (define keys-by-kind
    '((pre-155 type from to nonce gas gasPrice value data)
      (legacy type from to nonce gas gasPrice value data chainId)
      (eip2930 type from to nonce gas gasPrice value data chainId accessList)
      (eip1559 type from to nonce gas maxPriorityFeePerGas maxFeePerGas value data chainId
               accessList)))

  (define quantity-keys
    `((nonce ,transaction-nonce) (gas ,transaction-gas-limit) (gasPrice ,transaction-gas-price)
      (maxPriorityFeePerGas ,transaction-max-priority-fee-per-gas)
      (maxFeePerGas ,transaction-max-fee-per-gas) (value ,transaction-value)
      (chainId ,transaction-chain-id)))

  (define (expected-keys tx)
    (match-define (cons _ keys) (assq (transaction-kind tx) keys-by-kind))
    (if (transaction-to tx) keys (remq 'to keys)))

  (define (random-case address prng)
    (match (random 3 prng)
      [0 (string-downcase address)]
      [1 (address->checksum-string address)]
      [2 (string-append "0x" (string-upcase (substring address 2)))]))

  (define (model-access-list tx)
    (for/list ([entry (in-list (transaction-access-list tx))])
      (match-define (cons address keys) entry)
      (hasheq 'address address 'storageKeys (map bytes->hex-string keys))))

  (define (check-json tx from json)
    (define name (format "~a" tx))
    (check-equal? (sort (hash-keys json) symbol<?) (sort (expected-keys tx) symbol<?) name)
    (for ([row (in-list quantity-keys)] #:when (hash-has-key? json (first row)))
      (match-define (list key accessor) row)
      (define text (hash-ref json key))
      (check-regexp-match #px"^0x(0|[1-9a-f][0-9a-f]*)$" text name)
      (check-equal? (string->number (substring text 2) 16) (accessor tx) name))
    (check-equal? (hash-ref json 'type) (format "0x~a" (transaction-type tx)) name)
    (check-equal? (hash-ref json 'from) (address->checksum-string from) name)
    (check-equal? (hash-ref json 'to #f) (transaction-to tx) name)
    (check-equal? (hash-ref json 'data) (bytes->hex-string (transaction-data tx)) name)
    (when (hash-has-key? json 'accessList)
      (check-equal? (hash-ref json 'accessList) (model-access-list tx) name)
      (for* ([entry (in-list (hash-ref json 'accessList))]
             [key (in-list (hash-ref entry 'storageKeys))])
        (check-equal? (string-length key) 66 name)))
    (check-true (jsexpr? json) name)
    (check-true (immutable? json) name))

  (test-case "J2: every field of the type, as quantities, EIP-55 addresses and lowercase hex"
    (define prng (seeded 5 2 7 1 8 3))
    (for* ([kind (in-list '(pre-155 legacy eip2930 eip1559))]
           [_ (in-range 200)])
      (define tx (random-transaction prng kind))
      (define from (random-case (random-address prng) prng))
      (check-json tx from (transaction->jsexpr tx #:from from)))
    (for ([tx (in-list edge-transactions)])
      (check-json tx anvil-alice (transaction->jsexpr tx #:from anvil-alice))))

  (test-case "J3: wrong arguments are the caller's contract violation"
    (define stx (sign-transaction example-key example-eip1559))
    (check-blames-here (transaction->jsexpr stx #:from anvil-alice))
    (check-blames-here (transaction->jsexpr 'tx #:from anvil-alice))
    (check-blames-here (transaction->jsexpr example-eip1559 #:from "0x12"))
    (check-blames-here (transaction->jsexpr example-eip1559 #:from (make-bytes 20 1)))
    (check-no-leak
     (exn-message (raised (lambda () (transaction->jsexpr example-eip1559 #:from marker-hex)))))
    (check-blames-here (transaction->jsexpr example-eip1559))
    (check-blames-here (transaction->jsexpr example-eip1559 #:from anvil-alice 1))
    (check-blames-here (transaction->jsexpr example-eip1559 #:from anvil-alice #:to anvil-bob)))

  (test-case "J4: web3/transaction still needs no network or JSON modules"
    (define names (filter values (map module-name (import-closure 'web3/transaction))))
    (check-equal? (filter (lambda (name) (member (collection name) '("net" "json"))) names) '())
    (check-false (member "web3/rpc" names)))

  (define changes
    `((chain-id 5) (nonce 1) (gas-price 1) (max-priority-fee-per-gas 1) (max-fee-per-gas 11)
      (gas-limit 30000) (to ,anvil-alice) (to #f) (value 1) (data #"\1")
      (access-list ((,anvil-bob)))))

  (define base-values (hasheq 'max-fee-per-gas 10 'max-priority-fee-per-gas 0))

  (test-case "J5: transaction-difference names the one field changed, or type"
    (for ([kind (in-list '(pre-155 legacy eip2930 eip1559))])
      (define base (transaction-of kind base-values))
      (check-false (transaction-difference base (transaction-of kind base-values)))
      (for ([change (in-list changes)])
        (match-define (list field value) change)
        (define changed (transaction-of kind (hash-set base-values field value)))
        (define fields (transaction-fields base))
        (define expected
          (and (hash-has-key? fields field)
               (not (equal? (hash-ref fields field) (hash-ref (transaction-fields changed) field)))
               field))
        (check-equal? (transaction-difference base changed) expected (format "~a ~a" kind change))
        (check-equal? (transaction-difference changed base) expected (format "~a ~a" kind change))))
    (check-equal? (transaction-difference (transaction-of 'pre-155 base-values)
                                          (transaction-of 'legacy base-values))
                  'chain-id)
    (for* ([a (in-list '(legacy eip2930 eip1559))]
           [b (in-list '(legacy eip2930 eip1559))]
           #:unless (eq? a b))
      (check-equal? (transaction-difference (transaction-of a base-values)
                                            (transaction-of b base-values))
                    'type))))
