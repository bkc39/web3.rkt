#lang racket/base

(module+ test
  (require racket/list
           racket/match
           rackunit
           web3
           (only-in "abi-gen.rkt" random-address)
           (only-in "helpers/accounts.rkt" anvil-addresses anvil-keys)
           (only-in "helpers/transactions.rkt" high-s-twin slot-1)
           (only-in "util.rkt" seeded))

  (define recipients
    (let ([prng (seeded 4 2 4 2 1 9)])
      (for/list ([_ (in-range 8)])
        (address->checksum-string (random-address prng)))))

  (define gas-price (gwei->wei 10))
  (define priority-fee (gwei->wei 1))
  (define value (ether->wei 1))
  (define init-code (hex-string->bytes "0x602a60005260206000f3"))

  (define (key i)
    (hex-string->secret-key (list-ref anvil-keys i)))

  (define (pending-nonce address)
    (eth-get-transaction-count address "pending"))

  (define (build kind sender to)
    (define chain-id (eth-chain-id))
    (define nonce (pending-nonce sender))
    (match kind
      ['legacy
       (legacy-transaction #:chain-id chain-id #:nonce nonce #:gas-price gas-price
                           #:gas-limit 21000 #:to to #:value value)]
      ['pre-155
       (legacy-transaction #:chain-id #f #:nonce nonce #:gas-price gas-price
                           #:gas-limit 21000 #:to to #:value value)]
      ['eip2930
       (eip2930-transaction #:chain-id chain-id #:nonce nonce #:gas-price gas-price
                            #:gas-limit 30000 #:to to #:value value
                            #:access-list (list (list to slot-1)))]
      ['eip1559
       (eip1559-transaction #:chain-id chain-id #:nonce nonce
                            #:max-priority-fee-per-gas priority-fee #:max-fee-per-gas gas-price
                            #:gas-limit 21000 #:to to #:value value)]
      ['creation
       (eip1559-transaction #:chain-id chain-id #:nonce nonce
                            #:max-priority-fee-per-gas priority-fee #:max-fee-per-gas gas-price
                            #:gas-limit 100000 #:to #f #:value value #:data init-code)]))

  (define (quantity text)
    (hex-string->number text))

  (define (raw-by-hash hash)
    (hex-string->bytes (rpc-call "eth_getRawTransactionByHash" (list hash))))

  (define (check-node-fields stx node)
    (define tx (signed-transaction-transaction stx))
    (match-define (signature r s y-parity) (signed-transaction-signature stx))
    (define (field key) (hash-ref node key 'null))
    (define name (format "~s" stx))
    (check-equal? (string-downcase (field 'from)) (string-downcase (signed-transaction-sender stx))
                  name)
    (check-equal? (field 'hash) (signed-transaction-hash stx) name)
    (check-equal? (quantity (field 'type)) (transaction-type tx) name)
    (check-equal? (quantity (field 'nonce)) (transaction-nonce tx) name)
    (check-equal? (match (field 'chainId) ['null #f] [id (quantity id)]) (transaction-chain-id tx)
                  name)
    (check-equal? (field 'to) (match (transaction-to tx) [#f 'null] [to (string-downcase to)]) name)
    (check-equal? (quantity (field 'value)) (transaction-value tx) name)
    (check-equal? (field 'input) (bytes->hex-string (transaction-data tx)) name)
    (check-equal? (quantity (field 'gas)) (transaction-gas-limit tx) name)
    (check-equal? (quantity (field 'r)) r name)
    (check-equal? (quantity (field 's)) s name)
    (match (transaction-type tx)
      [0
       (check-equal? (quantity (field 'gasPrice)) (transaction-gas-price tx) name)
       (check-equal? (quantity (field 'v))
                     (match (transaction-chain-id tx)
                       [#f (+ 27 y-parity)]
                       [chain-id (+ 35 (* 2 chain-id) y-parity)])
                     name)]
      [type
       (check-equal? (quantity (field 'yParity)) y-parity name)
       (check-equal? (field 'accessList)
                     (for/list ([entry (in-list (transaction-access-list tx))])
                       (match-define (cons address keys) entry)
                       (hasheq 'address (string-downcase address)
                               'storageKeys (map bytes->hex-string keys)))
                     name)
       (if (= type 1)
           (check-equal? (quantity (field 'gasPrice)) (transaction-gas-price tx) name)
           (check-equal? (list (quantity (field 'maxFeePerGas))
                               (quantity (field 'maxPriorityFeePerGas)))
                         (list (transaction-max-fee-per-gas tx)
                               (transaction-max-priority-fee-per-gas tx))
                         name))]))

  (define (check-sent kind key-index recipient)
    (define sender (list-ref anvil-addresses key-index))
    (define to (and (not (eq? kind 'creation)) recipient))
    (define before (and to (eth-get-balance to "latest")))
    (define stx (sign-transaction (key key-index) (build kind sender to)))
    (define tx (signed-transaction-transaction stx))
    (define hash (eth-send-raw-transaction (bytes->hex-string (signed-transaction->bytes stx))))
    (check-equal? hash (signed-transaction-hash stx))
    (define receipt (wait-for-receipt hash))
    (check-true (receipt-succeeded? receipt))
    (check-node-fields stx (eth-get-transaction-by-hash hash))
    (check-equal? (raw-by-hash hash) (signed-transaction->bytes stx))
    (if to
        (check-equal? (- (eth-get-balance to "latest") before) value)
        (check-equal? (string-downcase (hash-ref receipt 'contractAddress))
                      (string-downcase (create-address sender (transaction-nonce tx))))))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping transaction node tests: anvil not found")]
    [else
     (with-anvil ()
       (test-case "N1, N4: legacy, EIP-2930 and EIP-1559 transfers and a creation are mined as sent"
         (for ([kind (in-list '(legacy eip2930 eip1559 creation))]
               [key-index (in-list '(1 2 3 1))]
               [recipient (in-list recipients)])
           (check-sent kind key-index recipient))))

     (with-anvil ()
       (test-case "N2: a transaction without a chain id is accepted"
         (check-sent 'pre-155 1 (list-ref recipients 4))))

     (with-anvil ()
       (test-case "N3: a high s is refused here; a node that mines it finds the signer"
         (define sender (list-ref anvil-addresses 2))
         (define stx (sign-transaction (key 2) (build 'eip1559 sender (list-ref recipients 5))))
         (define twin (high-s-twin (signed-transaction->bytes stx)))
         (check-exn exn:fail:web3:raw-transaction? (lambda () (bytes->signed-transaction twin)))
         (define hash
           (with-handlers ([exn:fail:web3:rpc? (lambda (_) #f)])
             (eth-send-raw-transaction (bytes->hex-string twin))))
         (when hash
           (check-not-equal? hash (signed-transaction-hash stx))
           (wait-for-receipt hash)
           (check-equal? (string-downcase (hash-ref (eth-get-transaction-by-hash hash) 'from))
                         (string-downcase sender)))))

     (with-anvil ()
       (test-case "N5: a transaction the node signs parses to its sender and hash"
         (define sender (first (eth-accounts)))
         (define hash (eth-send-transaction (hasheq 'from sender
                                                    'to (list-ref recipients 6)
                                                    'value (number->hex-string value))))
         (wait-for-receipt hash)
         (define stx (bytes->signed-transaction (raw-by-hash hash)))
         (check-equal? (string-downcase (signed-transaction-sender stx)) (string-downcase sender))
         (check-equal? (signed-transaction-hash stx) hash)))]))
