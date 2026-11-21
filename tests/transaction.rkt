#lang racket/base

(module+ test
  (require (only-in keccak keccak256)
           (only-in racket/contract flat-contract-predicate)
           (only-in libsecp256k1 integer->secret-key)
           racket/list
           racket/match
           rackunit
           web3/account
           (only-in web3/address address->checksum-string)
           (only-in web3/private/hex bytes->natural)
           (only-in web3/rlp rlp-decode)
           (only-in web3/rpc bytes->hex-string)
           web3/transaction
           (only-in web3/units ether->wei gwei->wei)
           "helpers/accounts.rkt"
           (only-in "helpers/secrets.rkt" check-no-leak marker-integer)
           "helpers/transactions.rkt"
           "modules.rkt"
           (only-in "util.rkt" anvil-bob check-blames-here seeded))

  (define eip-155
    (legacy-transaction #:chain-id 1 #:nonce 9 #:gas-price (gwei->wei 20) #:gas-limit 21000
                        #:to "0x3535353535353535353535353535353535353535"
                        #:value (ether->wei 1)))

  (define eip-155-key
    (hex-string->secret-key "0x4646464646464646464646464646464646464646464646464646464646464646"))

  (test-case "L1: the EIP-155 text's example"
    (define stx (sign-transaction eip-155-key eip-155))
    (check-equal? (bytes->hex-string (transaction->bytes eip-155))
                  (string-append
                   "0xec098504a817c800825208943535353535353535353535353535353535353535"
                   "880de0b6b3a764000080018080"))
    (check-equal? (bytes->hex-string (transaction-signing-hash eip-155))
                  "0xdaf5a779ae972f972197303d7b574746c7ef83eadac0f2791ad23db92e4c8e53")
    (check-equal? (bytes->hex-string (signed-transaction->bytes stx))
                  (string-append
                   "0xf86c098504a817c800825208943535353535353535353535353535353535353535"
                   "880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c"
                   "71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc"
                   "64214b297fb1966a3b6d83"))
    (match-define (signed-transaction _ (signature r s y)) stx)
    (check-equal? (list r s y)
                  '(18515461264373351373200002665853028612451056578545711640558177340181847433846
                    46948507304638947509940763649030358759909902576025900602547168820602576006531
                    0))
    (check-equal? (signed-transaction-sender stx) "0x9d8A62f656a8d1615C1294fd71e9CFb3E4855A4F"))

  (define pinned-unsigned
    `((eip1559 ,(hasheq 'chain-id 31337 'max-priority-fee-per-gas (gwei->wei 2)
                        'max-fee-per-gas (gwei->wei 30) 'value (ether->wei "1.5"))
               ("0x02f2827a698084773594008506fc23ac008252089470997970c51812dc3a010c7d01b50e0d17"
                "dc79c88814d1120d7b16000080c0"))
      (eip1559 ,(hasheq 'chain-id (sub1 (expt 2 64)) 'nonce (sub1 (expt 2 64))
                        'gas-limit (sub1 (expt 2 64)) 'max-fee-per-gas 1 'to #f
                        'data #"\x60\x80")
               ("0x02e388ffffffffffffffff88ffffffffffffffff800188ffffffffffffffff8080826080c0"))
      (eip1559 ,(hasheq 'nonce 1 'max-priority-fee-per-gas 3 'max-fee-per-gas 7
                        'gas-limit 50000 'value 5 'data #"\xde\xad\xbe\xef")
               ("0x02e30101030782c3509470997970c51812dc3a010c7d01b50e0d17dc79c80584deadbeefc0"))
      (eip2930 ,(hasheq)
               ("0x01de0180808252089470997970c51812dc3a010c7d01b50e0d17dc79c88080c0"))
      (eip1559 ,(hasheq 'gas-limit 60000 'to #f
                        'access-list '(("0x0000000000000000000000000000000000000001")))
               ("0x02e20180808082ea60808080d7d6940000000000000000000000000000000000000001c0"))))

  (test-case "L2: unsigned encodings that cast mktx --raw-unsigned printed"
    (for ([row (in-list pinned-unsigned)])
      (match-define (list kind fields chunks) row)
      (check-equal? (bytes->hex-string (transaction->bytes (transaction-of kind fields)))
                    (apply string-append chunks)
                    (format "~s" row)))
    (check-equal? (bytes->hex-string (transaction-signing-hash example-eip1559))
                  "0x71332aa4ec0c857500caebcfd8864ac4dbfcd623ea1244c72b294e798959f48d"))

  (define pinned-signed
    '((("0xf86e808506fc23ac008252089470997970c51812dc3a010c7d01b50e0d17dc79c88814d1120d7b16"
        "00008082f4f6a0bcdaf956c629f89d42ac0b1c93413ba5054ed8994e698357dd5f46e67b0faccea06607"
        "43d0f7ea2e5700c4d8a6aec09bd3f91260bda7e208a9c0619265741b7598")
       "0x23d0874d9c3f3f40f63df8695d58550f22d4095d05272b0564eca4433005cde9")
      (("0x01f8a9827a69808506fc23ac008275309470997970c51812dc3a010c7d01b50e0d17dc79c88814d1"
        "120d7b16000080f838f79470997970c51812dc3a010c7d01b50e0d17dc79c8e1a000000000000000000000"
        "0000000000000000000000000000000000000000000101a0803a9c4afe2ab4265bfc759488c8c64e3c31b9"
        "4a723e5109851afbe391bc8bb9a011fb7da455cc3c5cfc3fff4b48318aa3c42832481634546bdf474cc5dd"
        "69c7a7")
       "0x9e7b669c47958eaa50ebec28a6941dfc397986688e124eb1cacc1d290a693326")
      (("0x02f875827a698084773594008506fc23ac008252089470997970c51812dc3a010c7d01b50e0d17dc79"
        "c88814d1120d7b16000080c001a0e4283c98410a9ee00d296ef6d31b47e6326541bf89e14640e3ff09b1"
        "45517165a070e1e6a5e2a7b71af2ff6278becbe8de276a30f350ea156b438678751338171f")
       "0x6e6196f107bb02ceffd0db99a2c3be3053f8d1917061e67f88c7843aa77833e2")))

  (test-case "L3: signed encodings and hashes that cast mktx and decode-tx printed"
    (for ([tx (in-list example-transactions)]
          [row (in-list pinned-signed)])
      (match-define (list chunks hash) row)
      (define stx (sign-transaction example-key tx))
      (check-equal? (bytes->hex-string (signed-transaction->bytes stx))
                    (apply string-append chunks))
      (check-equal? (signed-transaction-hash stx) hash)))

  (define model-cases
    (let ([prng (seeded 7 5 1 2 3 4)])
      (append (for*/list ([kind (in-list '(pre-155 legacy eip2930 eip1559))]
                          [_ (in-range 300)])
                (cons (random-transaction prng kind) (random-key prng)))
              (for/list ([tx (in-list edge-transactions)])
                (cons tx (random-key prng))))))

  (define model-signed
    (for/list ([pair (in-list model-cases)])
      (match-define (cons tx key) pair)
      (sign-transaction key tx)))

  (test-case "L4: encodings and hashes agree with the model written from the EIPs"
    (for ([pair (in-list model-cases)]
          [stx (in-list model-signed)])
      (match-define (cons tx _) pair)
      (define name (format "~s" tx))
      (define unsigned (model-unsigned tx))
      (define raw (model-raw tx (signed-transaction-signature stx)))
      (check-equal? (transaction->bytes tx) unsigned name)
      (check-equal? (transaction-signing-hash tx) (keccak256 unsigned) name)
      (check-equal? (signed-transaction->bytes stx) raw name)
      (check-equal? (signed-transaction-hash stx) (bytes->hex-string (keccak256 raw)) name)))

  (define (legacy-v raw)
    (match (rlp-decode raw)
      [(list _ ... v _ _) (bytes->natural v)]))

  (test-case "L5: v and the first byte of raw bytes"
    (for ([chain-id (in-list (list #f 0 1 31337 (sub1 (expt 2 64))))])
      (define stx
        (sign-transaction example-key (transaction-of (if chain-id 'legacy 'pre-155)
                                                      (hasheq 'chain-id chain-id))))
      (match-define (signature _ _ y) (signed-transaction-signature stx))
      (check-equal? (legacy-v (signed-transaction->bytes stx))
                    (if chain-id (+ 35 (* 2 chain-id) y) (+ 27 y))
                    (format "chain ~a" chain-id)))
    (for ([stx (in-list model-signed)])
      (define first-byte (bytes-ref (signed-transaction->bytes stx) 0))
      (match (transaction-type (signed-transaction-transaction stx))
        [0 (check-true (>= first-byte #xc0))]
        [type (check-equal? first-byte type)])))

  (test-case "L6: without a chain id the signing preimage is the RLP of six items"
    (for ([tx (in-list edge-transactions)]
          #:when (eq? (transaction-kind tx) 'pre-155))
      (check-equal? (length (rlp-decode (transaction->bytes tx))) 6)))

  (test-case "R1: parsing raw bytes gives the signed transaction back"
    (for ([stx (in-list model-signed)])
      (define parsed (bytes->signed-transaction (signed-transaction->bytes stx)))
      (define name (format "~s" stx))
      (check-equal? parsed stx name)
      (check-equal? (signed-transaction-transaction parsed) (signed-transaction-transaction stx))
      (check-equal? (signed-transaction-signature parsed) (signed-transaction-signature stx))
      (check-equal? (signed-transaction-sender parsed) (signed-transaction-sender stx))
      (check-equal? (signed-transaction-hash parsed) (signed-transaction-hash stx))))

  (define (rebuilt tx)
    (transaction-of (transaction-kind tx) (transaction-fields tx)))

  (test-case "R3: values are equal exactly when their bytes are"
    (define sample
      (append edge-transactions (take (map signed-transaction-transaction model-signed) 40)))
    (for* ([a (in-list sample)]
           [b (in-list sample)])
      (check-equal? (equal? a b) (equal? (transaction->bytes a) (transaction->bytes b))))
    (for ([tx (in-list sample)])
      (define twin (rebuilt tx))
      (check-equal? twin tx)
      (check-equal? (equal-hash-code twin) (equal-hash-code tx))
      (check-equal? (equal-secondary-hash-code twin) (equal-secondary-hash-code tx)))
    (define signed-sample (take model-signed 40))
    (for* ([a (in-list signed-sample)]
           [b (in-list signed-sample)])
      (check-equal? (equal? a b)
                    (equal? (signed-transaction->bytes a) (signed-transaction->bytes b))))
    (for ([stx (in-list signed-sample)])
      (define twin (bytes->signed-transaction (signed-transaction->bytes stx)))
      (check-equal? (equal-hash-code twin) (equal-hash-code stx)))
    (define kinds
      (list (transaction-of 'pre-155 (hasheq)) (transaction-of 'legacy (hasheq))
            (transaction-of 'eip2930 (hasheq)) (transaction-of 'eip1559 (hasheq))))
    (for* ([a (in-list kinds)]
           [b (in-list kinds)]
           #:unless (eq? a b))
      (check-false (equal? a b)))
    (for ([v (in-list (append sample signed-sample))])
      (check-not-equal? (transaction? v) (signed-transaction? v))))

  (test-case "R4: accessors return what the constructors were given"
    (define tx
      (eip1559-transaction #:chain-id 5 #:nonce 6 #:max-priority-fee-per-gas 7
                           #:max-fee-per-gas 8 #:gas-limit 9
                           #:to (string-downcase anvil-bob) #:value 10 #:data #"\1\2"
                           #:access-list
                           (list (list (string-downcase anvil-bob) (make-bytes 32 3)))))
    (check-equal? (list (transaction-type tx) (transaction-chain-id tx) (transaction-nonce tx)
                        (transaction-max-priority-fee-per-gas tx) (transaction-max-fee-per-gas tx)
                        (transaction-gas-limit tx) (transaction-to tx) (transaction-value tx)
                        (transaction-data tx) (transaction-access-list tx))
                  (list 2 5 6 7 8 9 anvil-bob 10 #"\1\2" (list (list anvil-bob (make-bytes 32 3)))))
    (define legacy
      (legacy-transaction #:chain-id #f #:nonce 1 #:gas-price 2 #:gas-limit 3 #:to #f))
    (check-equal? (list (transaction-type legacy) (transaction-chain-id legacy)
                        (transaction-gas-price legacy) (transaction-to legacy)
                        (transaction-value legacy) (transaction-data legacy))
                  (list 0 #f 2 #f 0 #""))
    (define eip2930
      (eip2930-transaction #:chain-id 1 #:nonce 1 #:gas-price 2 #:gas-limit 3 #:to anvil-bob))
    (check-equal? (list (transaction-type eip2930) (transaction-gas-price eip2930)
                        (transaction-access-list eip2930) (transaction-value eip2930)
                        (transaction-data eip2930))
                  (list 1 2 '() 0 #""))
    (check-equal? (transaction-access-list (transaction-of 'eip1559 (hasheq))) '())
    (for ([tx (in-list edge-transactions)])
      (define to (transaction-to tx))
      (check-true (or (not to) (equal? to (address->checksum-string to))))))

  (test-case "R5: sign-transaction signs the signing hash"
    (for ([pair (in-list (take model-cases 60))])
      (match-define (cons tx key) pair)
      (define stx (sign-transaction key tx))
      (check-equal? (signed-transaction tx (sign-digest key (transaction-signing-hash tx))) stx)
      (check-equal? (signed-transaction tx (signed-transaction-signature stx)) stx)))

  (define (syntax-error form)
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'web3/transaction)
      (with-handlers ([exn:fail:syntax? values])
        (eval form)
        #f)))

  (test-case "R6: signed-transaction is a pattern, and no struct type is exported"
    (define stx (sign-transaction example-key example-eip1559))
    (match stx
      [(signed-transaction tx sig)
       (check-equal? tx example-eip1559)
       (check-equal? sig (signed-transaction-signature stx))])
    (for ([form (in-list '((struct-copy signed-transaction #f)
                           (struct forged signed-transaction ())
                           (struct-copy legacy-transaction #f)
                           (struct-copy eip2930-transaction #f)
                           (struct-copy eip1559-transaction #f)
                           (struct forged eip1559-transaction ())))])
      (check-pred exn:fail:syntax? (syntax-error form) (format "~s" form))))

  (define signing-cases
    (let ([prng (seeded 1 9 7 3 1 1)])
      (for*/list ([kind (in-list '(pre-155 legacy eip2930 eip1559))]
                  [key (in-sequences (in-list edge-keys)
                                     (for/list ([_ (in-range 25)]) (random-key prng)))])
        (cons key (random-transaction prng kind)))))

  (test-case "S1-S3: the sender is the key's address, s is low, and signing is deterministic"
    (for ([pair (in-list signing-cases)])
      (match-define (cons key tx) pair)
      (define stx (sign-transaction key tx))
      (check-equal? (signed-transaction-sender stx) (secret-key->address key))
      (check-true (signature-low-s? (signed-transaction-signature stx)))
      (check-equal? (sign-transaction key tx) stx)))

  (define (variant fields name)
    (define value (hash-ref fields name))
    (define priority (hash-ref fields 'max-priority-fee-per-gas #f))
    (define max-fee (hash-ref fields 'max-fee-per-gas #f))
    (define (with new) (hash-set fields name new))
    (match name
      ['chain-id #:when (not value) #f]
      ['to (with (if value #f anvil-bob))]
      ['data (with (bytes-append value #"\0"))]
      ['access-list (with (cons (list anvil-bob) value))]
      ['max-priority-fee-per-gas #:when (< value max-fee) (with (add1 value))]
      ['max-fee-per-gas #:when (> value priority) (with (sub1 value))]
      ['max-fee-per-gas #:when (< value (sub1 (expt 2 256))) (with (add1 value))]
      ['max-fee-per-gas #f]
      [_ #:when (positive? value) (with (sub1 value))]
      ['max-priority-fee-per-gas #f]
      [_ (with 1)]))

  (test-case "S4: every field is signed, the chain id included"
    (for ([pair (in-list signing-cases)])
      (match-define (cons _ tx) pair)
      (define kind (transaction-kind tx))
      (define fields (transaction-fields tx))
      (for ([name (in-list (hash-keys fields))])
        (define other (variant fields name))
        (when other
          (check-not-equal? (transaction-signing-hash (transaction-of kind other))
                            (transaction-signing-hash tx)
                            (format "~a ~a" kind name))))
      (when (eq? kind 'legacy)
        (check-not-equal? (transaction-signing-hash (transaction-of 'pre-155 fields))
                          (transaction-signing-hash tx))))
    (define on-1 (transaction-of 'eip1559 (hasheq 'chain-id 1)))
    (define on-2 (transaction-of 'eip1559 (hasheq 'chain-id 2)))
    (define sig (signed-transaction-signature (sign-transaction example-key on-1)))
    (check-not-equal? (signed-transaction-sender (signed-transaction on-2 sig))
                      (secret-key->address example-key)))

  (define (signature-message thunk)
    (with-handlers ([exn:fail:web3:signature? exn-message])
      (thunk)
      #f))

  (test-case "S5: a high s or a signature with no sender is refused"
    (define tx example-eip1559)
    (define sig (signed-transaction-signature (sign-transaction example-key tx)))
    (check-equal? (signature-message (lambda () (signed-transaction tx (other-s sig))))
                  "signed-transaction: s is more than half the curve order (EIP-2)")
    (define no-point (signature (no-point-r) 1 0))
    (check-equal? (signature-message (lambda () (signed-transaction tx no-point)))
                  "signed-transaction: the signature recovers no address"))

  (define u64-bad (list (expt 2 64) -1 1.0 "1"))
  (define u256-bad (list (expt 2 256) -1))
  (define to-bad
    (list (string-append "0x" (make-string 39 #\a)) (string-append "0x" (make-string 41 #\a))
          (string-append "0X" (make-string 40 #\a)) (make-bytes 20 1) 'none))
  (define key-32 (make-bytes 32 1))
  (define access-list-bad
    (list (list (list (string-append "0x" (make-string 38 #\a))))
          (list (list anvil-bob (make-bytes 31 1)))
          (list (list anvil-bob (make-bytes 33 1)))
          (list (list anvil-bob (bytes->hex-string key-32)))
          (vector (list anvil-bob key-32))))

  (define (constructor kind)
    (match kind
      ['legacy legacy-transaction]
      ['eip2930 eip2930-transaction]
      ['eip1559 eip1559-transaction]))

  (define kind-keywords
    (hasheq 'legacy '(#:chain-id #:gas-limit #:gas-price #:nonce #:to)
            'eip2930 '(#:chain-id #:gas-limit #:gas-price #:nonce #:to)
            'eip1559 '(#:chain-id #:gas-limit #:max-fee-per-gas #:max-priority-fee-per-gas
                       #:nonce #:to)))

  (define (arguments kind overrides)
    (define fields
      (for/fold ([fields (hasheq '#:chain-id 1 '#:nonce 0 '#:gas-price 0 '#:gas-limit 21000
                                 '#:to anvil-bob '#:max-priority-fee-per-gas 0
                                 '#:max-fee-per-gas 0)])
                ([(k v) (in-hash overrides)])
        (hash-set fields k v)))
    (define keywords
      (sort (remove-duplicates (append (hash-ref kind-keywords kind) (hash-keys overrides)))
            keyword<?))
    (values keywords (for/list ([k (in-list keywords)]) (hash-ref fields k))))

  (define (construct kind overrides)
    (define-values (keywords arguments-given) (arguments kind overrides))
    (keyword-apply (constructor kind) keywords arguments-given '()))

  (define blame-rows
    (append
     (for*/list ([kind (in-list '(legacy eip2930 eip1559))]
                 [k (in-list '(#:nonce #:gas-limit #:chain-id))]
                 [v (in-list u64-bad)])
       (list kind k v))
     (for*/list ([kind (in-list '(legacy eip2930))]
                 [k (in-list '(#:gas-price #:value))]
                 [v (in-list u256-bad)])
       (list kind k v))
     (for*/list ([k (in-list '(#:max-priority-fee-per-gas #:max-fee-per-gas #:value))]
                 [v (in-list u256-bad)])
       (list 'eip1559 k v))
     (list (list 'eip2930 '#:chain-id #f) (list 'eip1559 '#:chain-id #f))
     (for*/list ([kind (in-list '(legacy eip2930 eip1559))]
                 [v (in-list to-bad)])
       (list kind '#:to v))
     (for/list ([kind (in-list '(legacy eip2930 eip1559))])
       (list kind '#:data "0x"))
     (for*/list ([kind (in-list '(eip2930 eip1559))]
                 [v (in-list access-list-bad)])
       (list kind '#:access-list v))))

  (test-case "B1: a field outside its range blames the caller"
    (for ([row (in-list blame-rows)])
      (match-define (list kind k v) row)
      (check-blames-here (construct kind (hasheq k v))))
    (check-blames-here (eip1559-transaction #:chain-id 1 #:nonce 0 #:max-priority-fee-per-gas 2
                                            #:max-fee-per-gas 1 #:gas-limit 21000 #:to #f))
    (for ([kind (in-list '(eip2930 eip1559))])
      (check-equal? (transaction-access-list
                     (construct kind (hasheq '#:access-list (list (list anvil-bob)))))
                    (list (list anvil-bob)))))

  (define (keyword-error? kind keyword)
    (lambda (e)
      (and (exn:fail:contract? e)
           (regexp-match? (regexp-quote (symbol->string (object-name (constructor kind))))
                          (exn-message e))
           (regexp-match? (regexp-quote (keyword->string keyword)) (exn-message e)))))

  (test-case "B2: arity and keyword errors name the procedure and the keyword"
    (check-exn exn:fail:contract:arity? (lambda () (eip1559-transaction 1)))
    (for ([kind (in-list '(legacy eip2930 eip1559))])
      (define-values (keywords arguments-given) (arguments kind (hasheq)))
      (for ([k (in-list keywords)])
        (define-values (kept kept-values)
          (for/lists (ks vs) ([key (in-list keywords)] [v (in-list arguments-given)]
                              #:unless (eq? key k))
            (values key v)))
        (check-exn (keyword-error? kind k)
                   (lambda () (keyword-apply (constructor kind) kept kept-values '()))
                   (format "~a without ~a" kind k)))
      (check-exn (keyword-error? kind '#:gas)
                 (lambda () (construct kind (hasheq '#:gas 1))))))

  (test-case "B6: uint64/c and uint256/c accept the naturals below 2^64 and 2^256"
    (for ([row (in-list `((,uint64/c 64) (,uint256/c 256)))])
      (match-define (list range/c bits) row)
      (define accepts? (flat-contract-predicate range/c))
      (for ([v (in-list (list 0 1 (sub1 (expt 2 bits))))])
        (check-true (accepts? v) (format "~a ~a" bits v)))
      (for ([v (in-list (list -1 (expt 2 bits) 1/2 1.0 "1" #f))])
        (check-false (accepts? v) (format "~a ~s" bits v)))))

  (test-case "B7: procedures and their arity errors carry their public names"
    (define names
      '(signed-transaction sign-transaction legacy-transaction eip2930-transaction
                           eip1559-transaction bytes->signed-transaction))
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'web3/transaction)
      (for ([name (in-list names)])
        (define procedure (eval name))
        (check-equal? (object-name procedure) name)
        (check-exn (regexp (string-append "^" (regexp-quote (symbol->string name)) ": "))
                   (lambda () (eval `(,name 1 2 3 4 5 6 7)))
                   (symbol->string name)))))

  (define accessors
    (list transaction-type transaction-chain-id transaction-nonce transaction-gas-price
          transaction-max-priority-fee-per-gas transaction-max-fee-per-gas
          transaction-gas-limit transaction-to transaction-value transaction-data
          transaction-access-list transaction->bytes transaction-signing-hash))

  (test-case "B3: accessors blame callers that give another kind"
    (define stx (sign-transaction example-key example-eip1559))
    (check-blames-here (transaction-gas-price example-eip1559))
    (check-blames-here (transaction-max-fee-per-gas example-legacy))
    (check-blames-here (transaction-max-priority-fee-per-gas example-legacy))
    (check-blames-here (transaction-access-list example-legacy))
    (for ([accessor (in-list accessors)])
      (check-blames-here (accessor stx))
      (check-blames-here (accessor 'tx)))
    (check-blames-here (signed-transaction-sender example-eip1559))
    (check-blames-here (signed-transaction-hash 'tx))
    (check-blames-here (signed-transaction->bytes example-eip1559))
    (check-blames-here (bytes->signed-transaction "0x02"))
    (check-exn exn:fail:contract:arity? (lambda () (transaction-nonce)))
    (check-exn exn:fail:contract:arity?
               (lambda () (transaction-nonce example-legacy example-legacy)))
    (check-exn exn:fail:contract:arity? (lambda () (signed-transaction-hash stx stx))))

  (define marker-key (integer->secret-key marker-integer))

  (define (blame-message thunk)
    (with-handlers ([exn:fail:contract? exn-message])
      (thunk)
      #f))

  (test-case "B4: sign-transaction blames its caller and never shows the key"
    (define tx example-eip1559)
    (check-blames-here (sign-transaction marker-key 'tx))
    (check-blames-here (sign-transaction tx marker-key))
    (check-blames-here (sign-transaction marker-key))
    (check-blames-here (sign-transaction marker-key tx 1))
    (check-blames-here (sign-transaction marker-key tx #:chain-id 1))
    (for ([thunk (in-list (list (lambda () (sign-transaction marker-key 'tx))
                                (lambda () (sign-transaction tx marker-key))
                                (lambda () (sign-transaction marker-key))
                                (lambda () (sign-transaction marker-key tx 1))
                                (lambda () (sign-transaction marker-key tx #:chain-id 1))))])
      (check-no-leak (blame-message thunk)))
    (check-no-leak (format "~s" (sign-transaction marker-key tx))))

  (test-case "B5: values share no storage with their arguments or results"
    (define data (bytes 1 2 3))
    (define key (make-bytes 32 7))
    (define tx
      (transaction-of 'eip1559 (hasheq 'data data 'access-list (list (list anvil-bob key)))))
    (bytes-fill! data 0)
    (bytes-fill! key 0)
    (check-equal? (transaction-data tx) (bytes 1 2 3))
    (check-equal? (transaction-access-list tx) (list (list anvil-bob (make-bytes 32 7))))
    (check-true (immutable? (transaction-data tx)))
    (for ([entry (in-list (transaction-access-list tx))])
      (match-define (cons _ keys) entry)
      (for ([k (in-list keys)])
        (check-true (immutable? k))))
    (define stx (sign-transaction example-key tx))
    (define raw (signed-transaction->bytes stx))
    (bytes-fill! raw 0)
    (check-not-equal? (signed-transaction->bytes stx) raw)
    (define unsigned (transaction->bytes tx))
    (bytes-fill! unsigned 0)
    (check-not-equal? (transaction->bytes tx) unsigned))

  (test-case "D1: web3/transaction needs no network, JSON, node or units modules"
    (define names (filter values (map module-name (import-closure 'web3/transaction))))
    (check-equal? (filter (lambda (n) (member (collection n) '("net" "json"))) names) '())
    (check-equal? (filter (lambda (n) (member n '("web3/rpc" "web3/eth" "web3/contract"
                                                  "web3/units")))
                          names)
                  '()))

  (define (exported-names module)
    (module-declared? module #t)
    (define-values (variables syntax) (module->exports module))
    (for*/list ([phase+names (in-list (append variables syntax))]
                [name (in-list (match phase+names
                                 [(cons 0 names) names]
                                 [_ '()]))])
      (match name
        [(cons symbol _) symbol])))

  (test-case "D2: web3 provides every name web3/transaction does"
    (define all (exported-names 'web3))
    (for ([name (in-list (exported-names 'web3/transaction))])
      (check-not-false (memq name all) (format "~a" name)))
    (check-not-exn (lambda ()
                     (parameterize ([current-namespace (make-base-namespace)])
                       (namespace-require 'web3)))))

  (test-case "D3: the contract-call module has its new name"
    (check-true (file-exists? (collection-file-path "contract-call.rkt" "web3" "private")))
    (check-false (file-exists? (collection-file-path "transaction.rkt" "web3" "private"))))

  (test-case "D4: web3 and web3/transaction's internal submodule can be required together"
    (check-not-exn
     (lambda ()
       (parameterize ([current-namespace (make-base-namespace)])
         (expand '(module both racket/base
                    (require web3 (submod web3/transaction internal)))))))))
