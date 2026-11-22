#lang racket/base

(module+ test
  (require (only-in racket/contract
                    ->
                    ->i
                    any
                    any/c
                    blame-positive
                    contract
                    contract-first-order
                    exn:fail:contract:blame-object
                    exn:fail:contract:blame?
                    listof
                    or/c)
           (only-in racket/list drop-right last make-list)
           (only-in racket/match match-define)
           racket/string
           rackunit
           (only-in libsecp256k1 integer->secret-key)
           web3
           (only-in web3/private/argument address/c hash-argument/c)
           (only-in "helpers/accounts.rkt" anvil-keys)
           (only-in "util.rkt" anvil-alice anvil-bob))

  (define typo "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD")
  (define checksum-mismatch "mixed case that is not its EIP-55 checksum")
  (define checksum-reason (string-append "not an address: " checksum-mismatch))
  (define sender-checksum-reason (string-append "not an address or a signer: " checksum-mismatch))

  (define-contract c "function f(uint256 a) view returns (uint256)")

  (define (refusal thunk)
    (with-handlers ([exn:fail:contract:blame? values])
      (thunk)
      #f))

  (struct row (label thunk lines) #:transparent)

  (define (quoted v)
    (format "given: ~s" v))

  (define rows
    (list
     (row "a positional address on an endpoint"
          (lambda () (eth-get-balance typo "latest"))
          `(,(string-append "eth-get-balance: " checksum-reason) "argument: 1st" ,(quoted typo)))
     (row "send-transaction's to"
          (lambda () (send-transaction anvil-alice typo #:value 1))
          `(,(string-append "send-transaction: " checksum-reason) "argument: 2nd" ,(quoted typo)))
     (row "send-transaction's from"
          (lambda () (send-transaction typo anvil-bob #:value 1))
          `(,(string-append "send-transaction: " checksum-reason) "argument: 1st" ,(quoted typo)))
     (row "contract-call's #:from"
          (lambda () (contract-call anvil-bob "f()()" #:from typo))
          `(,(string-append "contract-call: " sender-checksum-reason)
            "argument: #:from" ,(quoted typo)))
     (row "contract-send's #:from"
          (lambda () (contract-send anvil-bob "f()" #:from typo))
          `(,(string-append "contract-send: " sender-checksum-reason)
            "argument: #:from" ,(quoted typo)))
     (row "contract-call's target, counted like any positional argument"
          (lambda () (contract-call typo "f()()"))
          `(,(string-append "contract-call: not an address or a contract instance: "
                            checksum-mismatch)
            "argument: 1st" ,(quoted typo)))
     (row "a secret key where fill-transaction's sender goes"
          (lambda () (fill-transaction (integer->secret-key 7) anvil-bob))
          '("fill-transaction: not an address or a signer: neither a string nor a signer"
            "argument: 1st" "given: a secret key"))
     (row "a secret key where contract-call's #:from goes"
          (lambda () (contract-call anvil-bob "f()()" #:from (integer->secret-key 7)))
          '("contract-call: not an address or a signer: neither a string nor a signer"
            "argument: #:from" "given: a secret key"))
     (row "a bound procedure's address"
          (lambda () (c-f typo 1))
          `(,(string-append "c-f: " checksum-reason) "argument: 1st" ,(quoted typo)))
     (row "a bound procedure's #:from"
          (lambda () (c-f anvil-bob 1 #:from typo))
          `(,(string-append "c-f: " sender-checksum-reason) "argument: #:from" ,(quoted typo)))
     (row "a transaction object's to"
          (lambda () (eth-call (hasheq 'to typo) "latest"))
          `(,(string-append "eth-call: " checksum-reason)
            "argument: 1st" "field: to" ,(quoted typo)))
     (row "a filter object's address"
          (lambda () (eth-get-logs (hasheq 'address typo)))
          `(,(string-append "eth-get-logs: not an address or a list of addresses: "
                            checksum-mismatch)
            "argument: 1st" "field: address" ,(quoted typo)))
     (row "a transaction constructor's #:to, which may be #f"
          (lambda ()
            (eip1559-transaction #:chain-id 1 #:nonce 0 #:max-priority-fee-per-gas 1
                                 #:max-fee-per-gas 2 #:gas-limit 21000 #:to typo))
          `(,(string-append "eip1559-transaction: " checksum-reason)
            "argument: #:to" ,(quoted typo)))
     (row "a log filter's addresses"
          (lambda () (log-filter #:address (list anvil-bob typo)))
          `(,(string-append "log-filter: " checksum-reason)
            "argument: #:address" "in: an element of" ,(quoted typo)))
     (row "a log filter's empty list of addresses"
          (lambda () (log-filter #:address '()))
          '("log-filter: not an address or a non-empty list of addresses: the list is empty"
            "argument: #:address" "given: an empty list"))
     (row "a hash argument"
          (lambda () (eth-get-transaction-receipt "0xaa"))
          '("eth-get-transaction-receipt: not a hash: not 0x and 64 hex digits"
            "argument: 1st" "given: \"0xaa\""))
     (row "a block hash, which may be #f"
          (lambda () (log-filter #:block-hash "0xaa"))
          '("log-filter: not a hash: not 0x and 64 hex digits"
            "argument: #:block-hash" "given: \"0xaa\""))))

  (define (check-short-refusal e lines label)
    (check-pred exn:fail:contract:blame? e label)
    (when e
      (define message (exn-message e))
      (define got (string-split message "\n"))
      (check-equal? (car got) (car lines) label)
      (check-equal? (map string-trim (drop-right (cdr got) 1)) (cdr lines) message)
      (check-regexp-match #rx"^  blaming: [^\n]*tests/refusals[.]rkt" (last got))
      (check-true (<= (length got) 6) message)
      (check-false (regexp-match? #rx"->|contract from|expected:" message) message)))

  (test-case "a refused address or hash says who, which argument, what and why, in a few lines"
    (for ([r (in-list rows)])
      (match-define (row label thunk lines) r)
      (check-short-refusal (refusal thunk) lines label)))

  (test-case "the three reasons an address is refused read differently"
    (define reasons
      (for/list ([bad (in-list (list 'alice "0x12" typo))])
        (car (string-split (exn-message (refusal (lambda () (eth-get-balance bad "latest"))))
                           "\n"))))
    (check-equal? reasons
                  (list "eth-get-balance: not an address: not a string"
                        "eth-get-balance: not an address: not 0x and 40 hex digits"
                        (string-append "eth-get-balance: " checksum-reason))))

  (test-case "the refusal blames the caller, not web3"
    (define e (refusal (lambda () (eth-get-balance typo "latest"))))
    (define party (format "~a" (blame-positive (exn:fail:contract:blame-object e))))
    (check-regexp-match #rx"tests/refusals[.]rkt" party)
    (check-false (regexp-match? #rx"eth[.]rkt" party)))

  (define key (car anvil-keys))
  (define digits (substring key 2))

  (define (check-described thunk given)
    (define e (refusal thunk))
    (check-pred exn:fail:contract:blame? e given)
    (when e
      (define message (exn-message e))
      (check-false (string-contains? (string-downcase message) digits) message)
      (check-regexp-match (regexp (string-append "\n  given: " given "\n")) message)))

  (test-case "what could be a secret key is described, never shown"
    (define not-hashes
      (list (cons digits "a string of 64 characters")
            (cons (string->bytes/latin-1 digits) "a byte string of length 64")
            (cons (integer->secret-key 7) "a secret key")))
    (for ([bad+given (in-list (cons (cons key "a string of 66 characters") not-hashes))])
      (match-define (cons bad given) bad+given)
      (check-described (lambda () (eth-get-balance bad "latest")) given)
      (check-described (lambda () (contract-call anvil-bob "f()()" #:from bad)) given))
    (for ([bad+given (in-list not-hashes)])
      (match-define (cons bad given) bad+given)
      (check-described (lambda () (eth-get-transaction-receipt bad)) given)))

  (define mnemonic "test test test test test test test test test test test junk")
  (define secret-texts
    (list (cons (substring key 0 65) "a string of 65 characters")
          (cons (string-append (substring key 0 34) " " (substring key 34))
                "a string of 67 characters")
          (cons mnemonic (format "a string of ~a characters" (string-length mnemonic)))))

  (define (check-hidden thunk secret given)
    (define e (refusal thunk))
    (check-pred exn:fail:contract:blame? e given)
    (when e
      (define message (exn-message e))
      (check-false (string-contains? message secret) message)
      (check-false (regexp-match? #px"[0-9a-fA-F]{20}|test test" message) message)
      (check-regexp-match (regexp (string-append "\n  given: " given "\n")) message)))

  (test-case "text that is not 0x and at most 40 hex digits is described, never shown"
    (for ([secret+given (in-list secret-texts)])
      (match-define (cons secret given) secret+given)
      (check-hidden (lambda () (send-transaction secret anvil-bob #:value 1)) secret given)
      (check-hidden (lambda () (fill-transaction secret anvil-bob)) secret given)
      (check-hidden (lambda () (contract-call anvil-bob "f()()" #:from secret)) secret given)
      (check-hidden (lambda () (c-f anvil-bob 1 #:from secret)) secret given)
      (check-hidden (lambda () (eth-get-transaction-receipt secret)) secret given)))

  (define key-text (string-append "0x" (make-string 60 #\0) "beef"))

  (define key-text-rows
    (list
     (row "a key in a transaction object that is not JSON"
          (lambda () (eth-call (hasheq 'from key-text 'data #"\1") "latest"))
          '("eth-call: not an address: not 0x and 40 hex digits"
            "argument: 1st" "field: from" "given: a string of 66 characters"))
     (row "a key where a transaction object goes"
          (lambda () (eth-send-transaction key-text))
          '("eth-send-transaction: not a transaction object: not a hash table"
            "argument: 1st" "given: a string of 66 characters"))
     (row "a key in a transaction object with string keys"
          (lambda () (eth-call (hash "from" anvil-bob "note" key-text) "latest"))
          '("eth-call: not a transaction object: a key that is not a symbol"
            "argument: 1st" "given: a hash table"))
     (row "a key in a filter object with a field that is not JSON"
          (lambda () (eth-get-logs (hasheq 'address anvil-bob 'x key-text 'y 'sym)))
          '("eth-get-logs: not a filter object: its y field is not JSON"
            "argument: 1st" "given: a hash table"))
     (row "a key where a filter's topics go"
          (lambda () (log-filter #:topics key-text))
          '("log-filter: not a list of topic positions: not a list"
            "argument: #:topics" "given: a string of 66 characters"))
     (row "a symbol where a filter's topics go"
          (lambda () (log-filter #:topics 'topics))
          '("log-filter: not a list of topic positions: not a list"
            "argument: #:topics" "given: a symbol"))
     (row "five topic positions"
          (lambda () (log-filter #:topics (make-list 5 #f)))
          '("log-filter: not a list of topic positions: more than 4 elements"
            "argument: #:topics" "given: a list of 5 elements"))))

  (test-case "a refused object or topic list is described in a few lines, its contents never shown"
    (for ([r (in-list key-text-rows)])
      (match-define (row label thunk lines) r)
      (define e (refusal thunk))
      (check-short-refusal e lines label)
      (when e
        (check-false (string-contains? (exn-message e) "beef") (exn-message e))
        (check-false (regexp-match? #px"[0-9a-fA-F]{20}" (exn-message e)) (exn-message e)))))

  (test-case "a key named in a refused object is one that could not be hex"
    (for ([key (in-list (list (string->symbol (substring key-text 2)) 'deadbeef '|0x1| 'x-y))])
      (define e (refusal (lambda () (eth-call (hasheq key 'sym) "latest"))))
      (check-equal? (car (string-split (exn-message e) "\n"))
                    "eth-call: not a transaction object: a field that is not JSON"
                    (format "~s" key))))

  (test-case "a program that never loads private/opaque.rkt still sees a key described, not shown"
    (define program
      '((require (only-in libsecp256k1 integer->secret-key) web3/abi/event)
        (define opaque-before? (module-declared? 'web3/private/opaque #f))
        (define message
          (with-handlers ([exn:fail? exn-message])
            (indexed-hash (integer->secret-key 7))))
        (require web3/account)
        (list opaque-before? (module-declared? 'web3/private/opaque #f) message)))
    (match-define (list opaque-before? opaque-after? message)
      (parameterize ([current-namespace (make-base-namespace)])
        (for/last ([form (in-list program)])
          (eval form))))
    (check-equal? (list opaque-before? opaque-after?) '(#f #t))
    (check-not-false (member "  given: a value of another kind" (string-split message "\n"))
                     message)
    (check-false (regexp-match? #px"[0-9a-fA-F]{20}|secret" message) message))

  (test-case "a handler that runs while a refusal is raised sees Racket's own format"
    (define integer-only (contract (-> integer? any) values 'web3 'caller 'integer-only #f))
    (define seen #f)
    (with-handlers ([exn:fail:contract:blame? void])
      (call-with-exception-handler
       (lambda (e)
         (set! seen (with-handlers ([exn:fail:contract:blame? exn-message]) (integer-only 'x)))
         e)
       (lambda () (eth-get-balance typo "latest"))))
    (check-regexp-match #rx"^integer-only: contract violation\n  expected: integer[?]\n" seen)
    (check-equal? (length (regexp-match* #rx"given:" seen)) 1 seen))

  (test-case "an ->i argument is labelled by its position or keyword in that ->i alone"
    (define (labels thunk)
      (for/list ([line (in-list (string-split (exn-message (refusal thunk)) "\n"))]
                 #:when (regexp-match? #rx"^  (argument|in):" line))
        (string-trim line)))
    (define positional
      (contract (->i ([a any/c] [b address/c]) ([c address/c] #:d [d address/c]) any)
                (lambda (a b [c #f] #:d [d #f]) a) 'web3 'caller 'positional #f))
    (check-equal? (labels (lambda () (positional 1 typo))) '("argument: 2nd"))
    (check-equal? (labels (lambda () (positional 1 anvil-bob typo))) '("argument: 3rd"))
    (check-equal? (labels (lambda () (positional 1 anvil-bob #:d typo))) '("argument: #:d"))
    (define nested
      (contract (->i ([g (->i ([x address/c]) any)]) (#:size [x integer?]) any)
                (lambda (g #:size [x 0]) (g typo)) 'web3 'caller 'nested #f))
    (check-equal? (labels (lambda () (nested values))) '("argument: 1st" "argument: x")))

  (test-case "address/c keeps address? as its first-order check, so or/c picks properly"
    (define first-order (contract-first-order address/c))
    (for ([v (list anvil-bob (string-downcase anvil-bob) typo "0x12" 'alice)])
      (check-equal? (first-order v) (address? v) (format "~s" v)))
    (define maybe (contract (or/c #f address/c) #f 'web3 'caller))
    (check-false maybe)
    (check-equal? (contract (or/c #f address/c) anvil-bob 'web3 'caller) anvil-bob)
    (define e (refusal (lambda () (contract (or/c #f address/c) typo 'web3 'caller 'to #f))))
    (check-equal? (string-split (exn-message e) "\n")
                  (list (string-append "to: " checksum-reason)
                        (string-append "  " (quoted typo))
                        "  blaming: web3"))
    (define listed
      (refusal (lambda () (contract (listof hash-argument/c) '("0x1") 'web3 'caller 'topics #f))))
    (check-equal? (string-split (exn-message listed) "\n")
                  '("topics: not a hash: not 0x and 64 hex digits"
                    "  in: an element of"
                    "  given: \"0x1\""
                    "  blaming: web3"))))
