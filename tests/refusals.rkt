#lang racket/base

(module+ test
  (require (only-in racket/contract
                    blame-positive
                    contract
                    contract-first-order
                    exn:fail:contract:blame-object
                    exn:fail:contract:blame?
                    listof
                    or/c)
           (only-in racket/list drop-right last)
           (only-in racket/match match-define)
           racket/string
           rackunit
           (only-in libsecp256k1 integer->secret-key)
           web3
           (only-in web3/private/argument address/c hash-argument/c)
           (only-in "helpers/accounts.rkt" anvil-keys)
           (only-in "util.rkt" anvil-alice anvil-bob))

  (define typo "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD")
  (define checksum-reason "not an address: mixed case that is not its EIP-55 checksum")

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
          `(,(string-append "contract-call: " checksum-reason) "argument: #:from" ,(quoted typo)))
     (row "contract-send's #:from"
          (lambda () (contract-send anvil-bob "f()" #:from typo))
          `(,(string-append "contract-send: " checksum-reason) "argument: #:from" ,(quoted typo)))
     (row "a bound procedure's address"
          (lambda () (c-f typo 1))
          `(,(string-append "c-f: " checksum-reason) "argument: 1st" ,(quoted typo)))
     (row "a bound procedure's #:from"
          (lambda () (c-f anvil-bob 1 #:from typo))
          `(,(string-append "c-f: " checksum-reason) "argument: #:from" ,(quoted typo)))
     (row "a transaction object's to"
          (lambda () (eth-call (hasheq 'to typo) "latest"))
          `(,(string-append "eth-call: " checksum-reason)
            "argument: 1st" "field: to" ,(quoted typo)))
     (row "a filter object's address"
          (lambda () (eth-get-logs (hasheq 'address typo)))
          `(,(string-append "eth-get-logs: " checksum-reason)
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
            (cons (integer->secret-key 7) "a value of another kind")))
    (for ([bad+given (in-list (cons (cons key "a string of 66 characters") not-hashes))])
      (match-define (cons bad given) bad+given)
      (check-described (lambda () (eth-get-balance bad "latest")) given)
      (check-described (lambda () (contract-call anvil-bob "f()()" #:from bad)) given))
    (for ([bad+given (in-list not-hashes)])
      (match-define (cons bad given) bad+given)
      (check-described (lambda () (eth-get-transaction-receipt bad)) given)))

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
