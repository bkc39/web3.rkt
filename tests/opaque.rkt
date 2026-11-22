#lang racket/base

(module+ test
  (require (only-in racket/contract
                    ->
                    any/c
                    blame-positive
                    contract
                    contract-name
                    exn:fail:contract:blame-object
                    exn:fail:contract:blame?
                    listof
                    or/c)
           (only-in racket/match match)
           racket/string
           rackunit
           (only-in libsecp256k1 integer->secret-key secret-key->public-key)
           (only-in web3
                    local-signer
                    message-signed-by?
                    node-signer
                    send-transaction
                    sign-message
                    transaction->jsexpr)
           web3/address
           (only-in web3/private/argument address/c one-or-more/c)
           web3/private/opaque
           (only-in web3/private/signature signature)
           (only-in "helpers/transactions.rkt" example-eip1559))

  (define secret-text "s3cr3t-value")

  (define small/c (opaque/c 'small/c "a small thing" (lambda (v) (eqv? v 1))))

  (define (wrap c f)
    (contract c f 'provider 'caller))

  (define (refusal thunk)
    (with-handlers ([exn:fail:contract:blame? values])
      (thunk)
      #f))

  (define (field message name)
    (match (regexp-match (pregexp (string-append "(?m:^  " name ": (.*)$)")) message)
      [(list _ value) value]
      [_ #f]))

  (define (check-refusal e party expected given)
    (check-pred exn:fail:contract:blame? e)
    (when e
      (define message (exn-message e))
      (check-equal? (blame-positive (exn:fail:contract:blame-object e)) party message)
      (check-equal? (field message "(?:expected|promised)") expected message)
      (check-equal? (field message "(?:given|produced)") given message)
      (check-false (regexp-match? #rx"arguments[.][.][.]:" message) message)))

  (define one (wrap (opaque-> small/c any/c) (lambda (x) x)))

  (test-case "an opaque/c violation blames the caller and names only the kind of value"
    (for ([v (list "abc" " abc" "abc\n" #"abc" 5 'abc (integer->secret-key 7))]
          [kind (list "a string of 3 characters"
                      "a string of 4 characters, with leading or trailing whitespace"
                      "a string of 4 characters, with leading or trailing whitespace"
                      "a byte string of length 3"
                      "an exact integer"
                      "a symbol"
                      "a secret key")])
      (define e (refusal (lambda () (one v))))
      (check-refusal e 'caller "a small thing" kind)
      (when e
        (check-false (string-contains? (exn-message e) "abc") (exn-message e)))))

  (test-case "describe-kind names every kind it knows and nothing else"
    (check-equal? (map describe-kind
                       (list #t 'x 1.5 '(1) '(1 2) '() (hasheq) '(1 . 2) (integer->secret-key 1)
                             (secret-key->public-key (integer->secret-key 1)) (signature 1 1 0)
                             (local-signer (integer->secret-key 1))))
                  '("a boolean" "a symbol" "a value of another kind" "a list of 1 element"
                    "a list of 2 elements" "an empty list" "a hash table" "a value of another kind"
                    "a secret key" "a public key" "a signature" "a signer")))

  (test-case "a wrong arity names the counts, never the arguments"
    (define e (refusal (lambda () (one secret-text secret-text))))
    (check-refusal e 'caller "1 argument" "2 arguments")
    (when e
      (check-false (string-contains? (exn-message e) secret-text))))

  (test-case "an unknown keyword names the keyword, never its argument"
    (define e (refusal (lambda () (one 1 #:x secret-text))))
    (check-refusal e 'caller "no keyword arguments" "keyword #:x")
    (when e
      (check-false (string-contains? (exn-message e) secret-text))))

  (define keyed
    (wrap (opaque-> small/c #:to small/c [#:by small/c] any/c)
          (lambda (x #:to to #:by [by 1]) (list x to by))))

  (test-case "a required keyword is passed through and checked like any argument"
    (check-equal? (keyed 1 #:to 1) '(1 1 1))
    (check-equal? (keyed 1 #:to 1 #:by 1) '(1 1 1))
    (define e (refusal (lambda () (keyed 1 #:to secret-text))))
    (check-refusal e 'caller "a small thing" "a string of 12 characters")
    (when e
      (check-false (string-contains? (exn-message e) secret-text))))

  (test-case "a missing required keyword names the keyword, never the arguments"
    (define e (refusal (lambda () (keyed secret-text #:by secret-text))))
    (check-refusal e 'caller "keyword #:to" "no keyword #:to")
    (when e
      (check-false (string-contains? (exn-message e) secret-text)))
    (check-refusal (refusal (lambda () (keyed 1 #:to 1 #:from 1)))
                   'caller "keywords #:to or #:by" "keyword #:from"))

  (test-case "the name of a contract shows required keywords bare and optional ones bracketed"
    (check-equal? (contract-name (opaque-> small/c #:to small/c [#:by small/c] any/c))
                  '(opaque-> small/c #:to small/c (#:by small/c) any/c)))

  (test-case "values that meet the contracts pass through, and the name is kept"
    (define (identity x) x)
    (define wrapped (wrap (opaque-> small/c any/c) identity))
    (check-equal? (wrapped 1) 1)
    (check-equal? (object-name wrapped) 'identity))

  (test-case "a range violation blames the provider"
    (define e (refusal (lambda () ((wrap (opaque-> any/c small/c) (lambda (x) 2)) 1))))
    (check-refusal e 'provider "a small thing" "an exact integer"))

  (test-case "a plain flat contract inside opaque-> reports without the value"
    (define address (wrap (opaque-> checksum-address? any/c) (lambda (a) a)))
    (define e (refusal (lambda () (address secret-text))))
    (check-refusal e 'caller "checksum-address?" "a string of 12 characters")
    (when e
      (check-false (string-contains? (exn-message e) secret-text))))
  (test-case "an address inside opaque-> is public: its refusal shows it and says why"
    (define wrong-checksum "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD")
    (define bob "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")
    (define signed (sign-message (integer->secret-key 1) "m"))
    (for ([call (list (lambda () (send-transaction wrong-checksum bob))
                      (lambda () (node-signer wrong-checksum))
                      (lambda () (message-signed-by? signed "m" wrong-checksum))
                      (lambda () (transaction->jsexpr example-eip1559 #:from wrong-checksum)))]
          [who (list "send-transaction" "node-signer" "message-signed-by?" "transaction->jsexpr")]
          [argument (list "1st" "1st" "3rd" "#:from")])
      (define e (refusal call))
      (check-pred exn:fail:contract:blame? e who)
      (check-true (string-prefix?
                   (exn-message e)
                   (format "~a: not an address: mixed case that is not its EIP-55 checksum\n" who))
                  (exn-message e))
      (check-equal? (field (exn-message e) "argument") argument)
      (check-equal? (field (exn-message e) "given") (format "~s" wrong-checksum))))

  (test-case "opaque-> refuses a contract that is not flat unless private/argument.rkt built it"
    (define e
      (with-handlers ([exn:fail:contract? values])
        (opaque-> (or/c #f address/c) (listof address/c) any/c)))
    (check-pred exn:fail:contract? e)
    (check-false (exn:fail:contract:blame? e))
    (check-regexp-match #rx"^opaque->: a contract that is not flat" (exn-message e))
    (check-pred exn:fail:contract? (with-handlers ([exn:fail:contract? values])
                                     (opaque-> (listof (-> integer? any/c)) any/c))))

  (test-case "opaque-> keeps private/argument.rkt's contracts as they are"
    (define senders
      (contract (opaque-> address/c (one-or-more/c address/c "addresses") any/c) list
                'provider 'caller 'senders #f))
    (define bob "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")
    (check-equal? (senders bob (list bob)) (list bob (list bob)))
    (define key-hex (string-append "0x" (make-string 63 #\0) "7"))
    (define e (refusal (lambda () (senders bob key-hex))))
    (check-equal? (exn-message e)
                  (string-join '("senders: not addresses: not 0x and 40 hex digits"
                                 "  argument: 2nd"
                                 "  given: a string of 66 characters"
                                 "  blaming: caller")
                               "\n"))
    (define listed (refusal (lambda () (senders bob '("0x12")))))
    (check-equal? (exn-message listed)
                  (string-join '("senders: not an address: not 0x and 40 hex digits"
                                 "  argument: 2nd"
                                 "  in: an element of"
                                 "  given: \"0x12\""
                                 "  blaming: caller")
                               "\n")))

  (test-case "a secret key given where an address belongs is named, not shown"
    (define key (integer->secret-key 7))
    (define e (refusal (lambda () (node-signer key))))
    (check-equal? (field (exn-message e) "given") "a secret key")
    (define hex (string-append "0x" (make-string 63 #\0) "7"))
    (define text-e (refusal (lambda () (node-signer hex))))
    (check-equal? (field (exn-message text-e) "given") "a string of 66 characters")
    (check-false (string-contains? (exn-message text-e) hex))))
