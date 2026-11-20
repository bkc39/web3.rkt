#lang racket/base

(module+ test
  (require (only-in racket/contract
                    any/c
                    blame-positive
                    contract
                    exn:fail:contract:blame-object
                    exn:fail:contract:blame?)
           racket/string
           rackunit
           (only-in libsecp256k1 integer->secret-key secret-key->public-key)
           web3/address
           web3/private/opaque
           (only-in web3/private/signature signature))

  (define secret-text "s3cr3t-value")

  (define small/c (opaque/c 'small/c "a small thing" (lambda (v) (eqv? v 1))))

  (define (wrap c f)
    (contract c f 'provider 'caller))

  (define (refusal thunk)
    (with-handlers ([exn:fail:contract:blame? values])
      (thunk)
      #f))

  (define (field message name)
    (define m (regexp-match (pregexp (string-append "(?m:^  " name ": (.*)$)")) message))
    (and m (cadr m)))

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
                       (list #t 'x 1.5 '(1) (integer->secret-key 1)
                             (secret-key->public-key (integer->secret-key 1)) (signature 1 1 0)))
                  '("a boolean" "a symbol" "a value of another kind" "a value of another kind"
                    "a secret key" "a public key" "a signature")))

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
      (check-false (string-contains? (exn-message e) secret-text)))))
