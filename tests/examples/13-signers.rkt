#lang racket/base

(module+ test
  (require (only-in racket/string string-split)
           rackunit
           (only-in web3/rpc hex-string->bytes)
           (only-in "../helpers/examples.rkt" example-output example-tests)
           (only-in "../helpers/secrets.rkt" check-no-leak))

  (define key
    (hex-string->bytes "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))

  (define expected
    (for*/list ([name (in-list '("local-signer" "node-signer"))]
                [line (in-list
                       `(,(format "#<~a 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266>" name)
                         "  verified    #t"
                         ,(string-append "  transaction 0x6e6196f107bb02ceffd0db99a2c3be3053f8d1"
                                         "917061e67f88c7843aa77833e2")))])
      line))

  (example-tests "13-signers" #:anvil
    (define output (example-output "13-signers"))

    (test-case "X1: both signers' signatures verify, and they sign the same transaction"
      (check-equal? (string-split output "\n") expected))

    (test-case "X2: the output shows no part of the key"
      (check-no-leak output #:secret key))))
