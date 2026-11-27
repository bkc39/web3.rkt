#lang racket/base

(module+ test
  (require (only-in racket/file file->string)
           racket/runtime-path
           (only-in racket/string string-contains? string-split)
           rackunit
           (only-in web3 bytes->hex-string hex-string->bytes)
           (only-in "../helpers/examples.rkt" example-output example-tests)
           (only-in "../helpers/programs.rkt" echo-init echo-runtime)
           (only-in "../helpers/secrets.rkt" check-no-leak))

  (define-runtime-path program "../../examples/16-raw-contract.rkt")

  (define key
    (hex-string->bytes "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))

  (define creation-prefix "0x602280600b6000396000f3")

  (define expected
    `("deployed 0x5FbDB2315678afecb367f032d93F642f64180aa3"
      "call 0x000000000000000000000000000000000000000000000000000000000000002a"
      "revert 0xff0000000000000000000000000000000000000000000000000000000000002a"
      "halt invalid-opcode"
      ,(string-append "trace CALL output"
                      " 0x000000000000000000000000000000000000000000000000000000000000002a"
                      " gas used 22204")
      "gas estimate 22204 intrinsic 21140 floor 21350 used 22204"))

  (test-case "X1: the program states the tests' echo contract, runtime and creation prefix"
    (define source (file->string program))
    (check-equal? echo-init (bytes-append (hex-string->bytes creation-prefix) echo-runtime))
    (check-true (string-contains? source (format "~s" (bytes->hex-string echo-runtime))))
    (check-true (string-contains? source (format "~s" creation-prefix))))

  (example-tests "16-raw-contract" #:anvil
    (define output (example-output "16-raw-contract"))

    (test-case "X2: hand-assembled code deploys, echoes, reverts, halts, traces and estimates"
      (check-equal? (string-split output "\n") expected))

    (test-case "X3: the output shows no part of the key"
      (check-no-leak output #:secret key))))
