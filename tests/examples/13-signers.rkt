#lang racket/base

(module+ test
  (require racket/port
           (only-in racket/string string-append* string-join)
           rackunit
           (only-in web3 current-anvil-path)
           (only-in web3/rpc hex-string->bytes)
           (only-in "../helpers/secrets.rkt" check-no-leak))

  (define key
    (hex-string->bytes "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))

  (define signature
    (string-append* '("0xf16ea9a3478698f695fd1401bfe27e9e4a7e8e3da94aa72b021125e31fa899cc573c4"
                      "8ea3fe1d4ab61a9db10c19032026e3ed2dbccba5a178235ac27f94504311c")))

  (define (signer-lines name digest)
    `(,(format "#<~a 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266>" name)
      ,(format "  digest      ~a" digest)
      ,(format "  message     ~a" signature)
      "  recovered   0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"
      "  verified    #t"
      "  transaction 0x6e6196f107bb02ceffd0db99a2c3be3053f8d1917061e67f88c7843aa77833e2"))

  (define expected
    `("digest 0x50b2c43fd39106bafbba0da34fc430e1f91e3c96ea2acee2bc34119f92b37750"
      ,@(signer-lines "local-signer" signature)
      ,@(signer-lines "node-signer" "unsupported")))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping example 13: anvil not found")]
    [else
     (define output
       (with-output-to-string
         (lambda () (dynamic-require '(submod web3/examples/13-signers main) #f))))

     (test-case "X1: main prints the same signatures and transaction for both signers"
       (check-equal? output (string-append (string-join expected "\n") "\n")))

     (test-case "X2: the output shows no part of the key"
       (check-no-leak output #:secret key))]))
