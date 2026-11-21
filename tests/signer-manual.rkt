#lang racket/base

(module+ test
  (require rackunit
           "helpers/manual.rkt")

  (define blocks
    `(([(define key
          (hex-string->secret-key
           "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))
        (void)]
       [(define signer (local-signer key)) (void)]
       [signer (prints "#<local-signer 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266>")])
      ([(bytes->hex-string (hash-message "hello"))
        (value "0x50b2c43fd39106bafbba0da34fc430e1f91e3c96ea2acee2bc34119f92b37750")]
       [(define sig (signer-sign-message signer "hello")) (void)]
       [(bytes->hex-string (signature->bytes sig))
        (value ,(string-append
                 "0xf16ea9a3478698f695fd1401bfe27e9e4a7e8e3da94aa72b021125e31fa899cc573c48ea3fe1"
                 "d4ab61a9db10c19032026e3ed2dbccba5a178235ac27f94504311c"))]
       [(recover-message-address sig "hello") (value "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")]
       [(message-signed-by? sig "hello" (signer-address signer)) (value #t)]
       [(message-signed-by? sig "hello!" (signer-address signer)) (value #f)])
      ([(equal? (hash-message "héllo") (hash-message (string->bytes/utf-8 "héllo"))) (value #t)]
       [(equal? (hash-message "0xdeadbeef") (hash-message (bytes #xde #xad #xbe #xef)))
        (value #f)])
      ([(message-signed-by? (sign-message key "hello") "hello"
                            "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266")
        (value #t)])
      ([(define eip-155
          (legacy-transaction #:chain-id 1
                              #:nonce 9
                              #:gas-price (gwei->wei 20)
                              #:gas-limit 21000
                              #:to "0x3535353535353535353535353535353535353535"
                              #:value (ether->wei 1)))
        (void)]
       [(transaction->jsexpr eip-155 #:from "0x9d8A62f656a8d1615C1294fd71e9CFb3E4855A4F")
        (value #hasheq((type . "0x0") (from . "0x9d8A62f656a8d1615C1294fd71e9CFb3E4855A4F")
                       (to . "0x3535353535353535353535353535353535353535") (nonce . "0x9")
                       (gas . "0x5208") (gasPrice . "0x4a817c800")
                       (value . "0xde0b6b3a7640000") (data . "0x") (chainId . "0x1")))])
      ([(equal? (signer-sign-message signer "hello") (sign-message key "hello")) (value #t)])
      ([(define node-account (node-signer "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266")) (void)]
       [node-account (prints "#<node-signer 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266>")]
       [(signer-sign-digest node-account (hash-message "hello"))
        (raises "signer-sign-digest: not supported: the signer signs no digests")])
      ([(struct liar (key)
          #:methods gen:signer
          [(define (signer-address l) "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")
           (define (signer-sign-digest l digest) (sign-digest (liar-key l) digest))])
        (void)]
       [(signer-sign-message (liar key) "hello")
        (raises ,(string-append "signer-sign-message: wrong signer: the signature recovers "
                                "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266, not the signer's "
                                "address 0x70997970C51812dc3A010C7d01b50e0d17dc79C8"))])))

  (test-case "M1: the manual's signer and message examples give what the manual shows"
    (check-manual-blocks blocks)))
