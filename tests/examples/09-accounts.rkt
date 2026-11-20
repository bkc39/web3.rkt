#lang racket/base

(module+ test
  (require racket/match
           racket/port
           racket/string
           rackunit
           web3
           web3/examples/09-accounts)

  (define anvil-0
    (hex-string->secret-key "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))

  (define anvil-0-address "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")

  (define anvil-0-signature
    (string-append "0x73eebf81a611136662d65778960c853fdcaf6eca86793ed9cabc30f2195937af"
                   "78a07e601627da5b4cc80c0ab35f6894da19b4a01759d90c101d9c9dd1c6745d1b"))

  (test-case "run prints the address, the signature of hello's digest and its signer"
    (check-equal? (with-output-to-string (lambda () (run anvil-0)))
                  (string-append "address:   " anvil-0-address "\n"
                                 "signature: " anvil-0-signature "\n"
                                 "recovered: " anvil-0-address "\n")))

  (test-case "main signs with a fresh key and recovers its address"
    (define lines
      (string-split (with-output-to-string
                      (lambda () (dynamic-require '(submod web3/examples/09-accounts main) #f)))
                    "\n"))
    (match lines
      [(list (pregexp #px"^address:   (.*)$" (list _ address)) signature-line recovered-line)
       (check-pred checksum-address? address)
       (check-regexp-match #px"^signature: 0x[0-9a-f]{130}$" signature-line)
       (check-equal? recovered-line (string-append "recovered: " address))]
      [_ (fail (format "not three lines starting with an address: ~s" lines))]))

  (test-case "signer-of reads a signature whose last byte is the y-parity"
    (define hex (signature->hex (sign-digest anvil-0 message-digest)))
    (check-equal? (signer-of (string-append (substring hex 0 130) "00") message-digest)
                  anvil-0-address)))
