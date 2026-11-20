#lang racket/base

;; The examples of the manual's Accounts and Signatures sections, block by block, in one evaluator
;; set up as the manual's, with what each form prints there.

(module+ test
  (require racket/match
           rackunit
           (only-in scribble/example make-base-eval)
           (only-in web3/rpc hex-string->bytes)
           "helpers/secrets.rkt")

  (define anvil-0-address "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")

  (define blocks
    `(([(define key
          (hex-string->secret-key
           "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))
        (void)]
       [key (prints "#<secret-key>")]
       [(secret-key->address key) (value ,anvil-0-address)])
      ([(hex-string->secret-key
         "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80\n")
        (refused "a string of 67 characters, with leading or trailing whitespace")])
      ([(define digest (keccak256 #"hello")) (void)]
       [(define sig (sign-digest key digest)) (void)]
       [(recover-address sig digest) (value ,anvil-0-address)]
       [(equal? (recover-address sig (keccak256 #"hello!")) (secret-key->address key))
        (value #f)])
      ([(bytes->hex-string (signature->bytes sig))
        (value ,(string-append
                 "0x73eebf81a611136662d65778960c853fdcaf6eca86793ed9cabc30f2195937af"
                 "78a07e601627da5b4cc80c0ab35f6894da19b4a01759d90c101d9c9dd1c6745d1b"))]
       [(equal? (bytes->signature (signature->bytes sig)) sig) (value #t)])
      ([(secret-key->address
         (hex-string->secret-key
          "0000000000000000000000000000000000000000000000000000000000000001"))
        (value "0x7E5F4552091A69125d5DfCb7b8C2659029395Bdf")]
       [(hex-string->secret-key (make-string 64 #\0))
        (raises ,(string-append "hex-string->secret-key: invalid secret key;\n"
                                " the value is zero or not less than the curve order"))])
      ([(public-key->address (secret-key->public-key key)) (value ,anvil-0-address)])
      ([(define high (signature (signature-r sig)
                                (- secp256k1-order (signature-s sig))
                                (- 1 (signature-y-parity sig))))
        (void)]
       [(signature-low-s? high) (value #f)]
       [(recover-address high digest) (value ,anvil-0-address)])
      ([(define bs (signature->bytes sig)) (void)]
       [(equal? (bytes->signature (bytes-append (subbytes bs 0 64) (bytes 0))) sig) (value #t)]
       [(bytes->signature (bytes-append (subbytes bs 0 64) (bytes 37)))
        (raises "bytes->signature: invalid signature;\n v is 37, not 0, 1, 27 or 28")])))

  (define anvil-0-secret
    (hex-string->bytes "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))

  (define (outcome ev form)
    (with-handlers ([exn:fail? values])
      (ev form)))

  (define (check-form ev form expectation)
    (define name (format "~s" form))
    (define result (outcome ev form))
    (match expectation
      ['(void) (check-pred void? result name)]
      [`(prints ,text) (check-equal? (format "~a" result) text name)]
      [`(value ,v) (check-equal? result v name)]
      [`(raises ,message)
       (check-pred exn:fail? result name)
       (when (exn:fail? result)
         (check-equal? (exn-message result) message name))]
      [`(refused ,given)
       (check-pred exn:fail:contract? result name)
       (when (exn:fail? result)
         (check-regexp-match (regexp (string-append "\n  given: " (regexp-quote given) "\n"))
                             (exn-message result)
                             name)
         (check-no-leak (exn-message result) name #:secret anvil-0-secret))]))

  (test-case "the manual's account examples print what the manual shows"
    (define ev (make-base-eval '(require web3 (only-in keccak keccak256))))
    (for* ([block (in-list blocks)]
           [entry (in-list block)])
      (match-define (list form expectation) entry)
      (check-form ev form expectation))))
