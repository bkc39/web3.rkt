#lang racket/base

(module+ test
  (require racket/port
           (only-in racket/string string-append* string-join)
           rackunit
           (only-in web3/rpc hex-string->bytes)
           (only-in "../helpers/secrets.rkt" check-no-leak))

  (define key
    (hex-string->bytes "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))

  (define expected
    `("type 0"
      ,(string-append*
        '("  raw    0xf86e808506fc23ac008252089470997970c51812dc3a010c7d01b50e0d17dc79c88814d1"
          "120d7b1600008082f4f6a0bcdaf956c629f89d42ac0b1c93413ba5054ed8994e698357dd5f46e67b0f"
          "accea0660743d0f7ea2e5700c4d8a6aec09bd3f91260bda7e208a9c0619265741b7598"))
      "  hash   0x23d0874d9c3f3f40f63df8695d58550f22d4095d05272b0564eca4433005cde9"
      "  sender 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"
      "type 1"
      ,(string-append*
        '("  raw    0x01f8a9827a69808506fc23ac008275309470997970c51812dc3a010c7d01b50e0d17dc79"
          "c88814d1120d7b16000080f838f79470997970c51812dc3a010c7d01b50e0d17dc79c8e1a000000000"
          "0000000000000000000000000000000000000000000000000000000101a0803a9c4afe2ab4265bfc75"
          "9488c8c64e3c31b94a723e5109851afbe391bc8bb9a011fb7da455cc3c5cfc3fff4b48318aa3c42832"
          "481634546bdf474cc5dd69c7a7"))
      "  hash   0x9e7b669c47958eaa50ebec28a6941dfc397986688e124eb1cacc1d290a693326"
      "  sender 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"
      "type 2"
      ,(string-append*
        '("  raw    0x02f875827a698084773594008506fc23ac008252089470997970c51812dc3a010c7d01b5"
          "0e0d17dc79c88814d1120d7b16000080c001a0e4283c98410a9ee00d296ef6d31b47e6326541bf89e1"
          "4640e3ff09b145517165a070e1e6a5e2a7b71af2ff6278becbe8de276a30f350ea156b438678751338"
          "171f"))
      "  hash   0x6e6196f107bb02ceffd0db99a2c3be3053f8d1917061e67f88c7843aa77833e2"
      "  sender 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"
      "truncated: short at byte 1"))

  (define output
    (with-output-to-string
      (lambda () (dynamic-require '(submod web3/examples/12-transactions main) #f))))

  (test-case "X1: main prints three signed transactions, parsed back, and a refused truncation"
    (check-equal? output (string-append (string-join expected "\n") "\n")))

  (test-case "X2: the output shows no part of the key"
    (check-no-leak output #:secret key)))
