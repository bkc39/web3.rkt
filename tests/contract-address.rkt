#lang racket/base

;; CREATE and CREATE2 addresses, offline.

(module+ test
  (require racket/match
           rackunit
           web3
           "abi-gen.rkt"
           "util.rkt")

  (define zero-salt (make-bytes 32 0))

  (define (hex->salt hex)
    (word (string->number (substring hex 2) 16)))

  (test-case "create2-address gives EIP-1014's examples"
    (for ([row (in-list
                `(("0x0000000000000000000000000000000000000000" #f "0x00"
                   "0x4D1A2e2bB4F88F0250f26Ffff098B0b30B26BF38")
                  ("0xdeadbeef00000000000000000000000000000000" #f "0x00"
                   "0xB928f69Bb1D91Cd65274e3c79d8986362984fDA3")
                  ("0xdeadbeef00000000000000000000000000000000"
                   "0x000000000000000000000000feed000000000000000000000000000000000000" "0x00"
                   "0xD04116cDd17beBE565EB2422F2497E06cC1C9833")
                  ("0x0000000000000000000000000000000000000000" #f "0xdeadbeef"
                   "0x70f2b2914A2a4b783FaEFb75f459A580616Fcb5e")
                  ("0x00000000000000000000000000000000deadbeef" "0xcafebabe" "0xdeadbeef"
                   "0x60f3f640a8508fC6a86d45DF051962668E1e8AC7")
                  ("0x00000000000000000000000000000000deadbeef" "0xcafebabe"
                   ,(string-append "0x" (apply string-append (for/list ([_ (in-range 11)]) "deadbeef")))
                   "0x1d8bfDC5D46DC4f61D6b6115972536eBE6A8854C")
                  ("0x0000000000000000000000000000000000000000" #f "0x"
                   "0xE33C0C7F7df4809055C3ebA6c09CFe4BaF1BD9e0")))])
      (match-define (list deployer salt code address) row)
      (check-equal? (create2-address deployer (if salt (hex->salt salt) zero-salt) code) address
                    (format "~s" row))))

  (test-case "create-address gives the addresses anvil assigns"
    (define dave "0x90F79bf6EB2c4f870365E785982E1f101E93b906")
    (for ([row (in-list `((0 "0x057ef64E23666F000b34aE31332854aCBd1c8544")
                          (1 "0x261D8c5e9742e6f7f1076Fa1F560894524e19cad")
                          (127 "0x5959BCfA51CB0C3C25C5aE23a8F77ab60d085627")
                          (128 "0x7BdE19EEE94922E550637f0cf1D8ab6198Bb1B0A")
                          (255 "0x9aB4d1736c3c75f8B95697BA1E0a9a11aBE33734")
                          (256 "0x5c458fcc384110A29D71b674a33f9e5Fa126Ca63")
                          (65535 "0x4D1E16A3E1c52bbCd369A831410116C23D78D382")
                          (65536 "0x3776C0e36dfaCA628628AcEc8b1f240f4B6851e5")
                          (,(expt 2 32) "0xa1B7382EeCF24261dFA47475FD3E58eA1e67B276")
                          (,(- (expt 2 64) 2) "0x7B57a5A5C751878467e14246137dB39A9F837c04")))])
      (match-define (list nonce address) row)
      (check-equal? (create-address dave nonce) address (format "nonce ~a" nonce)))
    (check-equal? (create-address anvil-alice 0) "0x5FbDB2315678afecb367f032d93F642f64180aa3")
    (check-equal? (create-address anvil-alice 1) "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512"))

  (test-case "addresses do not depend on the case of the inputs or the form of salt and code"
    (define prng (seeded 1 0 1 4 7 2))
    (define (random-address)
      (bytes->hex-string (random-bytes prng 20)))
    (define (mixed-case s)
      (string-append "0x"
                     (list->string (for/list ([c (in-string s 2)])
                                     (if (zero? (random 2 prng)) (char-upcase c) c)))))
    (for ([_ (in-range 100)])
      (define sender (random-address))
      (define nonce (min (random-abi-value prng 'uint64) (- (expt 2 64) 2)))
      (define address (create-address sender nonce))
      (check-pred checksum-address? address)
      (check-equal? (create-address (string-append "0x" (string-upcase (substring sender 2))) nonce)
                    address)
      (check-equal? (create-address (mixed-case sender) nonce) address)
      (define salt (random-abi-value prng 'uint256))
      (define code (random-bytes prng (random 100 prng)))
      (define create2 (create2-address sender salt code))
      (check-pred checksum-address? create2)
      (check-equal? (create2-address (mixed-case sender) (word salt) (bytes->hex-string code)) create2))))
