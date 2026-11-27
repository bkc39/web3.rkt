#lang racket/base

(module+ test
  (require rackunit
           (prefix-in keccak: (only-in keccak keccak256))
           (prefix-in web3: (only-in web3 bytes->hex-string keccak256)))

  (test-case "web3 provides keccak's keccak256 itself"
    (check-eq? web3:keccak256 keccak:keccak256)
    (check-equal? (web3:bytes->hex-string (web3:keccak256 #""))
                  "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

  (test-case "a module may require keccak256 from both web3 and keccak"
    (define program
      '(module both racket/base
         (require (only-in keccak keccak256) web3)
         (provide hashed)
         (define hashed (keccak256 #"abc"))))
    (define hashed
      (parameterize ([current-namespace (make-base-namespace)])
        (eval program)
        (dynamic-require ''both 'hashed)))
    (check-equal? hashed (keccak:keccak256 #"abc"))))
