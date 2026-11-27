#lang racket/base

(module+ test
  (require racket/match
           rackunit
           web3
           (only-in "helpers/records.rkt" check-in-web3-and-manual)
           (only-in "util.rkt" anvil-bob anvil-carol check-blames-here))

  (define (keys count)
    (for/list ([index (in-range count)])
      (make-bytes 32 index)))

  (define intrinsic-rows
    `([#"" () #f 21000]
      [,(make-bytes 10 0) () #f 21040]
      [,(make-bytes 10 255) () #f 21160]
      [,(bytes 0 1 0 2) () #f 21040]
      [#"\1" () #t 53018]
      [,(make-bytes 32 0) () #t 53130]
      [,(make-bytes 33 0) () #t 53136]
      [#"" () #t 53000]
      [#"" ((,anvil-bob) (,anvil-carol ,@(keys 3))) #f 31500]
      [#"\1" ((,anvil-bob) (,anvil-carol ,@(keys 3))) #t 63518]))

  (test-case "G1: intrinsic gas counts data, creation words and the access list"
    (for ([row (in-list intrinsic-rows)])
      (match-define (list data access-list creation? gas) row)
      (check-equal? (intrinsic-gas data #:creation? creation? #:access-list access-list) gas
                    (format "~s ~s ~s" data access-list creation?))))

  (test-case "G1: the defaults are a call with no access list"
    (check-equal? (intrinsic-gas #"\1\0") 21020))

  (define floor-rows
    `([#"" 21000]
      [,(make-bytes 10 0) 21100]
      [,(make-bytes 10 255) 21400]
      [,(bytes 0 1 0 2) 21100]))

  (test-case "G2: the floor is 21000 and 10 per token"
    (for ([row (in-list floor-rows)])
      (match-define (list data gas) row)
      (check-equal? (floor-gas data) gas (format "~s" data))))

  (test-case "G3: the floor is above the intrinsic gas of any call data, and equal for none"
    (for ([data (list (make-bytes 1 0) (make-bytes 1 7) (make-bytes 100 3) (bytes 0 9 0 9))])
      (check-true (> (floor-gas data) (intrinsic-gas data)) (format "~s" data)))
    (check-equal? (floor-gas #"") (intrinsic-gas #"")))

  (test-case "G4: wrong arguments blame the caller"
    (check-blames-here (intrinsic-gas "0x"))
    (check-blames-here (intrinsic-gas #"" #:creation? 1))
    (check-blames-here (intrinsic-gas #"" #:access-list (list (list "0x1234"))))
    (check-blames-here (floor-gas "0x")))

  (test-case "G5: intrinsic-gas and floor-gas are in web3 and the manual"
    (check-in-web3-and-manual '(intrinsic-gas floor-gas))))
