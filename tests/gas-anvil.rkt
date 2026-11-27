#lang racket/base

(module+ test
  (require racket/format
           rackunit
           web3
           (only-in "helpers/mock.rkt" call-with-fields)
           (only-in "util.rkt" anvil-alice anvil-bob anvil-carol))

  (define nobody (integer->address #xdead1))
  (define data (bytes-append (make-bytes 10 0) (make-bytes 7 9) (bytes 0 1)))
  (define short-data (bytes 0 1))
  (define access-list (list (list anvil-bob) (list anvil-carol (make-bytes 32 1) (make-bytes 32 2)
                                                   (make-bytes 32 3))))
  (define init (hex-string->bytes "0x60006000f3"))

  (define (gas-used to . fields)
    (receipt-gas-used (call-with-fields send-transaction/receipt fields anvil-alice to)))

  (define (estimate to data)
    (estimate-transaction anvil-alice to #:data data))

  (define (floored-cases fork)
    (test-case (~a "O1: on " fork ", a send with calldata to no code uses the larger of the two")
      (check-true (> (floor-gas data) (intrinsic-gas data)))
      (check-equal? (gas-used nobody '#:data data) (max (intrinsic-gas data) (floor-gas data)))
      (check-equal? (estimate nobody data) (floor-gas data))))

  (define (common-cases fork)
    (test-case (~a "O2: on " fork ", a type 1 send with an access list uses its intrinsic gas")
      (check-true (> (intrinsic-gas short-data #:access-list access-list) (floor-gas short-data)))
      (check-equal? (gas-used nobody '#:data short-data '#:type 1 '#:access-list access-list)
                    (intrinsic-gas short-data #:access-list access-list)))

    (test-case (~a "O3: on " fork ", a creation that returns no code uses its intrinsic gas and 6")
      (check-equal? (gas-used #f '#:data init) (+ (intrinsic-gas init #:creation? #t) 6))))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping gas anvil tests: anvil not found")]
    [else
     (with-anvil ()
       (floored-cases "the latest hardfork")
       (common-cases "the latest hardfork"))
     (with-anvil (#:hardfork "prague")
       (floored-cases "prague")
       (common-cases "prague"))
     (with-anvil (#:hardfork "cancun")
       (test-case "O4: on cancun, before EIP-7623, the send's estimate is its intrinsic gas"
         ;; anvil's receipts charge EIP-7623's floor on every hardfork; its estimates do not.
         (check-equal? (estimate nobody data) (intrinsic-gas data)))
       (common-cases "cancun"))]))
