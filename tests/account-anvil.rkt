#lang racket/base

;; Recovery against the ecrecover precompile of a throwaway anvil.  Skipped when anvil is not
;; available.

(module+ test
  (require racket/match
           rackunit
           web3
           "helpers/accounts.rkt"
           (only-in "util.rkt" word))

  (define ecrecover "0x0000000000000000000000000000000000000001")

  (define (precompile-recover digest v r s)
    (define result
      (eth-call (hasheq 'to ecrecover
                        'data (bytes->hex-string (bytes-append digest (word v) (word r) (word s))))
                "latest"))
    (and (not (equal? result "0x"))
         (address->checksum-string (string-append "0x" (substring result 26)))))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping account node tests: anvil not found")]
    [else
     (with-anvil ()
       (test-case "recover-address agrees with the ecrecover precompile"
         (for* ([c (in-list (key-digest-cases 24 2 7 1 8 2 8))]
                [sig (in-list (let ([sig (sign-digest (car c) (cdr c))])
                                (list sig (other-s sig) (flipped-parity sig))))])
           (match-define (signature r s _) sig)
           (check-equal? (precompile-recover (cdr c) (signature-v sig) r s)
                         (recover-address sig (cdr c)))))

       (test-case "the precompile recovers nothing for v = 0"
         (match-define (signature r s _)
           (sign-digest (hex-string->secret-key (car anvil-keys)) digest-empty))
         (check-false (precompile-recover digest-empty 0 r s)))

       (test-case "anvil's accounts are the addresses of its ten keys"
         (check-equal? (map address->checksum-string (eth-accounts)) anvil-addresses)))]))
