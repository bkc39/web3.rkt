#lang racket/base

(module+ test
  (require (only-in racket/contract -> any/c contract)
           racket/port
           rackunit
           web3/account
           (only-in web3/rpc bytes->hex-string exn:fail:web3?)
           "helpers/accounts.rkt"
           "helpers/secrets.rkt"
           (only-in "util.rkt" word))

  (define good-key (hex-string->secret-key (car anvil-keys)))
  (define good-signature (signature 5 6 0))
  (define marker-symbol (string->symbol marker-hex))
  (define marker-bytes-hex (string->bytes/utf-8 marker-hex))

  (define (raised thunk)
    (with-handlers ([(lambda (_) #t) values])
      (thunk)
      #f))

  (define whitespace '("\n" "\r\n" " " "\t"))

  (define refused-calls
    (append
     (for*/list ([w (in-list whitespace)]
                 [s (in-list (list (string-append marker-hex w) (string-append w marker-hex)))])
       (lambda () (hex-string->secret-key s)))
     (list
      (lambda () (hex-string->secret-key (string-append "0x" marker-hex "1")))
      (lambda () (hex-string->secret-key (substring marker-hex 0 63)))
      (lambda () (hex-string->secret-key (string-append (substring marker-hex 0 63) "g")))
      (lambda () (hex-string->secret-key (string-append "0x0x" marker-hex)))
      (lambda () (hex-string->secret-key marker-bytes-hex))
      (lambda () (hex-string->secret-key marker-symbol))
      (lambda () (hex-string->secret-key marker-hex marker-hex))
      (lambda () (hex-string->secret-key marker-hex #:chain-id marker-hex))
      (lambda () (secret-key->address marker-hex))
      (lambda () (secret-key->address marker))
      (lambda () (sign-digest marker-hex digest-empty))
      (lambda () (sign-digest marker digest-empty))
      (lambda () (sign-digest marker good-key))
      (lambda () (public-key->address marker-hex))
      (lambda () (public-key->address marker))
      (lambda () (recover-address marker-hex digest-empty))
      (lambda () (recover-address marker digest-empty))
      (lambda () (recover-address good-signature marker-hex))
      (lambda () (bytes->signature marker-hex))
      (lambda () (bytes->signature marker))
      (lambda () (signature-v marker-hex))
      (lambda () (signature-v marker)))))

  (test-case "the leak check finds the marker in hex, written and decimal form"
    (check-true (leaks? (string-upcase marker-hex)))
    (check-true (leaks? (format "~s" marker)))
    (check-true (leaks? (number->string marker-integer)))
    (check-true (leaks? (exn-message (raised (lambda ()
                                               ((contract (-> symbol? any/c) values 'p 'n)
                                                marker-hex))))))
    (check-false (leaks? (bytes->hex-string (make-bytes 32 7)))))

  (test-case "no refusal shows the key it was given"
    (for ([call (in-list refused-calls)])
      (define e (raised call))
      (check-pred exn:fail? e)
      (when (exn:fail? e)
        (check-no-leak (exn-message e) (exn-message e))
        (when (exn:fail:web3? e)
          (check-no-leak (format "~s" (struct->vector e)))))))

  (test-case "a trailing newline is named in the refusal"
    (define e (raised (lambda () (hex-string->secret-key (string-append "0x" marker-hex "\n")))))
    (check-pred exn:fail:contract? e)
    (when e
      (check-regexp-match
       #rx"\n  given: a string of 67 characters, with leading or trailing whitespace\n"
       (exn-message e))))

  (test-case "a key prints as #<secret-key> in every mode"
    (define key (hex-string->secret-key marker-hex))
    (for ([mode (in-list '("~a" "~s" "~v" "~e"))])
      (check-equal? (format mode key) "#<secret-key>" mode)))

  (test-case "exn:fail:web3:key carries nothing but its message"
    (check-equal? (procedure-arity exn:fail:web3:key) 2)
    (check-equal? (procedure-arity exn:fail:web3:signature) 2)
    (for ([s (in-list (list (make-string 64 #\0)
                            (bytes->hex-string (word secp256k1-order))))])
      (define e (raised (lambda () (hex-string->secret-key s))))
      (check-pred exn:fail:web3:key? e)
      (when e
        (check-equal? (struct->vector e)
                      (vector 'struct:exn:fail:web3:key (exn-message e)
                              (exn-continuation-marks e) '...)))))

  (test-case "signing with the marker key prints nothing"
    (define key (hex-string->secret-key marker-hex))
    (check-equal? (parameterize ([error-print-width 10000])
                    (with-output-to-string
                      (lambda ()
                        (sign-digest key digest-empty)
                        (secret-key->address key))))
                  ""))

  (test-case "a signature is public: one whose r is the marker is accepted"
    (check-equal? (signature-r (signature marker-integer 1 0)) marker-integer)))
