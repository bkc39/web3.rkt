#lang racket/base

(module+ test
  (require (only-in racket/contract flat-contract-predicate)
           racket/match
           rackunit
           web3/abi
           (only-in web3/private/abi-decode abi-decode-reason/c)
           (only-in web3/rpc bytes->hex-string)
           "abi-gen.rkt"
           "bounded.rkt"
           "util.rkt")

  (define seed '(1 2 3 4 5 6))
  (define reason? (flat-contract-predicate abi-decode-reason/c))

  (test-case "decoding any input finishes in bounds with values or a decode error"
    (define prng (apply seeded seed))
    (for ([input (in-list (random-decode-inputs prng))])
      (match-define (list kind ts data original) input)
      (define name (format "seed ~s, ~a ~s ~a" seed kind (map abi-type->string ts) (bytes->hex-string data)))
      (match (call-bounded (lambda () (abi-decode ts data)))
        [(list 'ok vs)
         (check-decoded ts data vs name)
         (check-equal? (abi-decode ts (abi-encode ts vs)) vs name)
         (when (eq? kind 'append)
           (check-equal? vs (abi-decode ts original) name))]
        [(list 'raised (? exn:fail:web3:abi:decode? e))
         (check-pred reason? (exn:fail:web3:abi:decode-reason e) name)
         (check-true (<= (exn:fail:web3:abi:decode-offset e) (bytes-length data)) name)
         (check-not-eq? kind 'append name)]
        [other (fail (format "~a: ~e" name other))]))))
