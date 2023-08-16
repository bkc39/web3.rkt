#lang racket/base

(require net/http-client
         net/uri-codec
         racket/port
         json)

(module+ test
  (require rackunit))

(define-values (st h in)
  (let ([post-data
         '#hasheq((jsonrpc . "2.0")
                  (method . "web3_clientVersion")
                  (params . ())
                  (id . 67))])
    (http-sendrecv
     "eth.llamarpc.com"
     "/"
     #:method #"POST"
     #:ssl? #t
     #:headers '("Content-Type: application/json")
     #:data (jsexpr->bytes post-data))))

(module+ test
  (check-pred jsexpr? (list)))


(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))
