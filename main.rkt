#lang racket/base

(require net/http-client
         net/uri-codec
         racket/contract
         racket/port
         json)

(module+ test
  (require rackunit))

(define base-url
  "eth.llamarpc.com")

(define (call-with-params method-name id params)
  (define jsonrpc-version "2.0")
  (define post-data
    (make-hasheq
     `((jsonrpc . ,jsonrpc-version)
       (method . ,method-name)
       (params . ,params)
       (id . ,id))))
  (define-values (st h in)
    (http-sendrecv
     base-url "/"
     #:method #"POST"
     #:ssl? #t
     #:headers '("Content-Type: application/json")
     #:data (jsexpr->bytes post-data)))
  (bytes->jsexpr (port->bytes in)))

(define/contract (web3-clientVersion)
  (-> jsexpr?)
  (call-with-params "web3_clientVersion" 67 '()))

(define/contract (web3-sha3 data)
  (-> string? jsexpr?)
  (call-with-params "web3_sha3" 67 (list data)))

(define/contract (net-version)
  (-> jsexpr?)
  (call-with-params "net_version" 67 '()))

(define/contract (net-listening)
  (-> jsexpr?)
  (call-with-params "net_listening" 67 '()))

(define/contract (net-listening)
  (-> jsexpr?)
  (call-with-params "net_listening" 67 '()))

(define/contract (net-peerCount)
  (-> jsexpr?)
  (call-with-params "net_peerCount" 67 '()))

(module+ test
  (check-pred jsexpr? (web3-clientVersion))

  (define (check-response-with-result-type type-pred? fn . args)
    (define response
      (apply fn args))
    (check-pred jsexpr? response)
    (check-pred type-pred? (hash-ref response 'result)))

  (check-response-with-result-type string? web3-sha3 "0x68656c6c6f20776f726c64")
  (check-response-with-result-type string? net-version)
  (check-response-with-result-type string? net-version)
  (check-response-with-result-type boolean? net-listening)

  
  
  
  )


