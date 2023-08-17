#lang racket/base

(require net/http-client
         net/uri-codec
         racket/contract
         racket/port

         syntax/parse/define
         json
         (for-syntax casemate
                     racket/base
                     racket/syntax))

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

(define-syntax-parse-rule
  (define-json-rpc-endpoint rpc-name:id
    ((~datum ->) contract? ... return-contract?))
  #:with rpc-endpoint-name (format-id #'rpc-name
                                      "~a"
                                      (->kebab-case (syntax->datum #'rpc-name)))
  #:with (arg ...) (generate-temporaries #'(contract? ...))
  #:with rpc-endpoint-str #`#,(symbol->string
                               (syntax->datum #'rpc-name))
  (begin
    (define/contract (rpc-endpoint-name arg ... #:id [id 0])
      (->* (contract? ...)
           (#:id (or/c string? integer?))
           return-contract?)
      (call-with-params rpc-endpoint-str id (list arg ...)))))

(define-json-rpc-endpoint web3_clientVersion
  (-> jsexpr?))

(define-json-rpc-endpoint web3_sha3
  (-> string? jsexpr?))

(define-json-rpc-endpoint net_version
  (-> jsexpr?))

(define-json-rpc-endpoint net_listening
  (-> jsexpr?))

(define-json-rpc-endpoint net_peerCount
  (-> jsexpr?))

(define-json-rpc-endpoint eth_protocolVersion
  (-> jsexpr?))

(module+ test
  (define (check-response-with-result-type type-pred? fn . args)
    (define response
      (apply fn args))
    (check-pred jsexpr? response)
    (check-pred type-pred? (hash-ref response 'result)))

  (check-response-with-result-type string? web3-sha3 "0x68656c6c6f20776f726c64")
  (check-response-with-result-type string? net-version)
  (check-response-with-result-type string? net-version)
  (check-response-with-result-type boolean? net-listening)
  (check-response-with-result-type string? net-peer-count)
  (check-response-with-result-type string? eth-protocol-version)
  
  )


