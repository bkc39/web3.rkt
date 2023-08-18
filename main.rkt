#lang racket/base

(require
 json
 net/http-client
 net/uri-codec
 racket/contract
 racket/port
 syntax/parse/define
 racket/string)
(require
 (for-syntax
  casemate
  racket/base
  racket/syntax))

(module+ test
  (require rackunit))

(define base-url
  "eth.llamarpc.com")

(define (hash-has-keys? h ks)
  (-> hash? (listof symbol?) boolean?)
  (for/and ([k (in-list ks)])
    (hash-has-key? h k)))

(struct jsonrpc-response-payload (id jsonrpc result))

(define (jsexpr->jsonrpc-response-payload jsexpr)
  (-> (and/c jsexpr? hash?) jsonrpc-response-payload?)
  ;; (printf "~a~n" jsexpr)
  (for ([k (in-list '(id jsonrpc result))])
    (unless (hash-has-key? jsexpr k)
      (raise-argument-error 'jsexpr->jsonrpc-response-payload
                            "expected key: ~a in ~a"
                            k jsexpr)))
  (jsonrpc-response-payload (hash-ref jsexpr 'id)
                            (hash-ref jsexpr 'jsonrpc)
                            (hash-ref jsexpr 'result)))

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
  ((compose1 jsexpr->jsonrpc-response-payload
             bytes->jsexpr
             port->bytes)
   in))

(begin-for-syntax
  (define-syntax-class jsonrpc-input-spec
    (pattern [contract?:expr ->param:expr])
    (pattern contract?:expr
             #:attr ->param #'(lambda (x) x))))

(define-syntax-parse-rule
  (define-json-rpc-endpoint rpc-name:id
    ((~datum ->) p:jsonrpc-input-spec ... return-contract?)
    (~optional (~seq #:result-transform result-transform)))
  #:with rpc-endpoint-name (format-id #'rpc-name
                                      "~a"
                                      (->kebab-case (syntax->datum #'rpc-name)))
  #:with (arg ...) (generate-temporaries #'(p ...))
  #:with rpc-endpoint-str #`#,(symbol->string
                               (syntax->datum #'rpc-name))
  #:with result-fn (or (attribute result-transform)
                       #'(lambda (x) x))
  (begin
    (define/contract (rpc-endpoint-name arg ... #:id [id 0])
      (->* (p.contract? ...)
           (#:id (or/c string? integer?))
           return-contract?)
      (define response
        (call-with-params rpc-endpoint-str id (list (p.->param arg) ...)))
      (struct-copy
       jsonrpc-response-payload response
       [result (result-fn (jsonrpc-response-payload-result response))]))))

(define/contract (hex-string->number hex-string)
  (-> string? number?)
  (cond
    [(string-prefix? hex-string "0x")
     (string->number
      (substring hex-string 2 (string-length hex-string))
      16)]
    [else
     (string->number hex-string)]))

(define (number->hex-string n)
  (string-append "0x"
                 (number->string n 16)))


(define-json-rpc-endpoint web3_clientVersion
  (-> jsonrpc-response-payload?))

(define-json-rpc-endpoint web3_sha3
  (-> string? jsonrpc-response-payload?))

(define-json-rpc-endpoint net_version
  (-> jsonrpc-response-payload?)
  #:result-transform hex-string->number)

(define-json-rpc-endpoint net_listening
  (-> jsonrpc-response-payload?))

(define-json-rpc-endpoint net_peerCount
  (-> jsonrpc-response-payload?)
  #:result-transform hex-string->number)

(define-json-rpc-endpoint eth_protocolVersion
  (-> jsonrpc-response-payload?))

(define-json-rpc-endpoint eth_syncing
  (-> jsonrpc-response-payload?))

(define-json-rpc-endpoint eth_coinbase
  (-> jsonrpc-response-payload?))

(define-json-rpc-endpoint eth_chainId
  (-> jsonrpc-response-payload?))

(define-json-rpc-endpoint eth_mining
  (-> jsonrpc-response-payload?))

(define-json-rpc-endpoint eth_hashrate
  (-> jsonrpc-response-payload?))

;; unit wei
(define-json-rpc-endpoint eth_gasPrice
  (-> jsonrpc-response-payload?)
  #:result-transform hex-string->number)

(define-json-rpc-endpoint eth_accounts
  (-> jsonrpc-response-payload?))

(define-json-rpc-endpoint eth_blockNumber
  (-> jsonrpc-response-payload?)
  #:result-transform hex-string->number)

(define default-block-param/c
  (or/c exact-nonnegative-integer?
        "latest"
        "earliest"
        "pending"
        "safe"
        "finalized"))

;; wei
(define-json-rpc-endpoint eth_getBalance
  (-> string? default-block-param/c jsonrpc-response-payload?)
  #:result-transform hex-string->number)

(define-json-rpc-endpoint eth_getStorageAt
  (-> [string? (lambda (x) x)]
      [exact-nonnegative-integer? number->hex-string]
      default-block-param/c
      jsonrpc-response-payload?))

(module+ test
  (check-equal? (hex-string->number "0x2") 2)
  (check-equal? (hex-string->number "2") 2)
  
  (define (check-response-with-result-pred pred? fn . args)
    (define response
      (apply fn args))
    (check-pred jsonrpc-response-payload? response)
    (check-pred pred? (jsonrpc-response-payload-result response)))

  (define-syntax-parse-rule
    (check-endpoint* (fn:id args:expr ... (~datum =>?) pred?:expr) ...)
    (begin
      (check-response-with-result-pred pred? fn args ...)
      ...))

  (define (eth-syncing-response-valid? result)
    (define result-obj-keys
      '(startingBlock
        currentBlock
        highestBlock))
    (or (boolean? result)
        (and (jsexpr? result)
             (hash? result)
             (hash-has-keys? result result-obj-keys))))

  (define vitalik.eth
    "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045")

  (define storage-contract-addr
    "0x295a70b2de5e3953354a6a8344e616ed314d7251")
  
  (check-endpoint*
   [web3-sha3 "0x68656c6c6f20776f726c64" =>? string?]
   [net-version =>? exact-nonnegative-integer?]
   [net-listening =>? boolean?]
   [net-peer-count =>? exact-positive-integer?]
   [eth-protocol-version =>? string?]
   [eth-syncing =>? eth-syncing-response-valid?]
   [eth-coinbase =>? string?]
   [eth-chain-id =>? string?]
   [eth-mining =>? boolean?]
   [eth-hashrate =>? string?]
   [eth-gas-price =>? exact-nonnegative-integer?]
   [eth-accounts =>? (listof string?)]
   [eth-block-number =>? number?]
   [eth-get-balance vitalik.eth "latest" =>? number?]
   [eth-get-balance vitalik.eth "earliest" =>? number?]
   [eth-get-balance vitalik.eth "pending" =>? number?]
   [eth-get-balance vitalik.eth "finalized" =>? number?]
   [eth-get-balance vitalik.eth "safe" =>? number?]
   [eth-get-storage-at storage-contract-addr #x0 "latest" =>? string?]

   )
  )    

