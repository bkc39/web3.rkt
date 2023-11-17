#lang racket/base

(require
 json
 net/http-client
 net/url
 net/uri-codec
 racket/contract
 racket/port
 syntax/parse/define
 racket/exn
 racket/string
 rackunit)
(require
 (for-syntax
  casemate
  racket/base
  racket/syntax))

(module+ test
  (require rackunit))

(define-syntax-parse-rule (begin-for-tests expr ...)
  (module+ test
    expr ...))

(struct provider ())
(struct http-provider provider (conn)
  #:guard (struct-guard/c http-conn?))

(define (url->http-provider url-regexp)
  (define url
    (string->url url-regexp))
  (http-provider
   (http-conn-open
    (url-host url)
    #:ssl? (equal? "https" (url-scheme url))
    #:port (or (url-port url)
               (cond
                 [(equal? "https" (url-scheme url))
                  443]
                 [else
                  80]))
    #:auto-reconnect? #t)))

(module+ test
  (define http-provider
    (url->http-provider "http://localhost:8545"))
  (check-pred provider? http-provider))

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

(define current-provider
  (make-parameter
   'none
   (lambda (p)
     (unless (http-provider? p)
       (raise-argument-error 'current-provider
                             "http-provider?"
                             p))
     p)
   'current-provider-procedure))

(module+ test
  (current-provider http-provider))

(define-syntax-parse-rule (with-provider client:expr body:expr ...)
  (parameterize ([current-provider client])
    body ...))

(begin-for-tests
  ;; (define test-client
  ;;   (url->http-provider "https://eth.llamarpc.com"))
  ;; (current-provider test-client)
  
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

  (define example-block-hash
    "0xb903239f8543d04b5dc1ba6579132b143087c68db1b2168786408fcbce568238"))

(define (call-with-params method-name id params)
  (define jsonrpc-version "2.0")
  (define post-data
    (make-hasheq
     `((jsonrpc . ,jsonrpc-version)
       (method . ,method-name)
       (params . ,params)
       (id . ,id))))
  ;; (printf "sending params: ~a\n" post-data)
  (define-values (st h in)
    (http-conn-sendrecv!
     (http-provider-conn (current-provider))
     "/"
     #:method #"POST"
     #:headers '("Content-Type: application/json")
     #:data (jsexpr->bytes post-data)))
  (define jsexpr-response
    ((compose1 bytes->jsexpr port->bytes) in))
  (jsexpr->jsonrpc-response-payload jsexpr-response))

(define/contract (transaction-count-response->number r)
  (-> any/c exact-nonnegative-integer?)
  (cond
    [(equal? r 'null)
     0]
    [(string? r)
     (hex-string->number r)]
    [else
     (raise-argument-error
      'eth-get-block-transaction-count-by-hash
      "response either 'null or string?"
      r)]))

(begin-for-syntax
  (define-syntax-class jsonrpc-input-spec
    (pattern [contract?:expr ->param:expr])
    (pattern contract?:expr
             #:attr ->param #'(lambda (x) x)))
  
  (define-syntax-class endpoint-test-spec
    (pattern (args:expr ... (~datum =>?) pred?:expr))))


(define (check-response-with-result-pred pred? fn . args)
  (define response
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (list 'exception-raised e))])
      (apply fn args)))

  (when (and (list? response)
             (eq? 'exception-raised (car response)))
    (fail-check (format "Exception raised:~n~a~n"
                        (exn->string (cadr response)))))
  
  
  (check-pred jsonrpc-response-payload? response)
  (check-pred pred? (jsonrpc-response-payload-result response)))

(define-syntax-parse-rule
  (check-endpoint* (fn:id args:expr ... (~datum =>?) pred?:expr) ...)
  (begin
    (check-response-with-result-pred pred? fn args ...)
    ...))

(define-syntax-parse-rule
  (define-jsonrpc-endpoint rpc-name:id
    ((~datum ->) p:jsonrpc-input-spec ... return-contract?)
    (~optional (~seq #:result-transform result-transform))
    (~optional (~seq #:test* t*:endpoint-test-spec ...+)))
  #:with rpc-endpoint-name (format-id #'rpc-name
                                      "~a"
                                      (->kebab-case (syntax->datum #'rpc-name)))
  #:with (arg ...) (generate-temporaries #'(p ...))
  #:with rpc-endpoint-str #`#,(symbol->string
                               (syntax->datum #'rpc-name))
  #:with result-fn (or (attribute result-transform)
                       #'(lambda (x) x))
  #:with (test-case* ...)
  (if (attribute t*)
      (map (syntax-parser
             [t:endpoint-test-spec
              #'(rpc-endpoint-name t.args ... =>? t.pred?)])
           (syntax->list #'(t* ...)))
      #'())
  #:with test-cases (if (attribute t*)
                        #'(module+ test
                            (check-endpoint*
                             test-case* ...))
                        #'(void))
  (begin
    (define/contract (rpc-endpoint-name arg ... #:id [id 0])
      (->* (p.contract? ...)
           (#:id (or/c string? integer?))
           return-contract?)
      (define response
        (call-with-params rpc-endpoint-str id (list (p.->param arg) ...)))
      (struct-copy
       jsonrpc-response-payload response
       [result (result-fn (jsonrpc-response-payload-result response))]))
    test-cases))

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

(define-jsonrpc-endpoint web3_clientVersion
  (-> jsonrpc-response-payload?)
  #:test* [=>? string?])

(define-jsonrpc-endpoint web3_sha3
  (-> string? jsonrpc-response-payload?)
  #:test* ["0x68656c6c6f20776f726c64" =>? string?])

(define-jsonrpc-endpoint net_version
  (-> jsonrpc-response-payload?)
  #:result-transform hex-string->number
  #:test* [=>? exact-nonnegative-integer?])

(define-jsonrpc-endpoint net_listening
  (-> jsonrpc-response-payload?)
  #:test* [=>? boolean?])

(define-jsonrpc-endpoint net_peerCount
  (-> jsonrpc-response-payload?)
  #:result-transform hex-string->number
  #:test* [=>? exact-positive-integer?])

(define-jsonrpc-endpoint eth_protocolVersion
  (-> jsonrpc-response-payload?)
  #:test* [=>? string?])

(define-jsonrpc-endpoint eth_syncing
  (-> jsonrpc-response-payload?)
  #:test* [=>? eth-syncing-response-valid?])

(define-jsonrpc-endpoint eth_coinbase
  (-> jsonrpc-response-payload?)
  #:test* [=>? string?])

(define-jsonrpc-endpoint eth_chainId
  (-> jsonrpc-response-payload?)
  #:test* [=>? string?])

(define-jsonrpc-endpoint eth_mining
  (-> jsonrpc-response-payload?)
  #:test* [=>? boolean?])

(define-jsonrpc-endpoint eth_hashrate
  (-> jsonrpc-response-payload?)
  #:test* [=>? string?])

;; unit wei
(define-jsonrpc-endpoint eth_gasPrice
  (-> jsonrpc-response-payload?)
  #:result-transform hex-string->number
  #:test* [=>? exact-nonnegative-integer?])

(define-jsonrpc-endpoint eth_accounts
  (-> jsonrpc-response-payload?)
  #:test* [=>? (listof string?)])

(define-jsonrpc-endpoint eth_blockNumber
  (-> jsonrpc-response-payload?)
  #:result-transform hex-string->number
  #:test* [=>? number?])

(define default-block-param/c
  (or/c exact-nonnegative-integer?
        "latest"
        "earliest"
        "pending"
        "safe"
        "finalized"))

;; wei
(define-jsonrpc-endpoint eth_getBalance
  (-> string? default-block-param/c jsonrpc-response-payload?)
  #:result-transform hex-string->number
  #:test*
  [vitalik.eth "latest" =>? number?]
  [vitalik.eth "earliest" =>? number?]
  [vitalik.eth "pending" =>? number?]
  [vitalik.eth "finalized" =>? number?]
  [vitalik.eth "safe" =>? number?])

(define-jsonrpc-endpoint eth_getStorageAt
  (-> string?
      [exact-nonnegative-integer? number->hex-string]
      default-block-param/c
      jsonrpc-response-payload?)
  #:test*
  [storage-contract-addr #x0 "latest" =>? string?]
  [storage-contract-addr #x0 "earliest" =>? string?]
  [storage-contract-addr #x0 "pending" =>? string?]
  [storage-contract-addr #x0 "finalized" =>? string?]
  [storage-contract-addr #x0 "safe" =>? string?])

(define-jsonrpc-endpoint eth_getTransactionCount
  (-> string?
      default-block-param/c
      jsonrpc-response-payload?)
  #:result-transform hex-string->number
  #:test* [vitalik.eth "latest" =>? exact-nonnegative-integer?])

(define-jsonrpc-endpoint eth_getBlockTransactionCountByHash
  (-> string?
      jsonrpc-response-payload?)
  #:result-transform transaction-count-response->number
  #:test*
  [example-block-hash =>? exact-nonnegative-integer?])

(define/contract (default-block-param->param-string p)
  (-> default-block-param/c string?)
  (cond
    [(exact-nonnegative-integer? p)
     (number->hex-string p)]
    [else
     p]))

(define-jsonrpc-endpoint eth_getBlockTransactionCountByNumber
  (-> [default-block-param/c default-block-param->param-string]
      jsonrpc-response-payload?)
  #:result-transform transaction-count-response->number
  #:test*
  [232 =>? exact-nonnegative-integer?]
  ["latest" =>? exact-nonnegative-integer?])

(module+ test
  (check-equal? (hex-string->number "0x2") 2)
  (check-equal? (hex-string->number "2") 2))

