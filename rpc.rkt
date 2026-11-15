#lang racket/base

;; JSON-RPC transport: providers, structured errors, hex helpers, and the
;; `define-jsonrpc-endpoint` form that eth.rkt and anvil.rkt are built from.

(require json
         net/http-client
         net/url
         racket/contract
         racket/generic
         racket/match
         racket/port
         racket/string
         syntax/parse/define
         (for-syntax casemate
                     racket/base
                     racket/syntax))

(provide (struct-out exn:fail:web3)
         (struct-out exn:fail:web3:rpc)
         gen:provider
         provider?
         http-provider?
         mock-provider?
         with-provider
         define-jsonrpc-endpoint
         hex-string?
         (contract-out
          [rename send-request provider-request (-> provider? string? (listof jsexpr?) any)]
          [rename close-provider provider-close (-> provider? void?)]
          [current-provider (parameter/c (or/c #f provider?))]
          [url->http-provider (-> string? http-provider?)]
          [http-provider-url (-> http-provider? string?)]
          [mock-provider (-> mock-handler/c mock-provider?)]
          [mock-provider-calls (-> mock-provider? (listof (cons/c string? list?)))]
          [raise-rpc-error (->* (string? (or/c #f exact-integer?) string?)
                                ((or/c #f jsexpr?))
                                any)]
          [rpc-call (->* (string? (listof jsexpr?))
                         (#:provider (or/c #f provider?))
                         any)])
         hex-string->number
         number->hex-string
         bytes->hex-string
         hex-string->bytes)

(module+ test
  (require rackunit))

(struct exn:fail:web3 exn:fail ())
(struct exn:fail:web3:rpc exn:fail:web3 (method code rpc-message data))

(define (raise-rpc-error method code message [data #f])
  (raise (exn:fail:web3:rpc (format "~a: ~a" method message)
                            (current-continuation-marks)
                            method
                            code
                            message
                            data)))

(define (raise-web3-error method fmt . args)
  (raise (exn:fail:web3 (format "~a: ~a" method (apply format fmt args))
                        (current-continuation-marks))))

;;;; providers

(define-generics provider
  (provider-request provider method params)
  (provider-close provider)
  #:requires [provider-request]
  #:fallbacks [(define (provider-close _provider) (void))])

;; Providers are written by users, so what they return is checked here.
(define (send-request provider method params)
  (define result (provider-request provider method params))
  (unless (jsexpr? result)
    (raise-web3-error method "the provider returned ~e, not a JSON value" result))
  result)

(define (close-provider provider)
  (provider-close provider)
  (void))

(define current-provider (make-parameter #f))

(define-syntax-parse-rule (with-provider provider body:expr ...+)
  #:declare provider (expr/c #'(or/c #f provider?) #:name "the provider")
  (parameterize ([current-provider provider.c])
    body ...))

;;;; HTTP

(struct http-provider (url path conn owner last-id)
  #:methods gen:provider
  [(define (provider-request p method params)
     (response->result method
                       (call-with-connection p (lambda (conn) (exchange p conn method params)))))
   (define (provider-close p)
     (call-with-connection p drop-connection!))])

(define (url->http-provider url-string)
  (define u (string->url url-string))
  (define ssl? (equal? "https" (url-scheme u)))
  (define path
    (cond
      [(regexp-match #px"^[a-zA-Z]+://[^/]+(/.+)$" url-string) => cadr]
      [else "/"]))
  (http-provider url-string
                 path
                 (http-conn-open (url-host u)
                                 #:ssl? ssl?
                                 #:port (or (url-port u) (if ssl? 443 80))
                                 #:auto-reconnect? #t)
                 (box #f)
                 (box 0)))

;; `owner` holds the `holder` of the request using the connection.  A
;; request killed mid-exchange never releases it: the next one sees the
;; holder's thread dead, takes over, and drops the half-read connection.
(struct holder (thread released))

(define (call-with-connection p proc)
  (match-define (struct* http-provider ([conn conn] [owner owner])) p)
  (define me (holder (current-thread) (make-semaphore 0)))
  (define breaks? (break-enabled))
  (define finished? #f)
  (parameterize-break #f
    (take-connection! owner me conn breaks?)
    (dynamic-wind
      void
      (lambda ()
        (begin0 (parameterize-break breaks? (proc conn))
          (set! finished? #t)))
      (lambda ()
        (unless finished? (drop-connection! conn))
        (set-box! owner #f)
        (semaphore-post (holder-released me))))))

(define (take-connection! owner me conn breaks?)
  (define wait (if breaks? sync/enable-break sync))
  (let retry ()
    (define held (unbox owner))
    (cond
      [(not held) (unless (box-cas! owner #f me) (retry))]
      [(not (thread-dead? (holder-thread held)))
       (wait (thread-dead-evt (holder-thread held)) (semaphore-peek-evt (holder-released held)))
       (retry)]
      [(box-cas! owner held me) (drop-connection! conn)]
      [else (retry)])))

;; Closing flushes, which fails on a broken connection; abandoning it
;; still makes the next request reconnect.
(define (drop-connection! conn)
  (with-handlers ([exn:fail? (lambda (_) (http-conn-abandon! conn))])
    (http-conn-close! conn)))

(define (exchange p conn method params)
  (match-define (struct* http-provider ([path path] [last-id last-id])) p)
  (define id (add1 (unbox last-id)))
  (set-box! last-id id)
  (define body (jsexpr->bytes (hasheq 'jsonrpc "2.0" 'id id 'method method 'params params)))
  (define response
    (with-handlers ([exn:fail? (lambda (e) (raise-web3-error method "~a" (exn-message e)))])
      (define-values (_status _headers in)
        (http-conn-sendrecv! conn path
                             #:method #"POST"
                             #:headers '("Content-Type: application/json")
                             #:data body))
      (bytes->jsexpr (port->bytes in))))
  (cond
    [(not (hash? response)) (raise-web3-error method "malformed response ~e" response)]
    [(not (answers? id response))
     (raise-web3-error method "response id ~e does not answer request ~a"
                       (hash-ref response 'id 'null)
                       id)]
    [else response]))

;; JSON-RPC lets an error carry a null id when the request's was unreadable.
(define (answers? id response)
  (match* ((hash-ref response 'id 'null) (hash-ref response 'error 'null))
    [((== id) _) #t]
    [('null error) (not (eq? error 'null))]
    [(_ _) #f]))

(define (response->result method response)
  (match (hash-ref response 'error 'null)
    ['null (hash-ref response 'result
                     (lambda () (raise-web3-error method "malformed response ~e" response)))]
    [error (raise-error-object method error)]))

;; Nodes do not always send well-formed error objects; keep what fits the
;; exn:fail:web3:rpc fields.
(define (raise-error-object method e)
  (define (field key ok?)
    (define v (if (hash? e) (hash-ref e key #f) #f))
    (and (ok? v) v))
  (raise-rpc-error method
                   (field 'code exact-integer?)
                   (or (field 'message string?) (format "~a" e))
                   (field 'data jsexpr?)))

;;;; mock

(define mock-handler/c
  (or/c (-> string? list? jsexpr?)
        (hash/c string? (or/c (-> list? jsexpr?) jsexpr?))))

(struct mock-provider (handler log)
  #:constructor-name new-mock-provider
  #:omit-define-syntaxes
  #:methods gen:provider
  [(define (provider-request p method params)
     (record-call! (mock-provider-log p) (cons method params))
     (mock-answer (mock-provider-handler p) method params))])

(define (mock-provider handler)
  (new-mock-provider handler (box '())))

(define (record-call! log call)
  (define calls (unbox log))
  (unless (box-cas! log calls (cons call calls))
    (record-call! log call)))

(define (mock-answer handler method params)
  (define answer
    (cond
      [(hash? handler)
       (hash-ref handler method (lambda () (raise-rpc-error method -32601 "Method not found")))]
      [else (lambda (ps) (handler method ps))]))
  (cond
    [(procedure? answer) (answer params)]
    [else answer]))

(define (mock-provider-calls p)
  (reverse (unbox (mock-provider-log p))))

;;;; calls

(define (rpc-call method params #:provider [provider (current-provider)])
  (cond
    [provider (send-request provider method params)]
    [else (raise-web3-error method "no provider; use with-provider or set current-provider")]))

;;;; endpoints

(begin-for-syntax
  (define-syntax-class arg-spec
    (pattern [contract:expr ->param:expr])
    (pattern contract:expr #:with ->param #'values)))

(define-syntax-parse-rule
  (define-jsonrpc-endpoint rpc-name:id
    ((~datum ->) p:arg-spec ... result-contract:expr)
    (~optional (~seq #:result-transform transform:expr) #:defaults ([transform #'values])))
  #:with fn-name (format-id #'rpc-name "~a" (->kebab-case (syntax-e #'rpc-name)))
  #:with method (datum->syntax #'rpc-name (symbol->string (syntax-e #'rpc-name)))
  #:with (arg ...) (generate-temporaries #'(p ...))
  (define/contract (fn-name arg ... #:provider [provider (current-provider)])
    (->* (p.contract ...) (#:provider (or/c #f provider?)) any)
    (call-endpoint method (list (p.->param arg) ...) provider transform result-contract)))

;; The node's result is untrusted: one the transform rejects or the result
;; contract refuses is the node's fault, not the caller's.
(define (call-endpoint method params provider transform result/c)
  (define ok? (flat-contract-predicate result/c))
  (define raw (rpc-call method params #:provider provider))
  (match (with-handlers ([exn:fail? (lambda (_) #f)])
           (list (transform raw)))
    [(list (? ok? result)) result]
    [_ (raise-web3-error method "unexpected result ~e" raw)]))

;;;; hex

(define (hex-string? v)
  (and (string? v) (regexp-match? #px"^0x[0-9a-fA-F]*$" v)))

(define/contract (hex-string->number s)
  (-> string? exact-nonnegative-integer?)
  (define n
    (cond
      [(string=? s "0x") 0]
      [(hex-string? s) (string->number (substring s 2) 16)]
      [(string-prefix? s "0x") #f]
      [else (string->number s)]))
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'hex-string->number "a hex quantity" s))
  n)

(define/contract (number->hex-string n)
  (-> exact-nonnegative-integer? string?)
  (string-append "0x" (number->string n 16)))

(define/contract (bytes->hex-string bs)
  (-> bytes? string?)
  (string-append
   "0x"
   (apply string-append
          (for/list ([b (in-bytes bs)])
            (string-append (if (< b 16) "0" "") (number->string b 16))))))

(define/contract (hex-string->bytes s)
  (-> string? bytes?)
  (define h (if (string-prefix? s "0x") (substring s 2) s))
  (unless (even? (string-length h))
    (raise-argument-error 'hex-string->bytes "hex string of even length" s))
  (apply bytes
         (for/list ([i (in-range 0 (string-length h) 2)])
           (string->number (substring h i (+ i 2)) 16))))

(module+ test
  (check-equal? (hex-string->number "0x2") 2)
  (check-equal? (hex-string->number "0x") 0)
  (check-equal? (hex-string->number "2") 2)
  (check-exn #rx"expected: a hex quantity" (lambda () (hex-string->number "0xzz")))
  (check-exn #rx"expected: a hex quantity" (lambda () (hex-string->number "1.5")))
  (check-equal? (number->hex-string #xbeef) "0xbeef")
  (check-equal? (bytes->hex-string (bytes 0 15 255)) "0x000fff")
  (check-equal? (hex-string->bytes "0x000fff") (bytes 0 15 255))
  (check-true (hex-string? "0xAb01"))
  (check-false (hex-string? "ab"))
  (check-exn #rx"no provider"
             (lambda () (parameterize ([current-provider #f]) (rpc-call "eth_chainId" '()))))

  (test-case "raise-rpc-error"
    (define e (with-handlers ([values values]) (raise-rpc-error "eth_call" 3 "reverted" "0x08c3")))
    (check-pred exn:fail:web3:rpc? e)
    (check-equal? (exn-message e) "eth_call: reverted")
    (check-equal? (list (exn:fail:web3:rpc-method e)
                        (exn:fail:web3:rpc-code e)
                        (exn:fail:web3:rpc-rpc-message e)
                        (exn:fail:web3:rpc-data e))
                  '("eth_call" 3 "reverted" "0x08c3"))
    (check-false (exn:fail:web3:rpc-data
                  (with-handlers ([values values]) (raise-rpc-error "m" #f "x"))))))
