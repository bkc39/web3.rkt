#lang racket/base

;; JSON-RPC transport: providers, structured errors, hex helpers, and the
;; `define-jsonrpc-endpoint` form that eth.rkt and anvil.rkt are built from.

(require json
         net/http-client
         net/url
         openssl
         racket/contract
         racket/generic
         racket/match
         racket/port
         racket/string
         racket/tcp
         (only-in syntax/location quote-module-name quote-srcloc)
         syntax/parse/define
         "private/exn.rkt"
         "private/hex.rkt"
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
          [url->http-provider
           (->i ([url string?])
                (#:ssl-context [ssl-context (url) (if (https-url? url)
                                                      ssl-client-context?
                                                      ssl-context-needs-https)])
                [_ http-provider?])]
          [http-provider-url (-> http-provider? string?)]
          [mock-provider (-> mock-handler/c mock-provider?)]
          [mock-provider-calls (-> mock-provider? (listof (cons/c string? list?)))]
          [raise-rpc-error (->* (string? (or/c #f exact-integer?) string?)
                                ((or/c #f jsexpr?))
                                any)]
          [rpc-call (->* (string? (listof jsexpr?))
                         (#:provider (or/c #f provider?))
                         any)]
          [hex-string->number (-> hex-string? exact-nonnegative-integer?)])
         number->hex-string
         bytes->hex-string
         hex-string->bytes)

(module+ test
  (require rackunit))

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

(struct http-provider (url path host port ssl-context link owner last-id)
  #:methods gen:provider
  [(define (provider-request p method params)
     (response->result method (call-with-connection p (lambda () (exchange p method params)))))
   (define (provider-close p)
     (call-with-connection p (lambda () (drop-link! p))))])

(define (https-scheme? u)
  (equal? "https" (url-scheme u)))

(define (https-url? url-string)
  (https-scheme? (string->url url-string)))

(define ssl-context-needs-https
  (flat-named-contract "#:ssl-context needs an https URL" (lambda (_) #f)))

(define (url->http-provider url-string #:ssl-context [ssl-context #f])
  (define u (string->url url-string))
  (define https? (https-scheme? u))
  (define path
    (cond
      [(regexp-match #px"^[a-zA-Z]+://[^/]+(/.+)$" url-string) => cadr]
      [else "/"]))
  (http-provider url-string
                 path
                 (url-host u)
                 (or (url-port u) (if https? 443 80))
                 (provider-ssl-context https? ssl-context)
                 (box #f)
                 (box #f)
                 (box 0)))

(define (provider-ssl-context https? ssl-context)
  (cond
    [(not https?) #f]
    [ssl-context ssl-context]
    [(not ssl-available?)
     (raise-web3-error 'url->http-provider "TLS is not available: ~a" ssl-load-fail-reason)]
    [else (ssl-secure-client-context)]))

;; `link` holds the open connection, or #f.  A connection is never reopened
;; in place: net/http-client's body readers keep the `http-conn` and close
;; it when their read fails, so a dropped connection takes its struct along.
(struct link (conn in))

;; `owner` holds the `holder` of the request using the connection.  A
;; request killed mid-exchange never releases it: the next one sees the
;; holder's thread dead, takes over, and drops the half-read connection.
(struct holder (thread released))

(define (call-with-connection p proc)
  (define owner (http-provider-owner p))
  (define me (holder (current-thread) (make-semaphore 0)))
  (define breaks? (break-enabled))
  (define finished? #f)
  (parameterize-break #f
    (take-connection! p me breaks?)
    (dynamic-wind
      void
      (lambda ()
        (begin0 (parameterize-break breaks? (proc))
          (set! finished? #t)))
      (lambda ()
        (unless finished? (drop-link! p))
        (set-box! owner #f)
        (semaphore-post (holder-released me))))))

(define (take-connection! p me breaks?)
  (define owner (http-provider-owner p))
  (define wait (if breaks? sync/enable-break sync))
  (let retry ()
    (define held (unbox owner))
    (cond
      [(not held) (unless (box-cas! owner #f me) (retry))]
      [(not (thread-dead? (holder-thread held)))
       (wait (thread-dead-evt (holder-thread held)) (semaphore-peek-evt (holder-released held)))
       (retry)]
      [(box-cas! owner held me) (drop-link! p)]
      [else (retry)])))

;; An idle connection has nothing to read: a byte or an EOF there means the
;; node has closed it, or will.
(define (connection! p)
  (define current (unbox (http-provider-link p)))
  (cond
    [(and current (idle? current)) (link-conn current)]
    [else
     (drop-link! p)
     (open-link! p)]))

(define (idle? l)
  (and (http-conn-live? (link-conn l))
       (not (with-handlers ([exn:fail? (lambda (_) #t)])
              (byte-ready? (link-in l))))))

(define (open-link! p)
  (match-define (struct* http-provider ([host host] [port port] [ssl-context ssl-context])) p)
  (define-values (tcp-in tcp-out) (tcp-connect host port))
  (define-values (in out abandon)
    (cond
      [ssl-context
       (define-values (ssl-in ssl-out) (tls-handshake host port ssl-context tcp-in tcp-out))
       (values ssl-in ssl-out ssl-abandon-port)]
      [else (values tcp-in tcp-out tcp-abandon-port)]))
  (define conn (http-conn))
  (http-conn-open! conn host
                   #:port port
                   #:ssl? (list (and ssl-context #t) in out abandon))
  (set-box! (http-provider-link p) (link conn in))
  conn)

;; The message names the host and port only: the URL may carry an API key.
(define (tls-handshake host port ssl-context in out)
  (with-handlers ([(lambda (_) #t)
                   (lambda (e)
                     (tcp-abandon-port in)
                     (tcp-abandon-port out)
                     (raise (tls-failure host port e)))])
    (ports->ssl-ports in out
                      #:mode 'connect
                      #:context ssl-context
                      #:hostname host
                      #:close-original? #t
                      #:shutdown-on-close? #f)))

(define (tls-failure host port raised)
  (cond
    [(exn:fail? raised)
     (exn:fail:web3 (format "TLS handshake with ~a:~a failed: ~a" host port (exn-message raised))
                    (current-continuation-marks))]
    [else raised]))

;; Closing flushes, which fails on a broken connection; abandoning it
;; releases the socket anyway.
(define (drop-link! p)
  (define dropped (unbox (http-provider-link p)))
  (set-box! (http-provider-link p) #f)
  (when dropped
    (with-handlers ([exn:fail? (lambda (_) (http-conn-abandon! (link-conn dropped)))])
      (http-conn-close! (link-conn dropped)))
    (close-input-port (link-in dropped))))

(define (exchange p method params)
  (match-define (struct* http-provider ([path path] [last-id last-id])) p)
  (define id (add1 (unbox last-id)))
  (set-box! last-id id)
  (define body (jsexpr->bytes (hasheq 'jsonrpc "2.0" 'id id 'method method 'params params)))
  (define response
    (with-handlers ([exn:fail? (lambda (e) (raise-web3-error method "~a" (exn-message e)))])
      (define-values (_status _headers in)
        (http-conn-sendrecv! (connection! p) path
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
    (pattern contract:expr #:with ->param #'values))

  (define (expand-endpoint-use endpoint stx)
    (define checked
      #`(#,(endpoint-transformer-checked-for endpoint)
         #,(syntax-local-lift-expression #'(quote-module-name))))
    (syntax-parse stx
      [_:id checked]
      [(_ . args) (quasisyntax/loc stx (#,checked . args))]))

  (struct endpoint-transformer (checked-for) #:property prop:procedure expand-endpoint-use))

(define (endpoint-checker signature procedure positive name srcloc)
  (define checked (make-hash))
  (lambda (negative)
    (hash-ref! checked
               negative
               (lambda () (contract signature procedure positive negative name srcloc)))))

(define-syntax-parse-rule
  (define-jsonrpc-endpoint rpc-name:id
    ((~datum ->) p:arg-spec ... result-contract:expr)
    (~optional (~seq #:result-transform transform:expr) #:defaults ([transform #'values])))
  #:with fn-name (format-id #'rpc-name "~a" (->kebab-case (syntax-e #'rpc-name)))
  #:with method (datum->syntax #'rpc-name (symbol->string (syntax-e #'rpc-name)))
  #:with (arg ...) (generate-temporaries #'(p ...))
  #:with (result? unchecked signature checked-for)
  (generate-temporaries '(result? unchecked signature checked-for))
  #:with procedure
  (syntax-property #'(lambda (arg ... #:provider [provider (current-provider)])
                       (call-endpoint method (list (p.->param arg) ...) provider transform result?))
                   'inferred-name
                   (syntax-e #'fn-name))
  (begin
    (define result? (flat-contract-predicate (coerce-flat-contract 'fn-name result-contract)))
    (define unchecked procedure)
    (define signature (->* (p.contract ...) (#:provider (or/c #f provider?)) any))
    (define checked-for
      (endpoint-checker signature unchecked (quote-module-name) 'fn-name (quote-srcloc rpc-name)))
    (define-syntax fn-name (endpoint-transformer (quote-syntax checked-for)))))

;; The node's result is untrusted: one the transform rejects or the result
;; contract refuses is the node's fault, not the caller's.
(define (call-endpoint method params provider transform ok?)
  (define raw (rpc-call method params #:provider provider))
  (match (with-handlers ([exn:fail:web3? raise]
                         [exn:fail? (lambda (_) #f)])
           (list (transform raw)))
    [(list (? ok? result)) result]
    [_ (raise-web3-error method "unexpected result ~e" raw)]))

;;;; hex

(define (hex-string? v)
  (and (string? v) (regexp-match? #px"^0x[0-9a-fA-F]*$" v)))

(define (hex-string->number s)
  (if (string=? s "0x") 0 (string->number (substring s 2) 16)))

(define/contract (number->hex-string n)
  (-> exact-nonnegative-integer? string?)
  (natural->hex n))

(define/contract (bytes->hex-string bs)
  (-> bytes? string?)
  (bytes->hex bs))

(define/contract (hex-string->bytes s)
  (-> string? bytes?)
  (define h (if (string-prefix? s "0x") s (string-append "0x" s)))
  (unless (even? (string-length h))
    (raise-argument-error 'hex-string->bytes "hex string of even length" s))
  (hex->bytes h))

(module+ test
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

;; White-box: the provider's connection struct is not visible outside.
(module+ test
  (define (serve-requests in out arrived)
    (define length
      (for/fold ([n 0])
                ([line (in-lines in 'return-linefeed)]
                 #:break (string=? line ""))
        (match (regexp-match #px"^(?i:content-length): *([0-9]+)" line)
          [(list _ digits) (string->number digits)]
          [_ n])))
    (define request (bytes->jsexpr (read-bytes length in)))
    (define body (jsexpr->bytes (hasheq 'jsonrpc "2.0" 'id (hash-ref request 'id) 'result "0x1")))
    (fprintf out "HTTP/1.1 200 OK\r\nContent-Length: ~a\r\n\r\n" (bytes-length body))
    (flush-output out)
    (when (equal? (hash-ref request 'method) "hold")
      (semaphore-post arrived)
      (sync never-evt))
    (write-bytes body out)
    (flush-output out)
    (serve-requests in out arrived))

  (test-case "an interrupted request's connection struct is never reused"
    (define server (make-custodian))
    (define arrived (make-semaphore))
    (define port
      (parameterize ([current-custodian server])
        (define listener (tcp-listen 0 4 #t "127.0.0.1"))
        (thread (lambda ()
                  (let accept ()
                    (define-values (in out) (tcp-accept listener))
                    (thread (lambda ()
                              (with-handlers ([exn:fail? void]) (serve-requests in out arrived))))
                    (accept))))
        (define-values (_host port _rhost _rport) (tcp-addresses listener #t))
        port))
    (define p (url->http-provider (format "http://127.0.0.1:~a/" port)))
    (define (connection) (link-conn (unbox (http-provider-link p))))
    (define (run)
      (rpc-call "ok" '() #:provider p)
      (define before (connection))
      (define caller
        (thread (lambda ()
                  (with-handlers ([exn:break? void])
                    (rpc-call "hold" '() #:provider p)))))
      (sync arrived)
      (break-thread caller)
      (thread-wait caller)
      (list (rpc-call "ok" '() #:provider p) (eq? before (connection))))
    (define outcome (make-channel))
    (define runner
      (thread (lambda () (channel-put outcome (with-handlers ([exn:fail? exn-message]) (run))))))
    (check-equal? (sync/timeout 20 outcome) '("0x1" #f))
    (kill-thread runner)
    (custodian-shutdown-all server)
    (provider-close p)))
