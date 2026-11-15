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
         current-provider
         with-provider
         define-jsonrpc-endpoint
         hex-string?
         (contract-out
          [provider-request (-> provider? string? (listof jsexpr?) jsexpr?)]
          [provider-close (-> provider? void?)]
          [url->http-provider (-> string? http-provider?)]
          [http-provider-url (-> http-provider? string?)]
          [mock-provider (-> mock-handler/c mock-provider?)]
          [mock-provider-calls (-> mock-provider? (listof (cons/c string? list?)))]
          [raise-rpc-error (->* (string? (or/c #f exact-integer?) string?)
                                ((or/c #f jsexpr?))
                                any)]
          [rpc-call (->* (string? (listof jsexpr?))
                         (#:provider (or/c #f provider?))
                         jsexpr?)])
         hex-string->number
         number->hex-string
         bytes->hex-string
         hex-string->bytes)

(module+ test
  (require rackunit))

;; Errors.  `code`, `message` and `data` are the JSON-RPC error fields; data
;; is where nodes put revert payloads.
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
  #:fallbacks [(define (provider-close _provider) (void))])

(define current-provider
  (make-parameter #f
                  (lambda (p)
                    (unless (or (not p) (provider? p))
                      (raise-argument-error 'current-provider "(or/c #f provider?)" p))
                    p)))

(define-syntax-parse-rule (with-provider provider:expr body:expr ...+)
  (parameterize ([current-provider provider])
    body ...))

;;;; HTTP

;; One HTTP connection per provider, reused across calls (and reopened on
;; demand); the semaphore makes a provider safe to share between threads.
(struct http-provider (url path conn lock)
  #:methods gen:provider
  [(define (provider-request p method params)
     (http-request p method params))
   (define (provider-close p)
     (match-define (struct* http-provider ([conn conn] [lock lock])) p)
     (call-with-semaphore lock (lambda () (http-conn-close! conn))))])

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
                 (make-semaphore 1)))

(define request-id-lock (make-semaphore 1))
(define last-request-id 0)

(define (next-request-id!)
  (call-with-semaphore request-id-lock
                       (lambda ()
                         (set! last-request-id (add1 last-request-id))
                         last-request-id)))

(define (http-request p method params)
  (match-define (struct* http-provider ([path path] [conn conn] [lock lock])) p)
  (define body
    (jsexpr->bytes (hasheq 'jsonrpc "2.0"
                           'id (next-request-id!)
                           'method method
                           'params params)))
  (define response
    (with-handlers ([exn:fail? (lambda (e) (raise-web3-error method "~a" (exn-message e)))])
      (call-with-semaphore
       lock
       (lambda ()
         (define-values (_status _headers in)
           (http-conn-sendrecv! conn path
                                #:method #"POST"
                                #:headers '("Content-Type: application/json")
                                #:data body))
         (bytes->jsexpr (port->bytes in))))))
  (cond
    [(not (hash? response)) (raise-web3-error method "malformed response ~e" response)]
    [(hash-ref response 'error #f) => (lambda (e) (raise-error-object method e))]
    [else (hash-ref response 'result 'null)]))

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

;; `log` is newest first; one lock for every mock keeps the constructor
;; one-argument, and recording a call is too short to contend.
(define mock-log-lock (make-semaphore 1))

(struct mock-provider (handler [log #:auto #:mutable])
  #:auto-value '()
  #:methods gen:provider
  [(define (provider-request p method params)
     (call-with-semaphore mock-log-lock
                          (lambda ()
                            (set-mock-provider-log! p (cons (cons method params)
                                                            (mock-provider-log p)))))
     (mock-answer (mock-provider-handler p) method params))])

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
  (reverse (mock-provider-log p)))

;;;; calls

(define (rpc-call method params #:provider [provider (current-provider)])
  (cond
    [provider (provider-request provider method params)]
    [else (raise-web3-error method "no provider; use with-provider or set current-provider")]))

;;;; endpoints

;; (define-jsonrpc-endpoint eth_getBalance
;;   (-> string? [block-param/c block-param->string] exact-nonnegative-integer?)
;;   #:result-transform hex-string->number)
;;
;; defines (eth-get-balance address block #:provider [p (current-provider)]).
;; Each argument spec is a contract, optionally paired with a conversion to
;; the JSON-RPC parameter; the last spec is the result's contract.
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
    (->* (p.contract ...) (#:provider (or/c #f provider?)) result-contract)
    (transform (rpc-call method (list (p.->param arg) ...) #:provider provider))))

;;;; hex

(define (hex-string? v)
  (and (string? v) (regexp-match? #px"^0x[0-9a-fA-F]*$" v)))

(define/contract (hex-string->number s)
  (-> string? exact-nonnegative-integer?)
  (if (string-prefix? s "0x")
      (if (= (string-length s) 2) 0 (string->number (substring s 2) 16))
      (string->number s)))

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
  (check-equal? (number->hex-string #xbeef) "0xbeef")
  (check-equal? (bytes->hex-string (bytes 0 15 255)) "0x000fff")
  (check-equal? (hex-string->bytes "0x000fff") (bytes 0 15 255))
  (check-true (hex-string? "0xAb01"))
  (check-false (hex-string? "ab"))
  (check-exn #rx"no provider"
             (lambda () (parameterize ([current-provider #f]) (rpc-call "eth_chainId" '())))))

(module+ test
  (require racket/list
           racket/tcp)

  (test-case "request ids are distinct across threads"
    (define ids (make-channel))
    (for ([_ (in-range 8)])
      (thread (lambda () (for ([_ (in-range 50)]) (channel-put ids (next-request-id!))))))
    (define seen (for/list ([_ (in-range 400)]) (channel-get ids)))
    (check-false (check-duplicates seen)))

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
                  (with-handlers ([values values]) (raise-rpc-error "m" #f "x")))))

  ;; A node that answers each request with the next of `bodies`, verbatim,
  ;; so the HTTP envelope handling is tested without anvil.  `(seen)` returns
  ;; the requests received and the number of connections accepted.
  (define (call-with-canned-node bodies proc)
    (define listener (tcp-listen 0 4 #t "127.0.0.1"))
    (define-values (_host port _rhost _rport) (tcp-addresses listener #t))
    (define requests '())
    (define connections 0)
    (define (read-request in)
      (define length
        (for/fold ([n 0])
                  ([line (in-lines in 'return-linefeed)]
                   #:break (string=? line ""))
          (match (regexp-match #px"^(?i:content-length): *([0-9]+)" line)
            [(list _ digits) (string->number digits)]
            [_ n])))
      (set! requests (append requests (list (bytes->jsexpr (read-bytes length in))))))
    (define (serve in out bodies)
      (cond
        [(or (null? bodies) (eof-object? (peek-byte in))) bodies]
        [else
         (read-request in)
         (define body (string->bytes/utf-8 (car bodies)))
         (write-bytes (bytes-append #"HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n"
                                    (string->bytes/utf-8
                                     (format "Content-Length: ~a\r\n\r\n" (bytes-length body)))
                                    body)
                      out)
         (flush-output out)
         (serve in out (cdr bodies))]))
    (define server
      (thread
       (lambda ()
         (let accept ([bodies bodies])
           (define-values (in out) (tcp-accept listener))
           (set! connections (add1 connections))
           (define rest (serve in out bodies))
           (close-input-port in)
           (close-output-port out)
           (unless (null? rest) (accept rest))))))
    (define p (url->http-provider (format "http://127.0.0.1:~a/" port)))
    (dynamic-wind
      void
      (lambda () (proc p (lambda () (values requests connections))))
      (lambda ()
        (provider-close p)
        (kill-thread server)
        (tcp-close listener))))

  (define (raised thunk)
    (with-handlers ([exn:fail:web3? values]) (thunk) #f))

  (test-case "HTTP: the request envelope and the result"
    (call-with-canned-node
     '("{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0x7a69\"}"
       "{\"jsonrpc\":\"2.0\",\"id\":2}")
     (lambda (p seen)
       (check-equal? (rpc-call "eth_getBalance" '("0xabc" "latest") #:provider p) "0x7a69")
       (check-equal? (provider-request p "eth_syncing" '()) 'null)
       (define-values (requests _connections) (seen))
       (match-define (list first-request second-request) requests)
       (check-equal? (hash-ref first-request 'jsonrpc) "2.0")
       (check-equal? (hash-ref first-request 'method) "eth_getBalance")
       (check-equal? (hash-ref first-request 'params) '("0xabc" "latest"))
       (check-true (< (hash-ref first-request 'id) (hash-ref second-request 'id))))))

  (test-case "HTTP: error responses become exn:fail:web3:rpc"
    (call-with-canned-node
     '("{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":3,\"message\":\"execution reverted\",\"data\":\"0x08c3\"}}"
       "{\"jsonrpc\":\"2.0\",\"id\":2,\"error\":\"boom\"}"
       "{\"jsonrpc\":\"2.0\",\"id\":3,\"error\":{\"code\":\"x\",\"message\":7}}")
     (lambda (p _seen)
       (define e (raised (lambda () (rpc-call "eth_call" '() #:provider p))))
       (check-pred exn:fail:web3:rpc? e)
       (check-equal? (exn-message e) "eth_call: execution reverted")
       (check-equal? (list (exn:fail:web3:rpc-code e) (exn:fail:web3:rpc-data e)) '(3 "0x08c3"))
       (define bare (raised (lambda () (rpc-call "eth_call" '() #:provider p))))
       (check-equal? (list (exn:fail:web3:rpc-code bare)
                           (exn:fail:web3:rpc-rpc-message bare)
                           (exn:fail:web3:rpc-data bare))
                     '(#f "boom" #f))
       (define odd (raised (lambda () (rpc-call "eth_call" '() #:provider p))))
       (check-pred exn:fail:web3:rpc? odd)
       (check-false (exn:fail:web3:rpc-code odd)))))

  (test-case "HTTP: undeliverable responses become exn:fail:web3"
    (call-with-canned-node
     '("<html>" "[1]")
     (lambda (p _seen)
       (for ([_ (in-range 2)])
         (define e (raised (lambda () (rpc-call "eth_chainId" '() #:provider p))))
         (check-pred exn:fail:web3? e)
         (check-false (exn:fail:web3:rpc? e))
         (check-regexp-match #rx"^eth_chainId: " (exn-message e))))))

  (test-case "HTTP: provider-close, then the next request reconnects"
    (call-with-canned-node
     '("{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0x1\"}"
       "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":\"0x2\"}")
     (lambda (p seen)
       (check-equal? (rpc-call "eth_blockNumber" '() #:provider p) "0x1")
       (provider-close p)
       (check-equal? (rpc-call "eth_blockNumber" '() #:provider p) "0x2")
       (define-values (_requests connections) (seen))
       (check-equal? connections 2)))))
