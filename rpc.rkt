#lang racket/base

;; JSON-RPC transport: providers, structured errors, hex helpers, and the
;; `define-jsonrpc-endpoint` form that eth.rkt and anvil.rkt are built from.

(require json
         net/http-client
         net/url
         racket/contract
         racket/port
         racket/string
         syntax/parse/define
         (for-syntax casemate
                     racket/base
                     racket/syntax))

(provide (struct-out exn:fail:web3)
         (struct-out exn:fail:web3:rpc)
         http-provider?
         http-provider-url
         url->http-provider
         current-provider
         with-provider
         rpc-call
         define-jsonrpc-endpoint
         hex-string?
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

;;;; providers

;; One HTTP connection per provider, reused across calls (and reopened on
;; demand); the semaphore makes a provider safe to share between threads.
(struct http-provider (url path conn lock))

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

(define current-provider
  (make-parameter #f
                  (lambda (p)
                    (unless (or (not p) (http-provider? p))
                      (raise-argument-error 'current-provider "(or/c #f http-provider?)" p))
                    p)))

(define-syntax-parse-rule (with-provider provider:expr body:expr ...+)
  (parameterize ([current-provider provider])
    body ...))

;;;; calls

(define request-ids 0)

;; rpc-call : string (listof jsexpr) -> jsexpr
;; The `result` of a JSON-RPC call; raises exn:fail:web3:rpc on an error
;; response.
(define (rpc-call method params #:provider [provider (current-provider)])
  (unless provider
    (raise (exn:fail:web3
            (format "~a: no provider; use with-provider or set current-provider" method)
            (current-continuation-marks))))
  (set! request-ids (add1 request-ids))
  (define response
    (call-with-semaphore
     (http-provider-lock provider)
     (lambda ()
       (define-values (_status _headers in)
         (http-conn-sendrecv! (http-provider-conn provider)
                              (http-provider-path provider)
                              #:method #"POST"
                              #:headers '("Content-Type: application/json")
                              #:data (jsexpr->bytes
                                      (hasheq 'jsonrpc "2.0"
                                              'id request-ids
                                              'method method
                                              'params params))))
       (bytes->jsexpr (port->bytes in)))))
  (cond
    [(not (hash? response))
     (raise (exn:fail:web3 (format "~a: malformed response ~e" method response)
                           (current-continuation-marks)))]
    [(hash-ref response 'error #f)
     => (lambda (e)
          (define message (if (hash? e) (hash-ref e 'message "") (format "~a" e)))
          (raise (exn:fail:web3:rpc (format "~a: ~a" method message)
                                    (current-continuation-marks)
                                    method
                                    (and (hash? e) (hash-ref e 'code #f))
                                    message
                                    (and (hash? e) (hash-ref e 'data #f)))))]
    [else (hash-ref response 'result 'null)]))

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
    (->* (p.contract ...) (#:provider (or/c #f http-provider?)) result-contract)
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
