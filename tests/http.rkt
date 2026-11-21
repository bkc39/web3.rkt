#lang racket/base

;; The HTTP provider against a canned node: a local server that answers
;; each request with the next scripted reply, so the envelope, reconnection
;; and interrupted requests are tested without anvil.

(module+ test
  (require json
           openssl
           racket/list
           racket/match
           racket/string
           racket/tcp
           rackunit
           web3
           (only-in "util.rkt" raised))

  ;; A reply is a body string, a procedure from the request to one, or one
  ;; of these wrapping a reply.  A `closing` reply ends its connection and
  ;; then posts `closed`; a `held-body` reply pauses after half its body.
  (struct held (arrived release reply))
  (struct held-body (arrived release reply))
  (struct closing (reply closed))

  (define (result-body id result)
    (jsexpr->string (hasheq 'jsonrpc "2.0" 'id id 'result result)))

  (define (within seconds thunk)
    (define outcome #f)
    (define worker
      (thread (lambda ()
                (set! outcome
                      (with-handlers ([(lambda (_) #t) (lambda (e) (lambda () (raise e)))])
                        (call-with-values thunk (lambda vs (lambda () (apply values vs)))))))))
    (cond
      [(sync/timeout seconds worker) (outcome)]
      [else
       (kill-thread worker)
       (fail (format "no answer within ~as" seconds))]))

  (define (read-request in)
    (define length
      (for/fold ([n 0])
                ([line (in-lines in 'return-linefeed)]
                 #:break (string=? line ""))
        (match (regexp-match #px"^(?i:content-length): *([0-9]+)" line)
          [(list _ digits) (string->number digits)]
          [_ n])))
    (bytes->jsexpr (read-bytes length in)))

  ;; The pause between head and body catches a client that reads a
  ;; response another thread is waiting for.
  (define (respond! out request reply)
    (match reply
      [(held arrived release reply)
       (semaphore-post arrived)
       (sync release)
       (respond! out request reply)]
      [(closing reply _closed) (respond! out request reply)]
      [(? procedure?) (respond! out request (reply request))]
      [(? string?) (write-response! out reply void)]
      [(held-body arrived release reply)
       (write-response! out reply (lambda ()
                                    (semaphore-post arrived)
                                    (sync release)))]))

  (define (write-response! out reply pause)
    (define body (string->bytes/utf-8 reply))
    (define half (quotient (bytes-length body) 2))
    (fprintf out "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: ~a\r\n\r\n"
             (bytes-length body))
    (flush-output out)
    (sleep 0.001)
    (write-bytes body out 0 half)
    (flush-output out)
    (pause)
    (write-bytes body out half)
    (flush-output out))

  (define test-pem (collection-file-path "test.pem" "openssl"))

  (define server-context
    (let ([context (ssl-make-server-context 'auto)])
      (ssl-load-certificate-chain! context test-pem)
      (ssl-load-private-key! context test-pem)
      context))

  (define (server-ports tls? in out)
    (cond
      [(not tls?) (values in out)]
      [else
       (with-handlers ([exn:fail? (lambda (_)
                                    (tcp-abandon-port in)
                                    (tcp-abandon-port out)
                                    (values #f #f))])
         (ports->ssl-ports in out #:mode 'accept #:context server-context #:close-original? #t))]))

  (define (call-with-canned-node replies proc
                                 #:tls? [tls? #f]
                                 #:connect [connect url->http-provider]
                                 #:path [path "/"])
    (define server-custodian (make-custodian))
    (define requests '())
    (define connections 0)
    (define (next-request in replies)
      (with-handlers ([exn:fail? (lambda (_) eof)])
        (if (or (null? replies) (eof-object? (peek-byte in)))
            eof
            (read-request in))))
    (define (serve in out replies)
      (match (next-request in replies)
        [(? eof-object?) (values replies void)]
        [request
         (set! requests (append requests (list request)))
         (with-handlers ([exn:fail? void])
           (respond! out request (car replies)))
         (match (car replies)
           [(closing _reply closed) (values (cdr replies) (lambda () (semaphore-post closed)))]
           [_ (serve in out (cdr replies))])]))
    (define port
      (parameterize ([current-custodian server-custodian])
        (define listener (tcp-listen 0 4 #t "127.0.0.1"))
        (thread
         (lambda ()
           (let accept ([replies replies])
             (define-values (tcp-in tcp-out) (tcp-accept listener))
             (set! connections (add1 connections))
             (define-values (in out) (server-ports tls? tcp-in tcp-out))
             (cond
               [in
                (define-values (rest closed!) (serve in out replies))
                (close-input-port in)
                (with-handlers ([exn:fail? void]) (close-output-port out))
                (closed!)
                (unless (null? rest) (accept rest))]
               [else (accept replies)]))))
        (define-values (_host port _rhost _rport) (tcp-addresses listener #t))
        port))
    (define p (connect (format "~a://127.0.0.1:~a~a" (if tls? "https" "http") port path)))
    (dynamic-wind
      void
      (lambda () (within 20 (lambda () (proc p (lambda () (values requests connections))))))
      (lambda ()
        (custodian-shutdown-all server-custodian)
        (within 5 (lambda () (provider-close p))))))

  (test-case "HTTP: the request envelope and the result"
    (call-with-canned-node
     '("{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0x7a69\"}"
       "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":null}")
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
       "{\"jsonrpc\":\"2.0\",\"id\":3,\"error\":{\"code\":\"x\",\"message\":7}}"
       "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32700,\"message\":\"Parse error\"}}")
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
       (check-false (exn:fail:web3:rpc-code odd))
       (define unread (raised (lambda () (rpc-call "eth_call" '() #:provider p))))
       (check-equal? (exn:fail:web3:rpc-code unread) -32700))))

  (test-case "HTTP: a null error is no error"
    (call-with-canned-node
     '("{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0x1\",\"error\":null}")
     (lambda (p _seen)
       (check-equal? (rpc-call "eth_blockNumber" '() #:provider p) "0x1"))))

  (test-case "HTTP: undeliverable responses become exn:fail:web3"
    (call-with-canned-node
     '("<html>"
       "[1]"
       "{\"jsonrpc\":\"2.0\",\"id\":3}"
       "{\"jsonrpc\":\"2.0\",\"id\":null,\"result\":\"0x1\"}")
     (lambda (p _seen)
       (for ([_ (in-range 4)])
         (define e (raised (lambda () (rpc-call "eth_chainId" '() #:provider p))))
         (check-pred exn:fail:web3? e)
         (check-false (exn:fail:web3:rpc? e))
         (check-regexp-match #rx"^eth_chainId: " (exn-message e))))))

  (test-case "HTTP: a response to another request is refused"
    (call-with-canned-node
     (list (result-body 99 "0x63") (result-body 2 "0x2"))
     (lambda (p _seen)
       (define e (raised (lambda () (rpc-call "eth_blockNumber" '() #:provider p))))
       (check-pred exn:fail:web3? e)
       (check-regexp-match #rx"response id 99 does not answer request 1" (exn-message e))
       (check-equal? (rpc-call "eth_blockNumber" '() #:provider p) "0x2"))))

  (test-case "HTTP: provider-close, then the next request reconnects"
    (call-with-canned-node
     (list (result-body 1 "0x1") (result-body 2 "0x2"))
     (lambda (p seen)
       (check-equal? (rpc-call "eth_blockNumber" '() #:provider p) "0x1")
       (provider-close p)
       (check-equal? (rpc-call "eth_blockNumber" '() #:provider p) "0x2")
       (define-values (_requests connections) (seen))
       (check-equal? connections 2))))

  (test-case "HTTP: a keep-alive connection the node closed is reopened"
    (define closed (make-semaphore))
    (call-with-canned-node
     (list (closing (result-body 1 "0x1") closed) (result-body 2 "0x2"))
     (lambda (p seen)
       (check-equal? (rpc-call "eth_blockNumber" '() #:provider p) "0x1")
       (sync closed)
       (check-equal? (rpc-call "eth_blockNumber" '() #:provider p) "0x2")
       (define-values (requests connections) (seen))
       (check-equal? (list (length requests) connections) '(2 2)))))

  (define (interrupted-request stop! [hold held])
    (define arrived (make-semaphore))
    (define release (make-semaphore))
    (call-with-canned-node
     (list (hold arrived release (result-body 1 "0x1")) (result-body 2 "0x2"))
     (lambda (p _seen)
       (define caller
         (thread (lambda ()
                   (with-handlers ([exn:break? void])
                     (rpc-call "eth_blockNumber" '() #:provider p)))))
       (sync arrived)
       (sync (system-idle-evt))
       (stop! caller)
       (semaphore-post release)
       (within 2 (lambda () (rpc-call "eth_blockNumber" '() #:provider p))))))

  (test-case "HTTP: a broken request's late reply is not the next one's"
    (check-equal? (interrupted-request (lambda (caller)
                                         (break-thread caller)
                                         (thread-wait caller)))
                  "0x2"))

  (test-case "HTTP: a request broken while reading its body leaves the next one alone"
    (check-equal? (interrupted-request (lambda (caller)
                                         (break-thread caller)
                                         (thread-wait caller))
                                       held-body)
                  "0x2"))

  (test-case "HTTP: a request killed mid-exchange does not block the next"
    (check-equal? (interrupted-request kill-thread) "0x2"))

  (test-case "HTTP: a request killed while reading its body does not block the next"
    (check-equal? (interrupted-request kill-thread held-body) "0x2"))

  (define (check-shared-provider #:tls? [tls? #f] #:connect [connect url->http-provider])
    (define (echo request)
      (result-body (hash-ref request 'id) (car (hash-ref request 'params))))
    (define (questions t)
      (for/list ([i (in-range 5)]) (format "~a.~a" t i)))
    (call-with-canned-node
     (make-list 40 echo)
     #:tls? tls?
     #:connect connect
     (lambda (p seen)
       (define answers (make-vector 8 #f))
       (define askers
         (for/list ([t (in-range 8)])
           (thread (lambda ()
                     (vector-set! answers t
                                  (for/list ([q (in-list (questions t))])
                                    (with-handlers ([exn:fail:web3? exn-message])
                                      (rpc-call "echo" (list q) #:provider p))))))))
       (for-each thread-wait askers)
       (check-equal? (vector->list answers) (map questions (range 8)))
       (define-values (requests _connections) (seen))
       (check-equal? (length requests) 40)
       (check-false (check-duplicates (map (lambda (r) (hash-ref r 'id)) requests))))))

  (test-case "HTTP: threads sharing a provider each get their own answer"
    (check-shared-provider))

  (define (roots->sources roots)
    (match roots
      ['none '()]
      ['test-pem (list test-pem)]))

  (define (client-context roots host-names)
    (define context (ssl-make-client-context 'auto))
    (define sources (roots->sources roots))
    (for ([source (in-list sources)])
      (ssl-load-verify-source! context source))
    (ssl-set-verify! context (pair? sources))
    (ssl-set-verify-hostname! context (eq? host-names 'checked))
    context)

  (define (connector spec)
    (match spec
      [`(default ,roots)
       (lambda (url)
         (parameterize ([ssl-default-verify-sources (roots->sources roots)])
           (url->http-provider url)))]
      [`(explicit ,roots ,host-names)
       (lambda (url) (url->http-provider url #:ssl-context (client-context roots host-names)))]))

  (define refusal-texts
    (hash 'chain #rx"certificate verify failed"
          'host-name #rx"certificate verify failed|certificate not valid for hostname"))

  (define (refused p [refusal 'chain])
    (define e (raised (lambda () (rpc-call "eth_chainId" '() #:provider p))))
    (check-pred exn:fail:web3? e)
    (check-false (exn:fail:web3:rpc? e))
    (check-regexp-match #px"^eth_chainId: TLS handshake with 127[.]0[.]0[.]1:[0-9]+ failed: "
                        (exn-message e))
    (check-regexp-match (hash-ref refusal-texts refusal) (exn-message e))
    e)

  (define ((check-refuses refusal) p seen)
    (refused p refusal)
    (define-values (requests _connections) (seen))
    (check-equal? requests '()))

  (define (check-accepts p _seen)
    (check-equal? (rpc-call "eth_chainId" '() #:provider p) "0x7a69"))

  (define outcome-checks
    (hash 'accepts check-accepts
          'refuses/chain (check-refuses 'chain)
          'refuses/host-name (check-refuses 'host-name)))

  (test-case "HTTPS: which contexts accept a self-signed node"
    (for ([row (in-list '(("no trusted roots" (default none) refuses/chain)
                          ("the node's certificate trusted, its host name wrong"
                           (default test-pem) refuses/host-name)
                          ("a context trusting the certificate, host names unchecked"
                           (explicit test-pem unchecked) accepts)
                          ("a context trusting the certificate, host names checked"
                           (explicit test-pem checked) refuses/host-name)
                          ("a context that checks nothing" (explicit none unchecked) accepts)))])
      (match-define (list name spec outcome) row)
      (test-case name
        (call-with-canned-node
         (list (result-body 1 "0x7a69"))
         #:tls? #t
         #:connect (connector spec)
         (hash-ref outcome-checks outcome)))))

  (test-case "HTTPS: a context the caller changes applies to the next connection"
    (define context (ssl-make-client-context 'auto))
    (ssl-set-verify! context #t)
    (call-with-canned-node
     (list (result-body 2 "0x7a69"))
     #:tls? #t
     #:connect (lambda (url) (url->http-provider url #:ssl-context context))
     (lambda (p seen)
       (refused p)
       (ssl-load-verify-source! context test-pem)
       (check-equal? (rpc-call "eth_chainId" '() #:provider p) "0x7a69")
       (define-values (requests connections) (seen))
       (check-equal? (list (length requests) connections) '(1 2)))))

  (test-case "HTTPS: every connection uses the provider's context"
    (define closed (make-semaphore))
    (call-with-canned-node
     (list (result-body 1 "0x1") (closing (result-body 2 "0x2") closed) (result-body 3 "0x3"))
     #:tls? #t
     #:connect (connector '(explicit test-pem unchecked))
     (lambda (p seen)
       (check-equal? (rpc-call "eth_blockNumber" '() #:provider p) "0x1")
       (provider-close p)
       (check-equal? (rpc-call "eth_blockNumber" '() #:provider p) "0x2")
       (sync closed)
       (check-equal? (rpc-call "eth_blockNumber" '() #:provider p) "0x3")
       (define-values (requests connections) (seen))
       (check-equal? (list (length requests) connections) '(3 3))))
    (call-with-canned-node
     '()
     #:tls? #t
     #:connect (connector '(default none))
     (lambda (p seen)
       (refused p)
       (refused p)
       (provider-close p)
       (refused p)
       (define-values (requests connections) (seen))
       (check-equal? (list (length requests) connections) '(0 3)))))

  (test-case "HTTPS: a refused handshake names the host and port, not the URL"
    (call-with-canned-node
     '()
     #:tls? #t
     #:connect (connector '(default none))
     #:path "/v3/s3cr3t-key?apikey=hunter2"
     (lambda (p _seen)
       (define message (exn-message (refused p)))
       (match-define (list _ port) (regexp-match #px":([0-9]+)/" (http-provider-url p)))
       (check-true (string-contains? message (format "127.0.0.1:~a" port)))
       (check-false (string-contains? message "s3cr3t-key"))
       (check-false (string-contains? message "hunter2")))))

  (define (open-ports custodian)
    (filter port? (custodian-managed-list custodian (current-custodian))))

  (test-case "HTTPS: a refused handshake leaves no port open"
    (call-with-canned-node
     '()
     #:tls? #t
     #:connect (connector '(default none))
     (lambda (p _seen)
       (define requester (make-custodian))
       (parameterize ([current-custodian requester])
         (refused p))
       (check-equal? (open-ports requester) '()))))

  (test-case "HTTPS: a request broken during the handshake raises the break, leaving no port open"
    (define server (make-custodian))
    (define accepted (make-semaphore))
    (define port
      (parameterize ([current-custodian server])
        (define listener (tcp-listen 0 4 #t "127.0.0.1"))
        (thread (lambda ()
                  (tcp-accept listener)
                  (semaphore-post accepted)
                  (sync never-evt)))
        (define-values (_host port _rhost _rport) (tcp-addresses listener #t))
        port))
    (define p ((connector '(explicit none unchecked)) (format "https://127.0.0.1:~a/" port)))
    (define requester (make-custodian))
    (define outcome #f)
    (define caller
      (parameterize ([current-custodian requester])
        (thread (lambda ()
                  (set! outcome (with-handlers ([exn:break? values])
                                  (rpc-call "eth_chainId" '() #:provider p)))))))
    (sync accepted)
    (sync (system-idle-evt))
    (break-thread caller)
    (within 5 (lambda () (thread-wait caller)))
    (custodian-shutdown-all server)
    (check-pred exn:break? outcome)
    (check-equal? (open-ports requester) '()))

  (test-case "HTTPS: a refused TCP connect is not a TLS failure"
    (define listener (tcp-listen 0 4 #t "127.0.0.1"))
    (define-values (_host port _rhost _rport) (tcp-addresses listener #t))
    (tcp-close listener)
    (define p ((connector '(explicit none unchecked)) (format "https://127.0.0.1:~a/" port)))
    (define e (raised (lambda () (rpc-call "eth_chainId" '() #:provider p))))
    (check-pred exn:fail:web3? e)
    (check-regexp-match #rx"^eth_chainId: tcp-connect" (exn-message e))
    (check-false (regexp-match? #rx"TLS handshake" (exn-message e))))

  (test-case "HTTPS: a trusting context serves concurrent callers"
    (check-shared-provider #:tls? #t #:connect (connector '(explicit test-pem unchecked)))))
