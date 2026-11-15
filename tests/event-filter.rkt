#lang racket/base

;; Log filters and fetching logs, against mock providers.

(module+ test
  (require racket/list
           racket/match
           rackunit
           web3
           (only-in (submod web3/event internal) log-limit-error?)
           "abi-gen.rkt"
           "events.rkt"
           "util.rkt")

  (struct filter-json (filter jsexpr) #:prefab)
  (struct chunking (filter chunk node calls) #:prefab)
  (struct halving (filter chunk limit calls) #:prefab)

  (define token "0x5FbDB2315678afecb367f032d93F642f64180aa3")
  (define bob-mixed "0x70997970c51812dc3A010C7d01b50e0d17dc79C8")
  (define block-hash (string-append "0x" (make-string 64 #\c)))
  (define x-topic (string-append "0x" (make-string 64 #\1)))
  (define y-topic (string-append "0x" (make-string 64 #\2)))
  (define default-range '((fromBlock . "earliest") (toBlock . "latest")))

  (define (jsexpr . fields)
    (make-immutable-hasheq (append fields default-range)))

  (define filter-jsexprs
    `(#s(filter-json ,(log-filter) ,(jsexpr))
      #s(filter-json ,(log-filter #:address token) ,(jsexpr (cons 'address token)))
      #s(filter-json ,(log-filter #:address (list token bob-mixed)) ,(jsexpr (cons 'address (list token bob-mixed))))
      #s(filter-json ,(log-filter #:from-block 5 #:to-block 10) #hasheq((fromBlock . "0x5") (toBlock . "0xa")))
      #s(filter-json ,(log-filter #:from-block 0 #:to-block "pending")
                     #hasheq((fromBlock . "0x0") (toBlock . "pending")))
      #s(filter-json ,(log-filter #:block-hash block-hash) #hasheq((blockHash . ,block-hash)))
      #s(filter-json ,(log-filter #:topics (list transfer-topic #f (list x-topic y-topic)))
                     ,(jsexpr (cons 'topics (list transfer-topic 'null (list x-topic y-topic)))))
      #s(filter-json ,(log-filter #:topics '()) ,(jsexpr))
      #s(filter-json ,(log-filter #:event transfer) ,(jsexpr (cons 'topics (list transfer-topic))))
      #s(filter-json ,(log-filter #:event transfer #:where (hasheq 'to bob-mixed))
                     ,(jsexpr (cons 'topics (list transfer-topic 'null (topic-word anvil-bob)))))
      #s(filter-json ,(log-filter #:event note #:where (hasheq 'sender anvil-alice))
                     ,(jsexpr (cons 'topics (list (topic-word anvil-alice)))))
      #s(filter-json ,(log-filter #:event note) ,(jsexpr))
      #s(filter-json ,(log-filter #:event tagged #:where (hasheq 'tag (any-of "a" "b")))
                     ,(jsexpr (cons 'topics (list (event-topic tagged)
                                                  (list (abi-value->topic 'string "a")
                                                        (abi-value->topic 'string "b"))))))))

  (test-case "a filter's JSON has exactly the keys it was given"
    (for ([row (in-list filter-jsexprs)])
      (match-define (struct* filter-json ([filter f] [jsexpr j])) row)
      (check-equal? (log-filter->jsexpr f) j (format "~a" f))))

  (define (random-where prng e)
    (for/hasheq ([p (in-list (abi-event-parameters e))]
                 [i (in-naturals)]
                 #:when (and (abi-event-parameter-indexed? p) (zero? (random 2 prng))))
      (values (string->symbol (or (abi-event-parameter-name p) (format "~a" i)))
              (random-abi-value prng (abi-event-parameter-type p)))))

  (test-case "an event and #:where give the topics event-topics gives"
    (define prng (seeded 4 4 4 1 2 3))
    (for ([e (in-list (append (map string->abi-event (list transfer deposit note bare paired nested))
                              (for/list ([_ (in-range 200)]) (random-abi-event prng))))])
      (define where (for/hasheq ([(k v) (in-hash (random-where prng e))]
                                 #:unless (string->number (symbol->string k)))
                      (values k v)))
      (check-equal? (log-filter->jsexpr (log-filter #:topics (event-topics e where)))
                    (log-filter->jsexpr (log-filter #:event e #:where where))
                    (abi-event->string e))))

  (test-case "log-filter blames its caller"
    (check-blames-here (log-filter #:block-hash block-hash #:from-block 1))
    (check-blames-here (log-filter #:block-hash block-hash #:to-block "latest"))
    (check-blames-here (log-filter #:topics (list transfer-topic) #:event transfer))
    (check-blames-here (log-filter #:where (hasheq 'to anvil-bob)))
    (check-blames-here (log-filter #:from-block 10 #:to-block 9))
    (check-blames-here (log-filter #:topics (list #f #f #f #f #f)))
    (check-blames-here (log-filter #:topics (list "0x12")))
    (check-blames-here (log-filter #:topics (list '())))
    (check-blames-here (log-filter #:address '()))
    (check-blames-here (log-filter #:address "0x12"))
    (check-blames-here (log-filter #:address (list token "x")))
    (check-blames-here (log-filter #:from-block "newest"))
    (check-blames-here (log-filter #:block-hash "0x12"))
    (check-blames-here (log-filter #:event transfer #:where (hasheq 'value 1)))
    (check-equal? (log-filter->jsexpr (log-filter #:block-hash #f #:from-block 1))
                  #hasheq((fromBlock . "0x1") (toBlock . "latest"))))

  (define (log-at block)
    (log-jsexpr #:topics (list transfer-topic (topic-word anvil-alice) (topic-word anvil-bob))
                #:data (word 1)
                #:index block))

  (define (hex->number h)
    (string->number (substring h 2) 16))

  (define (first-block request)
    (match (hash-ref request 'fromBlock #f)
      [(? string? (regexp #rx"^0x" (list _))) (hex->number (hash-ref request 'fromBlock))]
      [_ 0]))

  ;; A node whose eth_getLogs answers with one log per request, its index
  ;; the request's first block, plus the given answers.
  (define (logs-node [answers (hash)] #:fail? [fail? (lambda (from to) #f)] #:error [error limit-error])
    (mock-provider
     (lambda (method params)
       (match* (method params)
         [("eth_getLogs" (list (and request (hash-table ['fromBlock from] ['toBlock to]))))
          (if (fail? from to) (raise error) (list (log-at (first-block request))))]
         [("eth_getLogs" (list request)) (list (log-at (first-block request)))]
         [(_ _) (match (hash-ref answers (cons method params) #f)
                  [#f (raise-rpc-error method -32601 "Method not found")]
                  [answer answer])]))))

  (define (rpc-error method code message)
    (with-handlers ([exn:fail:web3:rpc? values])
      (raise-rpc-error method code message)))

  (define limit-error (rpc-error "eth_getLogs" -32005 "query returned more than 10000 results"))

  (define (call-summary call)
    (match call
      [(list "eth_getLogs" (hash-table ['fromBlock from] ['toBlock to])) (list from to)]
      [(list "eth_getLogs" j) (list j)]
      [(cons method params) (cons method params)]))

  (define transfers (log-filter #:event transfer))

  (define chunked-requests
    `(#s(chunking ,(log-filter #:from-block 0 #:to-block 9) 4 #hash()
                  (("0x0" "0x3") ("0x4" "0x7") ("0x8" "0x9")))
      #s(chunking ,transfers 100 #hash((("eth_blockNumber") . "0xfa"))
                  (("eth_blockNumber") ("0x0" "0x63") ("0x64" "0xc7") ("0xc8" "0xfa")))
      #s(chunking ,(log-filter #:from-block 10 #:to-block 10) 5 #hash() (("0xa" "0xa")))
      #s(chunking ,(log-filter #:from-block 7) 3 #hash((("eth_blockNumber") . "0x5")) (("eth_blockNumber")))
      #s(chunking ,(log-filter #:from-block "safe" #:to-block "finalized") 10
                  #hash((("eth_getBlockByNumber" "safe" #f) . #hasheq((number . "0x3")))
                        (("eth_getBlockByNumber" "finalized" #f) . #hasheq((number . "0x5"))))
                  (("eth_getBlockByNumber" "safe" #f) ("eth_getBlockByNumber" "finalized" #f) ("0x3" "0x5")))
      #s(chunking ,(log-filter #:from-block 0 #:to-block 9) #f #hash() (("0x0" "0x9")))
      #s(chunking ,transfers #f #hash() (("earliest" "latest")))
      #s(chunking ,(log-filter #:block-hash block-hash) 2 #hash() ((#hasheq((blockHash . ,block-hash)))))))

  (test-case "get-logs requests a range in chunks, in order"
    (for ([row (in-list chunked-requests)])
      (match-define (struct* chunking ([filter f] [chunk chunk] [node answers] [calls calls])) row)
      (define node (logs-node answers))
      (define logs (get-logs f #:chunk chunk #:provider node #:events '()))
      (check-equal? (map call-summary (mock-provider-calls node)) calls (format "~a ~a" f chunk))
      (check-equal? (map log-index logs)
                    (for/list ([call (in-list (mock-provider-calls node))]
                               #:when (equal? (first call) "eth_getLogs"))
                      (first-block (second call)))
                    (format "~a ~a" f chunk))))

  (define halved-requests
    `(#s(halving ,(log-filter #:from-block 0 #:to-block 250) 100 50
                 (("0x0" "0x63") ("0x0" "0x31") ("0x32" "0x63") ("0x64" "0x95") ("0x96" "0xc7")
                  ("0xc8" "0xf9") ("0xfa" "0xfa")))
      #s(halving ,(log-filter #:from-block 0 #:to-block 7) 8 1
                 (("0x0" "0x7") ("0x0" "0x3") ("0x0" "0x1") ("0x0" "0x0") ("0x1" "0x1") ("0x2" "0x2")
                  ("0x3" "0x3") ("0x4" "0x4") ("0x5" "0x5") ("0x6" "0x6") ("0x7" "0x7")))))

  (define (span-over limit)
    (lambda (from to)
      (or (not (regexp-match? #rx"^0x" from))
          (> (+ (- (hex->number to) (hex->number from)) 1) limit))))

  (test-case "a range the node refuses as too large is halved, and the span never grows"
    (for ([row (in-list halved-requests)])
      (match-define (struct* halving ([filter f] [chunk chunk] [limit limit] [calls calls])) row)
      (define node (logs-node #:fail? (span-over limit)))
      (get-logs f #:chunk chunk #:provider node)
      (check-equal? (map call-summary (mock-provider-calls node)) calls (format "~a" f)))
    (define whole (logs-node (hash '("eth_blockNumber") "0xfa") #:fail? (span-over 125)))
    (check-equal? (length (get-logs transfers #:provider whole)) 3)
    (check-equal? (map call-summary (mock-provider-calls whole))
                  '(("earliest" "latest") ("eth_blockNumber") ("0x0" "0x7c") ("0x7d" "0xf9") ("0xfa" "0xfa")))
    (define stuck (logs-node #:fail? (lambda (from to) #t)))
    (check-eq? (raised (lambda () (get-logs (log-filter #:from-block 5 #:to-block 5) #:chunk 1 #:provider stuck)))
               limit-error)
    (check-equal? (length (mock-provider-calls stuck)) 1))

  (define never-retried
    (list (rpc-error "eth_getLogs" -32602 "invalid block range params")
          (rpc-error "eth_getLogs" -32602 "BlockOutOfRangeError: block height is 10 but requested was 16777216")
          (rpc-error "eth_getLogs" -32000 "unknown block")
          (rpc-error "eth_getLogs" -32005 "daily request count exceeded, request rate limited")
          (rpc-error "eth_getLogs" -32601 "Method not found")
          (rpc-error "eth_getLogs" -32603 "internal error")
          (exn:fail:web3 "eth_getLogs: connection refused" (current-continuation-marks))))

  (test-case "other errors are raised after one request"
    (for ([e (in-list never-retried)])
      (define node (logs-node #:fail? (lambda (from to) #t) #:error e))
      (check-eq? (raised (lambda () (get-logs transfers #:provider node))) e (exn-message e))
      (check-equal? (length (mock-provider-calls node)) 1 (exn-message e)))
    (define refused (rpc-error "eth_blockNumber" -32603 "internal error"))
    (define node (mock-provider (hash "eth_blockNumber" (lambda (_) (raise refused)))))
    (check-eq? (raised (lambda () (get-logs transfers #:chunk 10 #:provider node))) refused))

  (define limit-errors
    (list (rpc-error "eth_getLogs" -32005 "query returned more than 10000 results")
          (rpc-error "eth_getLogs" -32602
                     (string-append "Log response size exceeded. You can make eth_getLogs requests with up to a 2K "
                                    "block range and no limit on the response size, or you can request any block "
                                    "range with a cap of 10K logs in the response."))
          (rpc-error "eth_getLogs" -32614 "eth_getLogs is limited to a 10,000 range")
          (rpc-error "eth_getLogs" -32600 "block range is too wide")
          (rpc-error "eth_getLogs" -32602 "query exceeds max block range 100000")
          (rpc-error "eth_getLogs" -32602 "query exceeds max results 20000, retry with the range 1-500")
          (rpc-error "eth_getLogs" -32000 "ranges over 10000 blocks are not supported on freetier")
          (rpc-error "eth_getLogs" -32005 "limit exceeded")))

  (test-case "limit errors are recognised by code or wording, only from eth_getLogs"
    (for ([e (in-list limit-errors)])
      (check-true (log-limit-error? e) (exn-message e))
      (check-false (log-limit-error?
                    (rpc-error "eth_call" (exn:fail:web3:rpc-code e) (exn:fail:web3:rpc-rpc-message e)))
                   (exn-message e)))
    (for ([e (in-list never-retried)])
      (check-false (log-limit-error? e) (exn-message e))))

  (define (malformed-from thunk who)
    (define e (raised thunk))
    (check-pred exn:fail:web3? e)
    (when who
      (check-regexp-match (regexp (string-append "^" who ": malformed")) (exn-message e))))

  (test-case "malformed node data raises exn:fail:web3"
    (malformed-from (lambda () (get-logs transfers #:provider (mock-provider (hash "eth_getLogs" '(42)))))
                    "get-logs")
    (define five-topics (log-jsexpr #:topics (make-list 5 transfer-topic)))
    (malformed-from (lambda () (get-logs transfers #:provider (mock-provider (hash "eth_getLogs" (list five-topics)))))
                    "get-logs")
    (malformed-from (lambda () (get-logs transfers #:provider (mock-provider (hash "eth_getLogs" "x")))) #f)
    (malformed-from (lambda ()
                      (get-logs (log-filter #:from-block "safe") #:chunk 10
                                #:provider (mock-provider (hash "eth_getBlockByNumber" 'null))))
                    "get-logs"))

  (define approval-json
    (log-jsexpr #:topics (list (event-topic approval) (topic-word anvil-alice) (topic-word anvil-bob))
                #:data (word 5)
                #:index 1))

  (test-case "get-logs decodes with the filter's event by default"
    (define node (mock-provider (hash "eth_getLogs" (list (log-at 0) approval-json))))
    (define f (log-filter #:address token #:event transfer))
    (match (get-logs f #:provider node)
      [(list (? event-log? sent) (? log? approved))
       (check-equal? (event-log-arguments sent) (list anvil-alice anvil-bob 1))
       (check-false (event-log? approved))]
      [other (fail (format "~a" other))])
    (check-equal? (mock-provider-calls node) (list (list "eth_getLogs" (log-filter->jsexpr f))))
    (check-false (ormap event-log? (get-logs f #:events '() #:provider node))))

  (test-case "every keyword at its default is the same as none"
    (check-equal? (log-filter->jsexpr (log-filter #:address #f #:from-block "earliest" #:to-block "latest"
                                                  #:block-hash #f #:topics '() #:event #f #:where (hasheq)))
                  (log-filter->jsexpr (log-filter)))
    (define (traffic thunk)
      (define node (mock-provider (hash "eth_getLogs" (list (log-at 0)))))
      (list (thunk node) (mock-provider-calls node)))
    (define f (log-filter #:event transfer))
    (check-equal? (traffic (lambda (node) (get-logs f #:provider node)))
                  (traffic (lambda (node) (get-logs f #:events (list transfer) #:chunk #f #:provider node))))
    (check-equal? (traffic (lambda (node) (parameterize ([current-provider node]) (get-logs f))))
                  (traffic (lambda (node) (get-logs f #:provider node)))))

  (test-case "receipt-logs reads and decodes a receipt's logs"
    (define receipt (hasheq 'status "0x1" 'logs (list (log-at 0) approval-json)))
    (check-equal? (map log->jsexpr (receipt-logs receipt)) (list (log-at 0) approval-json))
    (check-equal? (map event-log? (receipt-logs receipt #:events (list transfer))) '(#t #f))
    (for ([logs (in-list '(missing null "x"))])
      (define bad (if (eq? logs 'missing) (hasheq 'status "0x1") (hasheq 'logs logs)))
      (malformed-from (lambda () (receipt-logs bad)) "receipt-logs"))
    (check-blames-here (receipt-logs 'null)))

  (test-case "get-logs blames its caller"
    (check-blames-here (get-logs (hasheq)))
    (check-blames-here (get-logs transfers #:chunk 0))
    (check-blames-here (get-logs transfers #:events "Transfer()"))
    (check-blames-here (get-logs transfers #:provider 5))))
