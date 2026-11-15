#lang racket/base

;; Decoding logs as events.

(module+ test
  (require racket/list
           racket/match
           rackunit
           web3
           (only-in web3/private/hex bytes->hex)
           "abi-gen.rkt"
           "helpers/event-fixtures.rkt"
           "util.rkt")

  (struct dispatch (log expected) #:prefab)

  (define (log-of e vs #:extra [extra #""])
    (define-values (topics data) (topics+data e vs))
    (jsexpr->log (log-jsexpr #:topics topics #:data (bytes-append data extra))))

  (define (generated-cases n)
    (define prng (seeded 1 1 2 3 5 8))
    (for/list ([_ (in-range n)])
      (define e (random-abi-event prng))
      (cons e (for/list ([p (in-list (abi-event-parameters e))])
                (random-abi-value prng (abi-event-parameter-type p))))))

  (define (log-fields l)
    (list (log-address l) (log-topics l) (log-data l) (log-block-number l) (log-block-hash l)
          (log-block-timestamp l) (log-transaction-hash l) (log-transaction-index l) (log-index l)
          (log-removed? l) (log->jsexpr l)))

  (define (check-in-range e el)
    (for ([p (in-list (abi-event-parameters e))]
          [v (in-list (event-log-arguments el))])
      (define t (abi-event-parameter-type p))
      (if (indexed-hash? v)
          (check-regexp-match #px"^0x[0-9a-f]{64}$" (indexed-hash-topic v))
          (begin
            (check-true (abi-value? t v))
            (check-equal? (checksummed (list t) (list v)) (list v))))))

  (test-case "decoding a log gives back the arguments it was made from, whatever data follows"
    (define prng (seeded 8 5 3 2 1 1))
    (for ([c (in-list (generated-cases 500))])
      (match-define (cons e vs) c)
      (define l (log-of e vs))
      (define el (decode-log e l))
      (check-equal? (event-log-arguments el) (decoded-arguments e vs) (abi-event->string e))
      (check-equal? (event-log-event el) e)
      (check-equal? (log-fields el) (log-fields l))
      (check-in-range e el)
      (check-equal? (event-log-arguments (decode-log e (log-of e vs #:extra (random-bytes prng (add1 (random 64 prng))))))
                    (decoded-arguments e vs))))

  (define alice-word (topic-word anvil-alice))
  (define bob-word (topic-word anvil-bob))
  (define transfer-log (log-of transfer (list anvil-alice anvil-bob 250)))

  (define (raw-log topics [data #""])
    (jsexpr->log (log-jsexpr #:topics topics #:data data)))

  (define (flip-byte topic)
    (string-append (substring topic 0 2) (if (char=? (string-ref topic 2) #\0) "1" "0") (substring topic 3)))

  (test-case "a log whose topics do not fit the event raises 'topic"
    (define cases
      (list (list transfer (raw-log (list (flip-byte transfer-topic) alice-word bob-word) (word 1)))
            (list transfer (raw-log (list transfer-topic alice-word bob-word bob-word) (word 1)))
            (list transfer (raw-log (list transfer-topic alice-word) (word 1)))
            (list transfer (raw-log '() (word 1)))
            (list note (raw-log (list alice-word bob-word) (word 1)))
            (list transfer (log-of transfer-721 (list anvil-alice anvil-bob 7)))))
    (for ([c (in-list cases)])
      (match-define (list e l) c)
      (define types (map abi-event-parameter-type
                         (filter abi-event-parameter-indexed? (abi-event-parameters (string->abi-event e)))))
      (check-decode-error (decode-log e l) 'decode-log 'topic 0 #:type types)
      (check-regexp-match #rx"^decode-log: topics do not match"
                          (exn-message (decode-error (lambda () (decode-log e l)))))))

  (test-case "decoding offsets count the topics, then the data"
    (define deposit-topics (list (event-topic deposit) alice-word))
    (check-decode-error (decode-log transfer (raw-log (list transfer-topic (topic-word (expt 2 160)) bob-word)
                                                      (word 1)))
                        'decode-log 'non-canonical 32)
    (check-decode-error (decode-log flags (raw-log (list (event-topic flags) (topic-word 2) (topic-word 0)
                                                         (topic-word 0))))
                        'decode-log 'non-canonical 32)
    (check-decode-error (decode-log deposit (raw-log deposit-topics (bytes-append (word 1) (word (expt 2 64)))))
                        'decode-log 'offset 96)
    (check-decode-error (decode-log deposit (raw-log deposit-topics (make-bytes 31 0)))
                        'decode-log 'short 64)
    (check-decode-error (decode-log deposit (raw-log deposit-topics
                                                     (bytes-append (word 1000) (word 64) (word 1)
                                                                   (bytes 255) (make-bytes 31 0))))
                        'decode-log 'utf-8 160))

  (test-case "arguments by name, position, and the log's own fields"
    (define el (decode-log transfer transfer-log))
    (check-equal? (event-log-ref el 'value) 250)
    (check-equal? (event-log-ref el "value") 250)
    (check-equal? (event-log-ref el 2) 250)
    (check-equal? (event-log-name el) "Transfer")
    (check-pred log? el)
    (check-equal? (log-fields el) (log-fields transfer-log))
    (define unnamed (decode-log bare (log-of bare '(5 "hi"))))
    (check-equal? (list (event-log-ref unnamed 0) (event-log-ref unnamed 1)) '(5 "hi")))

  (define tagged-log
    (jsexpr->log (let-values ([(topics data) (topics+data tagged (list "hello" #"\1\2" anvil-alice "body"))])
                   (log-jsexpr #:topics topics #:data data #:block 7))))

  (test-case "decoded logs print their arguments and place"
    (check-equal? (format "~a" (decode-log transfer transfer-log))
                  (string-append "#<event-log Transfer(from: " anvil-alice ", to: " anvil-bob
                                 ", value: 250) block 2 index 0>"))
    (check-equal? (format "~s" (decode-log tagged tagged-log))
                  (string-append "#<event-log Tagged(tag: hash 0x1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8, "
                                 "blob: hash 0x22ae6da6b482f9b1b19b0b897c3fd43884180a1c5ee361e1107a1bc635649dda, "
                                 "sender: " anvil-alice ", text: \"body\") block 7 index 0>"))
    (check-regexp-match #rx"^#<event-log Bare[(]5, \"hi\"[)] "
                        (format "~v" (decode-log bare (log-of bare '(5 "hi")))))
    (check-regexp-match #rx"plain: [(]1, \"abc\"[)][)]"
                        (format "~a" (decode-log paired (log-of paired '((1 "abc") (1 "abc"))))))
    (check-regexp-match #px"^#<event-log Lists[(]ids: hash 0x[0-9a-f]{64}, "
                        (format "~a" (decode-log lists (log-of lists '((1 2) ("a") (3 4))))))
    (define-values (topics data) (topics+data transfer (list anvil-alice anvil-bob 250)))
    (define pending
      (jsexpr->log (hash-set* (log-jsexpr #:topics topics #:data data #:removed #t)
                              'blockNumber 'null 'logIndex 'null)))
    (check-equal? (format "~v" (list (decode-log transfer pending)))
                  (string-append "(list #<event-log Transfer(from: " anvil-alice ", to: " anvil-bob
                                 ", value: 250) block pending index pending removed>)")))

  (define note-log (raw-log (list alice-word) (word 9)))
  (define unknown-note (raw-log (list (topic-word (expt 2 160))) (word 9)))
  (define approval-log (log-of approval (list anvil-alice anvil-bob 1)))
  (define bad-deposit (raw-log (list (event-topic deposit) alice-word) (bytes #x12)))
  (define no-topics (raw-log '() (word 1)))

  (define dispatch-table
    `(#s(dispatch ,transfer-log ,transfer)
      #s(dispatch ,(log-of transfer-721 (list anvil-alice anvil-bob 7)) ,transfer-721)
      #s(dispatch ,(log-of deposit (list anvil-alice 5 "memo")) ,deposit)
      #s(dispatch ,(log-of counted '(3)) ,counted)
      #s(dispatch ,note-log ,note)
      #s(dispatch ,unknown-note #f)
      #s(dispatch ,approval-log #f)
      #s(dispatch ,bad-deposit #f)
      #s(dispatch ,no-topics #f)))

  (test-case "decode-logs decodes each log as the event with its topic 0 and number of topics"
    (define candidates (list transfer transfer-721 deposit counted note))
    (define logs (map dispatch-log dispatch-table))
    (define results (decode-logs candidates logs))
    (check-equal? (length results) (length logs))
    (for ([row (in-list dispatch-table)]
          [result (in-list results)])
      (match-define (struct* dispatch ([log l] [expected e])) row)
      (cond
        [e (check-equal? result (decode-log e l) e)]
        [else (check-eq? result l)]))
    (check-equal? (decode-logs candidates results) results)
    (for ([l (in-list logs)]
          [result (in-list (decode-logs '() logs))])
      (check-eq? result l)))

  (test-case "events that cannot be told apart raise, even with no logs"
    (define same-topics '("E(uint256 indexed a, uint256 b)" "E(uint256 indexed b, uint256 a)"))
    (define (refused events)
      (raised-type-error (lambda () (decode-logs events '()))))
    (check-pred exn:fail:web3:abi:type? (refused same-topics))
    (check-regexp-match #rx"^decode-logs: ambiguous events: " (exn-message (refused same-topics)))
    (check-pred exn:fail:web3:abi:type?
                (refused '("A(uint256 indexed a) anonymous" "B(address indexed b, string c) anonymous")))
    (check-equal? (decode-logs (list transfer (string->abi-event transfer)) (list transfer-log))
                  (list (decode-log transfer transfer-log))))

  (test-case "decoding blames its caller"
    (define el (decode-log transfer transfer-log))
    (check-blames-here (event-topic 42))
    (check-blames-here (abi-value->topic 42 1))
    (check-blames-here (event-topics transfer (hasheq 'value 1)))
    (check-blames-here (event-topics transfer (hasheq 'nope 1)))
    (check-blames-here (event-topics transfer (hasheq 'to anvil-bob 1 anvil-bob)))
    (check-blames-here (event-topics transfer (hasheq 5 1)))
    (check-blames-here (any-of))
    (check-blames-here (indexed-hash "0x12"))
    (check-blames-here (decode-log transfer 'x))
    (check-blames-here (decode-logs transfer '()))
    (check-blames-here (event-log-ref el 'nope))
    (check-blames-here (event-log-ref el 3))
    (check-blames-here (event-log-arguments transfer-log))
    (check-exn (lambda (e) (and (exn:fail:web3:abi:type? e) (regexp-match? #rx"^event-topics: " (exn-message e))))
               (lambda () (event-topics "Transfer(" (hasheq)))))

  (test-case "a 'topic decode error is an ABI error and a web3 error"
    (define e (decode-error (lambda () (decode-log transfer no-topics))))
    (check-equal? (exn:fail:web3:abi:decode-reason e) 'topic)
    (check-pred exn:fail:web3:abi? e)
    (check-pred exn:fail:web3? e))

  (define transfer-json
    (hasheq 'address "0x5fbdb2315678afecb367f032d93f642f64180aa3"
            'topics (list transfer-topic
                          "0x000000000000000000000000f39fd6e51aad88f6f4ce6ab8827279cfffb92266"
                          "0x00000000000000000000000070997970c51812dc3a010c7d01b50e0d17dc79c8")
            'data "0x00000000000000000000000000000000000000000000000000000000000000fa"
            'blockNumber "0x2" 'logIndex "0x0" 'transactionIndex "0x0" 'removed #f
            'blockHash (filled-hash #\a)
            'transactionHash (filled-hash #\b)))

  (test-case "the manual's examples"
    (check-equal? (string->abi-event "event Transfer(address indexed from, address indexed to, uint256 value)")
                  (abi-event "Transfer" (list (abi-event-parameter "from" (abi-address) #t)
                                              (abi-event-parameter "to" (abi-address) #t)
                                              (abi-event-parameter "value" (abi-uint 256) #f))
                             #f))
    (check-equal? (abi-event->string '(event Deposit ([from address #:indexed] [value uint256] [memo string])))
                  "event Deposit(address indexed from, uint256 value, string memo)")
    (check-equal? (abi-signature->string (abi-event->signature deposit)) "Deposit(address,uint256,string)")
    (check-equal? (event-topic transfer) transfer-topic)
    (check-equal? (abi-value->topic 'string "hello")
                  "0x1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8")
    (check-equal? (event-topics transfer (hasheq 'to anvil-bob))
                  (list transfer-topic #f "0x00000000000000000000000070997970c51812dc3a010c7d01b50e0d17dc79c8"))
    (check-equal? (format "~v" (jsexpr->log transfer-json))
                  (string-append "#<log 0x5FbDB2315678afecb367f032d93F642f64180aa3 block 2 index 0 topics "
                                 "[0xddf252ad… 0x00000000… 0x00000000…] data 32 bytes>"))
    (define sent (decode-log transfer (jsexpr->log transfer-json)))
    (check-equal? (format "~v" sent)
                  (string-append "#<event-log Transfer(from: " anvil-alice ", to: " anvil-bob
                                 ", value: 250) block 2 index 0>"))
    (check-equal? (event-log-ref sent 'value) 250)
    (check-equal? (log-filter->jsexpr
                   (log-filter #:address "0x5FbDB2315678afecb367f032d93F642f64180aa3" #:from-block 10
                               #:event transfer #:where (hasheq 'from (any-of anvil-alice anvil-bob))))
                  (hasheq 'address "0x5FbDB2315678afecb367f032d93F642f64180aa3"
                          'fromBlock "0xa"
                          'toBlock "latest"
                          'topics (list transfer-topic (list alice-word bob-word))))
    (define node (mock-provider (hash "eth_getLogs" (list transfer-json))))
    (check-equal? (get-logs (log-filter #:event transfer) #:provider node) (list sent))
    (check-equal? (mock-provider-calls node)
                  (list (list "eth_getLogs" (hasheq 'fromBlock "earliest" 'toBlock "latest"
                                                    'topics (list transfer-topic)))))))
