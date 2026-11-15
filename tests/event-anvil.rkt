#lang racket/base

;; Logs the fixture contracts emit on anvil.  Skipped when anvil is not
;; available.

(module+ test
  (require racket/list
           racket/match
           rackunit
           web3
           "../examples/common.rkt"
           "helpers/event-fixtures.rkt"
           "util.rkt")

  (define (hex->number h)
    (string->number (substring h 2) 16))

  (define (arguments logs)
    (map event-log-arguments logs))

  (struct place (block index) #:prefab)
  (struct fields (address topics data block block-hash transaction transaction-index index removed?)
    #:prefab)

  (define (log-place l)
    (place (log-block-number l) (log-index l)))

  (define (place<=? a b)
    (match* ((log-place a) (log-place b))
      [((struct* place ([block block-a] [index index-a])) (struct* place ([block block-b] [index index-b])))
       (or (< block-a block-b) (and (= block-a block-b) (<= index-a index-b)))]))

  (define (log-fields l)
    (fields (log-address l) (log-topics l) (log-data l) (log-block-number l) (log-block-hash l)
            (log-transaction-hash l) (log-transaction-index l) (log-index l) (log-removed? l)))

  (define (sorted? logs)
    (or (null? logs) (andmap place<=? (drop-right logs 1) (rest logs))))

  (define (token-logs token #:where [where (hasheq)])
    (get-logs (log-filter #:address token #:event transfer #:where where)))

  (cond
    [(not (current-anvil-path))
     (displayln "skipping event anvil tests: anvil not found (set WEB3_ANVIL or use `nix develop`)")]
    [else
     (with-anvil ()
       (parameterize ([current-account anvil-alice])
         (define-values (token deployed)
           (deploy-contract/receipt (artifact-bytecode "Token") "constructor(string,string,uint256)"
                                    "Token" "TOK" 1000000))
         (define-values (events _events-deployed) (deploy-contract/receipt (artifact-bytecode "Events")))
         (define (send-events signature . vs)
           (apply contract-send events signature vs))

         (test-case "the constructor's mint is a Transfer from zero"
           (match (token-logs token #:where (hasheq 'from zero-address))
             [(list minted)
              (check-equal? (event-log-arguments minted) (list zero-address anvil-alice 1000000))
              (check-equal? (log-block-number minted) (hex->number (hash-ref deployed 'blockNumber)))
              (check-equal? (log-transaction-hash minted) (hash-ref deployed 'transactionHash))]
             [other (fail (format "~a" other))]))

         (define sent (contract-send token "transfer(address,uint256)" anvil-bob 250))

         (test-case "a receipt's logs decode"
           (match (receipt-logs sent #:events (list transfer))
             [(list (? event-log? l))
              (check-equal? (event-log-arguments l) (list anvil-alice anvil-bob 250))
              (check-true (andmap checksum-address? (take (event-log-arguments l) 2)))
              (check-equal? (log-address l) token)
              (check-equal? (log-index l) 0)
              (check-eq? (log->jsexpr l) (first (hash-ref sent 'logs)))]
             [other (fail (format "~a" other))]))

         (define later
           (list (contract-send token "transfer(address,uint256)" anvil-bob 1)
                 (contract-send token "transfer(address,uint256)" anvil-carol 2)
                 (contract-send token "transfer(address,uint256)" anvil-bob 3)))

         (test-case "filtering by an indexed address"
           (check-equal? (arguments (token-logs token #:where (hasheq 'to anvil-bob)))
                         (list (list anvil-alice anvil-bob 250) (list anvil-alice anvil-bob 1)
                               (list anvil-alice anvil-bob 3)))
           (check-equal? (length (token-logs token #:where (hasheq 'to (any-of anvil-bob anvil-carol))))
                         4)
           (check-equal? (arguments (token-logs token #:where (hasheq 'to anvil-alice)))
                         (list (list zero-address anvil-alice 1000000)))
           (check-equal? (token-logs token #:where (hasheq 'to zero-address)) '()))

         (define deposited (contract-send token "deposit(string)" "hi" #:value 1000))

         (test-case "a deposit's memo decodes"
           (check-equal? (arguments (receipt-logs deposited #:events (list deposit)))
                         (list (list anvil-alice 1000 "hi"))))

         (define (only-log receipt event)
           (match (receipt-logs receipt #:events (list event))
             [(list l) l]
             [other (fail (format "~a" other))]))

         (define (matches event where)
           (length (get-logs (log-filter #:address events #:event event #:where where))))

         (test-case "dynamic indexed values are hashed, and filter by their hash"
           (define tagged-receipt (send-events "tag(string,bytes,string)" "hello" #"\1\2" "body"))
           (check-equal? (event-log-arguments (only-log tagged-receipt tagged))
                         (list (indexed-hash (abi-value->topic 'string "hello"))
                               (indexed-hash (abi-value->topic 'bytes #"\1\2"))
                               anvil-alice
                               "body"))
           (check-equal? (matches tagged (hasheq 'tag "hello")) 1)
           (check-equal? (matches tagged (hasheq 'tag "nope")) 0)
           (check-equal? (matches tagged (hasheq 'blob #"\1\2")) 1)
           (define names '("a" "" "bc" "0123456789012345678901234567890123"))
           (send-events "lists(uint256[],string[],uint8[2])" '(1 2) names '(3 4))
           (check-equal? (matches lists (hasheq 'ids '(1 2))) 1)
           (check-equal? (matches lists (hasheq 'names names)) 1)
           (check-equal? (matches lists (hasheq 'pair '(3 4))) 1)
           (check-equal? (matches lists (hasheq 'names '("a" "bc" "0123456789012345678901234567890123"))) 1)
           (send-events "nested(uint256[][],(uint256,string)[])" '((1 2) () (3)) '((1 "ab") (2 "")))
           (check-equal? (matches nested (hasheq 'grid '((1 2) () (3)))) 1)
           (check-equal? (matches nested (hasheq 'pairs '((1 "ab") (2 "")))) 1)
           (define paired-receipt (send-events "paired(uint256,string)" 1 "abc"))
           (check-equal? (matches paired (hasheq 'pair '(1 "abc"))) 1)
           (check-equal? (event-log-ref (only-log paired-receipt paired) 'plain) '(1 "abc"))
           (define flags-receipt (send-events "flags(bool,int8,bytes3)" #t -1 #"abc"))
           (check-equal? (event-log-arguments (only-log flags-receipt flags)) (list #t -1 #"abc"))
           (check-equal? (matches flags (hasheq 'delta -1)) 1))

         (test-case "anonymous events decode by their number of topics"
           (define noted (send-events "note(uint256)" 9))
           (check-equal? (arguments (receipt-logs noted #:events (list note four)))
                         (list (list anvil-alice 9) (list 1 2 3 9)))
           (define found (get-logs (log-filter #:address events #:event note #:where (hasheq 'sender anvil-alice))))
           (check-not-false (member (event-log-arguments (decode-log note (first (receipt-logs noted))))
                                    (arguments (filter event-log? found)))))

         (define mixed
           (for/list ([to (in-list (list anvil-bob anvil-carol anvil-bob anvil-carol))]
                      [gap (in-list '(0 2 1 3))])
             (for ([_ (in-range gap)]) (anvil-mine))
             (contract-send token "transfer(address,uint256)" to 5)))

         (test-case "a token's logs come in chain order, in any chunks, within their range"
           (define all (get-logs (log-filter #:address token)))
           (check-true (sorted? all))
           (check-equal? (map log-fields all)
                         (map log-fields
                              (append-map receipt-logs (append (list deployed sent) later (list deposited) mixed))))
           (for ([chunk (in-list '(1 2 3 7))])
             (check-equal? (get-logs (log-filter #:address token) #:chunk chunk) all (format "chunk ~a" chunk)))
           (define start (hex->number (hash-ref (first mixed) 'blockNumber)))
           (check-equal? (get-logs (log-filter #:address token #:from-block start #:to-block (+ start 2)))
                         (filter (lambda (l) (<= start (log-block-number l) (+ start 2))) all)))

         (test-case "a pending bound is the latest block, chunked or not"
           (define f (log-filter #:address token #:to-block "pending"))
           (define whole (get-logs f))
           (check-equal? whole (get-logs (log-filter #:address token)))
           (for ([chunk (in-list '(1 10))])
             (check-equal? (get-logs f #:chunk chunk) whole (format "chunk ~a" chunk)))
           (check-equal? (get-logs (log-filter #:address token #:from-block "pending") #:chunk 10)
                         (get-logs (log-filter #:address token #:from-block "pending"))))

         (test-case "one transaction, many logs"
           (define counted-logs (receipt-logs (send-events "count(uint256)" 5) #:events (list counted)))
           (check-equal? (map log-index counted-logs) '(0 1 2 3 4))
           (check-equal? (arguments counted-logs) '((0) (1) (2) (3) (4))))

         (test-case "logs sharing topic 0 decode by their number of topics"
           (send-events "transferNft(address,uint256)" anvil-bob 7)
           (define both (get-logs (log-filter #:event transfer) #:events (list transfer transfer-721)))
           (define-values (from-events from-token) (partition (lambda (l) (equal? (log-address l) events)) both))
           (check-equal? (arguments from-events) (list (list anvil-alice anvil-bob 7)))
           (check-false (null? from-token))
           (for ([l (in-list from-token)])
             (check-equal? (event-log-event l) (string->abi-event transfer))
             (check-equal? (length (log-topics l)) 3)))

         (test-case "unnamed parameters are read by position"
           (define l (only-log (send-events "bare(uint256,string)" 5 "hi") bare))
           (check-equal? (event-log-arguments l) '(5 "hi"))
           (check-equal? (event-log-ref l 1) "hi"))

         (test-case "logs carry their block's timestamp and are not removed"
           (for ([l (in-list (get-logs (log-filter)))])
             (define block (eth-get-block-by-number (log-block-number l) #f))
             (check-equal? (log-block-timestamp l) (hex->number (hash-ref block 'timestamp)))
             (check-false (log-removed? l))))))]))
