#lang racket/base

(module+ test
  (require (only-in racket/contract flat-contract-predicate)
           racket/list
           (only-in racket/sequence in-slice)
           racket/match
           rackunit
           web3
           (only-in web3/private/abi-decode abi-decode-reason/c)
           (only-in web3/private/hex bytes->hex)
           "abi-gen.rkt"
           "bounded.rkt"
           "helpers/event-fixtures.rkt"
           "util.rkt")

  (define seed '(5 4 3 2 1 0))
  (define reason? (flat-contract-predicate abi-decode-reason/c))

  (define (pick prng xs)
    (list-ref xs (random (length xs) prng)))

  (define (random-topics prng)
    (for/list ([_ (in-range (random 5 prng))])
      (if (zero? (random 2 prng)) (bytes->hex (random-word prng 64)) (random-hash prng))))

  (define (flip-bit prng topic)
    (define i (+ 2 (random 64 prng)))
    (define digit (string->number (substring topic i (add1 i)) 16))
    (string-append (substring topic 0 i)
                   (number->string (bitwise-xor digit (arithmetic-shift 1 (random 4 prng))) 16)
                   (substring topic (add1 i))))

  (define (mutate-log prng topics data)
    (match (random 5 prng)
      [0 (values (if (null? topics) topics (list-update topics (random (length topics) prng) (lambda (t) (flip-bit prng t))))
                 data)]
      [1 (values (if (null? topics) topics (remove (pick prng topics) topics)) data)]
      [2 (values (if (or (null? topics) (= (length topics) 4)) topics (cons (first topics) topics)) data)]
      [3 (values topics (subbytes data 0 (random (add1 (bytes-length data)) prng)))]
      [4 (values topics
                 (if (< (bytes-length data) 32)
                     data
                     (let ([out (bytes-copy data)])
                       (bytes-copy! out (* 32 (random (quotient (bytes-length data) 32) prng)) (random-word prng 64))
                       out)))]))

  ;; (list event log): random logs for random events, then mutations of
  ;; logs the events emit.
  (define (decode-inputs prng)
    (append
     (for/list ([_ (in-range 400)])
       (list (random-abi-event prng)
             (jsexpr->log (log-jsexpr #:topics (random-topics prng) #:data (random-data prng)))))
     (for/list ([_ (in-range 1600)])
       (define e (random-abi-event prng))
       (define vs (for/list ([p (in-list (abi-event-parameters e))])
                    (random-abi-value prng (abi-event-parameter-type p))))
       (define-values (topics data) (topics+data e vs))
       (define-values (mutated-topics mutated-data) (mutate-log prng topics data))
       (list e (jsexpr->log (log-jsexpr #:topics mutated-topics #:data mutated-data))))))

  (define (name-of e l)
    (format "seed ~s, ~a ~s ~a" seed (abi-event->string e) (log-topics l) (bytes->hex (log-data l))))

  (define (check-arguments e el name)
    (for ([p (in-list (abi-event-parameters e))]
          [v (in-list (event-log-arguments el))])
      (define t (abi-event-parameter-type p))
      (cond
        [(indexed-hash? v) (check-regexp-match #px"^0x[0-9a-f]{64}$" (indexed-hash-topic v) name)]
        [else
         (check-true (abi-value? t v) name)
         (check-equal? (checksummed (list t) (list v)) (list v) name)])))

  (test-case "decode-log of any log finishes in bounds with an event-log or a decode error"
    (define prng (apply seeded seed))
    (for ([input (in-list (decode-inputs prng))])
      (match-define (list e l) input)
      (define name (name-of e l))
      (match (call-bounded (lambda () (decode-log e l)))
        [(list 'ok (? event-log? el)) (check-arguments e el name)]
        [(list 'raised (? exn:fail:web3:abi:decode? x))
         (check-pred reason? (exn:fail:web3:abi:decode-reason x) name)
         (check-true (<= (exn:fail:web3:abi:decode-offset x)
                         (+ (* 32 (length (log-topics l))) (bytes-length (log-data l))))
                     name)]
        [other (fail (format "~a: ~e" name other))])))

  (define (ambiguous? events)
    (with-handlers ([exn:fail:web3:abi:type? (lambda (_) #t)])
      (decode-logs events '())
      #f))

  (test-case "decode-logs of any logs finishes in bounds, each log decoded or returned as it was"
    (define prng (apply seeded seed))
    (define inputs (decode-inputs prng))
    (for ([group (in-slice 4 inputs)])
      (define events
        (for/fold ([events '()])
                  ([input (in-list group)]
                   #:unless (ambiguous? (cons (first input) events)))
          (cons (first input) events)))
      (define logs (map second group))
      (match (call-bounded (lambda () (decode-logs events logs)))
        [(list 'ok results)
         (check-equal? (length results) (length logs))
         (for ([l (in-list logs)]
               [r (in-list results)])
           (check-true (or (eq? r l) (event-log? r)) (name-of (first (first group)) l)))]
        [other (fail (format "seed ~s: ~e" seed other))])))

  (define log-keys '(address topics data blockHash blockNumber blockTimestamp transactionHash
                     transactionIndex logIndex removed))

  (define (near-miss prng)
    (pick prng (list 'null #t #f 0 -1 1.5 "" "0x" "0x0" "0xzz" "0x1" (random-hash prng)
                     (substring (random-hash prng) 0 65) "0x5fbdb2315678afecb367f032d93f642f64180aa3"
                     (list (random-hash prng)) (make-list 5 (random-hash prng)) (list 1) (hasheq) '())))

  (define (random-object prng)
    (for/hasheq ([key (in-list log-keys)]
                 #:when (positive? (random 4 prng)))
      (values key (if (zero? (random 3 prng)) (near-miss prng) (good-field prng key)))))

  (define (random-jsexpr prng depth)
    (match (random (if (zero? depth) 2 4) prng)
      [0 (near-miss prng)]
      [1 (random-object prng)]
      [2 (for/list ([_ (in-range (random 3 prng))]) (random-jsexpr prng (sub1 depth)))]
      [3 (hash-set (random-object prng) (pick prng log-keys) (random-jsexpr prng (sub1 depth)))]))

  (define (good-field prng key)
    (hash-ref (log-jsexpr #:topics (list (random-hash prng)) #:data (random-bytes prng (random 40 prng))) key))

  (test-case "jsexpr->log of any JSON finishes in bounds with a log or exn:fail:web3"
    (define prng (apply seeded seed))
    (for ([_ (in-range 500)])
      (define v (random-jsexpr prng 3))
      (match (call-bounded (lambda () (jsexpr->log v)))
        [(list 'ok (? log?)) (void)]
        [(list 'raised (? exn:fail:web3?)) (void)]
        [other (fail (format "seed ~s, ~e: ~e" seed v other))]))))
