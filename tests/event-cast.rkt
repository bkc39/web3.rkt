#lang racket/base

;; Topics and data against cast's event encoder.  Skipped when cast has no
;; abi-encode-event.

(module+ test
  (require (only-in json string->jsexpr)
           racket/match
           rackunit
           web3
           (only-in web3/private/hex bytes->hex)
           "abi-gen.rkt"
           "cast.rkt"
           "events.rkt"
           "util.rkt")

  (define (encodes-events?)
    (and cast-path
         (with-handlers ([exn:fail? (lambda (_) #f)])
           (run-cast "abi-encode-event" "--help")
           #t)))

  (define (cast-event e vs)
    (define arguments
      (for/list ([p (in-list (abi-event-parameters e))]
                 [v (in-list vs)])
        (value->cast-argument (abi-event-parameter-type p) v)))
    (hash-ref (string->jsexpr (apply run-cast "abi-encode-event" "--json" "--" (abi-event->string e) arguments))
              'data))

  (cond
    [(not (encodes-events?))
     (displayln "skipping event differential: cast has no abi-encode-event")]
    [else
     (test-case "topics and data are what cast encodes"
       (define prng (seeded 9 9 8 2 4 4))
       (for ([_ (in-range 200)])
         (define e (random-abi-event prng))
         (define vs (for/list ([p (in-list (abi-event-parameters e))])
                      (random-abi-value prng (abi-event-parameter-type p) #:cast-safe? #t)))
         (define name (format "~a ~s" (abi-event->string e) vs))
         (match-define (hash-table ['topics cast-topics] ['data cast-data]) (cast-event e vs))
         (define-values (topics data) (topics+data e vs))
         (check-equal? (map string-downcase cast-topics) topics name)
         (check-equal? (string-downcase cast-data) (bytes->hex data) name)
         (define log (jsexpr->log (log-jsexpr #:topics cast-topics #:data data)))
         (check-equal? (event-log-arguments (decode-log e log)) (decoded-arguments e vs) name)))

     (test-case "event topics are cast's signature hashes"
       (define prng (seeded 7 7 7 1 1 1))
       (define generated
         (for*/list ([_ (in-range 200)]
                     [e (in-value (random-abi-event prng))]
                     #:unless (abi-event-anonymous? e))
           (abi-event->string e)))
       (define known
         (for/list ([row (in-list known-topics)]
                    #:when (match row [(list _ topic) topic]))
           (match row [(list event _) event])))
       (for ([e (in-list (append known generated))])
         (check-equal? (string-downcase (run-cast "sig-event" "--" e)) (event-topic e) e)))]))
