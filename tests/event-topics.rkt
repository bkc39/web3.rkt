#lang racket/base

;; Event topics and the topics of indexed values.

(module+ test
  (require (only-in keccak keccak256)
           racket/list
           racket/match
           rackunit
           web3/abi
           web3/abi/event
           (only-in web3/private/hex bytes->hex)
           "abi-gen.rkt"
           "helpers/event-fixtures.rkt"
           "modules.rkt"
           "util.rkt")

  (struct hashed (type value topic) #:prefab)
  (struct positions (event where topics) #:prefab)

  (define (keccak-hex s)
    (bytes->hex (keccak256 (string->bytes/utf-8 s))))

  (define (generated-events n)
    (define prng (seeded 2 7 1 8 2 8))
    (for/list ([_ (in-range n)]) (random-abi-event prng)))

  (test-case "event topics are the hashes solc and cast give"
    (for ([row (in-list known-topics)])
      (match-define (struct* known-topic ([event event] [topic topic])) row)
      (check-equal? (event-topic event) topic event)
      (check-equal? (event-topic (abi-event->sexp event)) topic event)
      (check-equal? (event-topic (string->abi-event event)) topic event)))

  (define (renamed e)
    (match-define (struct* abi-event ([name name] [parameters ps])) e)
    (abi-event name (for/list ([p (in-list ps)]) (abi-event-parameter #f (abi-event-parameter-type p) #f)) #f))

  (test-case "an event's topic hashes its signature, whatever its names and indexing"
    (for ([e (in-list (generated-events 300))]
          #:unless (abi-event-anonymous? e))
      (define topic (keccak-hex (abi-signature->string (abi-event->signature e))))
      (check-equal? (event-topic e) topic)
      (check-equal? (event-topic (renamed e)) topic))
    (check-false (event-topic note)))

  (define hashed-values
    `(#s(hashed string "hello" "0x1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8")
      #s(hashed bytes #"\1\2" "0x22ae6da6b482f9b1b19b0b897c3fd43884180a1c5ee361e1107a1bc635649dda")
      #s(hashed "uint256[]" (1 2) "0xe90b7bceb6e7df5418fb78d8ee546e97c83a08bbccc01a0644d599ccd2a7c2e0")
      #s(hashed "string[]" ("a" "" "bc" "0123456789012345678901234567890123")
                "0xfb73e4046d60401a44edcef58e09df51643d8ee59c69213094c465e5810cbbd9")
      #s(hashed "uint8[2]" (3 4) "0x2e174c10e159ea99b867ce3205125c24a42d128804e4070ed6fcc8cc98166aa0")
      #s(hashed "uint256[][]" ((1 2) () (3))
                "0x6e0c627900b24bd432fe7b1f713f1b0744091a646a9fe4a65a18dfed21f2949c")
      #s(hashed "(uint256,string)[]" ((1 "ab") (2 ""))
                "0xa596d5522c76517b8d96ff05fc2f750389b403d367101af9f1c921718fb8d587")
      #s(hashed "(uint256,string)" (1 "abc")
                "0x18bf85d2b3d46a178ca374281428c7c938078fb486172beeed04151a529ca310")
      #s(hashed "uint256[1]" (7) "0xa66cc928b5edb82af9bd49922954155ab7b0942694bea4ce44661d9a8736c688")
      #s(hashed "(uint256)" (7) "0xa66cc928b5edb82af9bd49922954155ab7b0942694bea4ce44661d9a8736c688")
      #s(hashed bool #t ,(topic-word 1))
      #s(hashed int8 -1 ,(filled-hash #\f))
      #s(hashed bytes3 #"abc" ,(string-append "0x616263" (make-string 58 #\0)))))

  (test-case "indexed values have the topics solc emits"
    (for ([row (in-list hashed-values)])
      (match-define (struct* hashed ([type t] [value v] [topic topic])) row)
      (check-equal? (abi-value->topic t v) topic (format "~a" t)))
    (check-equal? (abi-value->topic "string[]" '("a" "")) (abi-value->topic "string[]" '("a"))))

  (define (pad-word bs)
    (bytes-append bs (make-bytes (modulo (- (bytes-length bs)) 32) 0)))

  ;; Solidity's in-place encoding: values without offsets or lengths, each
  ;; element of an array or struct padded to whole words.
  (define (reference-in-place t v)
    (define (element t x)
      (if (hashed-type? t) (pad-word (reference-in-place t x)) (abi-encode (list t) (list x))))
    (match t
      [(struct* abi-string ()) (string->bytes/utf-8 v)]
      [(struct* abi-bytes ()) v]
      [(or (struct* abi-array ([element e])) (struct* abi-fixed-array ([element e])))
       (apply bytes-append #"" (map (lambda (x) (element e x)) v))]
      [(struct* abi-tuple ([components cs])) (apply bytes-append #"" (map element cs v))]))

  (define (reference-topic t v)
    (bytes->hex (if (hashed-type? t) (keccak256 (reference-in-place t v)) (abi-encode (list t) (list v)))))

  (test-case "the topic of a value is its word, or the hash of its in-place encoding"
    (define prng (seeded 3 1 4 1 5 9))
    (for ([_ (in-range 300)])
      (define t (random-abi-type prng))
      (define v (random-abi-value prng t))
      (check-equal? (abi-value->topic t v) (reference-topic t v) (abi-type->string t)))
    (for* ([n (in-list widths)]
           [row (in-list (list (list (abi-uint n) 0) (list (abi-uint n) (sub1 (expt 2 n)))
                               (list (abi-int n) (- (expt 2 (sub1 n))))
                               (list (abi-int n) (sub1 (expt 2 (sub1 n))))))])
      (match-define (list t v) row)
      (check-equal? (abi-value->topic t v) (bytes->hex (abi-encode (list t) (list v))))))

  (define bob-mixed "0x70997970c51812dc3A010C7d01b50e0d17dc79C8")
  (define tagged-topic (event-topic tagged))
  (define lists-topic (event-topic lists))
  (define hello-topic "0x1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8")

  (define topic-positions
    `(#s(positions ,transfer #hasheq() (,transfer-topic))
      #s(positions ,transfer #hasheq((to . ,anvil-bob)) (,transfer-topic #f ,(topic-word anvil-bob)))
      #s(positions ,transfer #hash((1 . ,anvil-bob)) (,transfer-topic #f ,(topic-word anvil-bob)))
      #s(positions ,transfer #hasheq((from . ,(any-of anvil-alice anvil-bob anvil-alice)))
                   (,transfer-topic (,(topic-word anvil-alice) ,(topic-word anvil-bob))))
      #s(positions ,transfer #hasheq((from . ,(any-of anvil-bob))) (,transfer-topic ,(topic-word anvil-bob)))
      #s(positions ,transfer #hasheq((to . ,bob-mixed)) (,transfer-topic #f ,(topic-word anvil-bob)))
      #s(positions ,note #hasheq((sender . ,anvil-alice)) (,(topic-word anvil-alice)))
      #s(positions ,note #hasheq() ())
      #s(positions ,tagged #hasheq((tag . "hello")) (,tagged-topic ,hello-topic))
      #s(positions ,tagged #hasheq((tag . ,(indexed-hash (string-append "0x" (string-upcase (substring hello-topic 2))))))
                   (,tagged-topic ,hello-topic))
      #s(positions ,lists #hasheq((names . ,(any-of '("a") '("a" ""))))
                   (,lists-topic #f ,(abi-value->topic "string[]" '("a"))))))

  (test-case "event-topics gives a topic per indexed argument, #f for any, and no trailing #f"
    (for ([row (in-list topic-positions)])
      (match-define (struct* positions ([event e] [where where] [topics topics])) row)
      (define result (event-topics e where))
      (check-equal? result topics (format "~a ~s" e where))
      (check-false (and (pair? result) (not (last result))))))

  (define (value-error thunk)
    (with-handlers ([exn:fail:web3:abi:value? values])
      (thunk)
      #f))

  (test-case "bad indexed values raise at the parameter's index"
    (define bad-address (value-error (lambda () (event-topics transfer (hasheq 'to "0x12")))))
    (check-equal? (exn:fail:web3:abi:value-path bad-address) '(1))
    (check-equal? (exn:fail:web3:abi:value-type bad-address) (abi-address))
    (check-regexp-match #rx"^event-topics: " (exn-message bad-address))
    (define bad-alternative (value-error (lambda () (event-topics transfer (hasheq 'to (any-of anvil-bob 7))))))
    (check-equal? (exn:fail:web3:abi:value-path bad-alternative) '(1))
    (define out-of-range (value-error (lambda () (abi-value->topic 'uint8 256))))
    (check-equal? (exn:fail:web3:abi:value-path out-of-range) '(0))
    (check-regexp-match #rx"^abi-value->topic: " (exn-message out-of-range)))

  (test-case "web3/abi/event and web3/eth/log need no JSON or transport modules"
    (define allowed
      '("web3/abi" "web3/abi/event" "web3/abi/event-type" "web3/abi/type" "web3/address"
        "web3/eth/log" "web3/private/abi-decode" "web3/private/exn" "web3/private/hex"
        "web3/private/node" "web3/private/who"))
    (define keccak-names (map module-name (import-closure 'keccak)))
    (for ([root (in-list '(web3/abi/event web3/eth/log))])
      (define names (filter values (map module-name (import-closure root))))
      (check-equal? (filter (lambda (n) (and (member (collection n) '("net" "json" "openssl" "ffi"))
                                             (not (member n keccak-names))))
                            names)
                    '()
                    (format "~a" root))
      (check-equal? (filter (lambda (n) (and (equal? (collection n) "web3") (not (member n allowed))))
                            names)
                    '()
                    (format "~a" root)))))
