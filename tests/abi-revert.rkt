#lang racket/base

(module+ test
  (require racket/match
           rackunit
           web3/abi
           (submod web3/abi internal)
           (only-in web3/rpc bytes->hex-string exn:fail:web3:rpc exn:fail:web3:rpc?
                    exn:fail:web3:rpc-code exn:fail:web3:rpc-data exn:fail:web3:rpc-method
                    exn:fail:web3:rpc-rpc-message raise-rpc-error)
           "abi-gen.rkt"
           "revert-cases.rkt"
           "util.rkt")

  (define (w n) (word->bytes n))
  (define A "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed")
  (define A* "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed")
  (define balance "InsufficientBalance(uint256,uint256)")
  (define max-word (sub1 (expt 2 256)))

  (test-case "decode-revert recognises built-in and given errors"
    (for ([row (in-list reverts)])
      (match-define (list payload errors name arguments string) row)
      (define r (decode-revert payload #:errors errors))
      (check-equal? (abi-revert-name r) name string)
      (check-equal? (abi-revert-arguments r) arguments string)
      (check-equal? (abi-revert->string r) string)
      (check-equal? (abi-revert-data r) payload string)
      (check-equal? (abi-revert-selector r)
                    (and (>= (bytes-length payload) 4) (subbytes payload 0 4))
                    string)
      (check-equal? (not (abi-revert-signature r)) (not name) string))
    (check-decode-error (decode-revert (bytes-append (bytes #x08 #xc3 #x79 #xa0) (w (expt 2 64))))
                        'decode-revert 'offset 4 #:type "string")
    (check-decode-error (decode-revert (bytes-append (bytes #x08 #xc3 #x79 #xa0) (w 32) (w 1) #"\377"))
                        'decode-revert 'utf-8 68 #:type "string")
    (check-decode-error (decode-revert (bytes-append (bytes #x4e #x48 #x7b #x71) (make-bytes 31)))
                        'decode-revert 'short 4 #:type '("uint256")))

  (test-case "error selectors"
    (for ([row (in-list `((,balance "0xcf479181") ("Error(string)" "0x08c379a0")
                          ("Panic(uint256)" "0x4e487b71")))])
      (match-define (list sig hex) row)
      (check-equal? (bytes->hex-string (error-selector sig)) hex)
      (check-equal? (error-selector (string->abi-signature sig)) (function-selector sig)))
    (check-exn (lambda (e)
                 (and (exn:fail:web3:abi:type? e)
                      (regexp-match? #rx"^error-selector: an error signature has no outputs"
                                     (exn-message e))))
               (lambda () (error-selector "Foo(uint256)(uint256)"))))

  (test-case "decode-revert inverts encode-call for generated errors"
    (define prng (vector->pseudo-random-generator (vector 9 8 7 6 5 4)))
    (for ([i (in-range 100)])
      (define ts (random-abi-types prng))
      (define vs (for/list ([t (in-list ts)]) (random-abi-value prng t)))
      (define sig (signature-string (format "E~a" i) ts))
      (define payload (apply encode-call sig vs))
      (define r (decode-revert payload #:errors (list sig)))
      (check-equal? (abi-revert-signature r) (string->abi-signature sig) sig)
      (check-equal? (abi-revert-arguments r) (checksummed ts vs) sig)
      (check-equal? (abi-revert-data r) payload sig)))

  (define (type-error-matching rx)
    (lambda (e) (and (exn:fail:web3:abi:type? e) (regexp-match? rx (exn-message e)))))

  (test-case "the error list is checked whatever the data"
    (define collision '("burn(uint256)" "collate_propagate_storage(bytes16)"))
    (for ([data (in-list (list #"" (encode-call "burn(uint256)" 1)))])
      (check-exn (type-error-matching
                  #rx"^decode-revert: selector collision: burn[(]uint256[)] and collate_propagate_storage[(]bytes16[)]")
                 (lambda () (decode-revert data #:errors collision))))
    (check-equal? (abi-revert-name (decode-revert (encode-call "Foo(uint256)" 1)
                                                  #:errors '("Foo(uint)" "Foo(uint256)")))
                  "Foo")
    (define builtin (decode-revert (encode-call "Error(string)" "x") #:errors '("Error(string)")))
    (check-equal? (abi-revert->string builtin) "Error(\"x\")")
    (check-exn (type-error-matching #rx"^decode-revert: ") (lambda () (decode-revert #"" #:errors '("Foo("))))
    (check-exn (type-error-matching #rx"^decode-revert: an error signature has no outputs")
               (lambda () (decode-revert #"" #:errors '("Foo()(uint256)")))))

  (test-case "decode-revert-reason keeps its old outputs and never raises"
    (check-equal? (decode-revert-reason
                   (bytes-append (bytes #x08 #xc3 #x79 #xa0) (w 32) (w 5) #"oops!" (make-bytes 27 0)))
                  "Error(\"oops!\")")
    (check-equal? (decode-revert-reason (encode-call "Panic(uint256)" #x11)) "Panic(0x11)")
    (check-false (decode-revert-reason #""))
    (check-false (decode-revert-reason
                  (bytes-append (bytes #x08 #xc3 #x79 #xa0) (w 32) (w 1) #"\377" (make-bytes 31))))
    (check-equal? (decode-revert-reason (bytes-append (encode-call "Panic(uint256)" #x11) (make-bytes 5 1)))
                  "Panic(0x11)")
    (check-false (decode-revert-reason (encode-call balance 10 20)))
    (define prng (vector->pseudo-random-generator (vector 3 3 3 3 3 3)))
    (define prefixes (list #"" (bytes #x08 #xc3 #x79 #xa0) (bytes #x4e #x48 #x7b #x71)))
    (for ([_ (in-range 500)])
      (define data (bytes-append (list-ref prefixes (random 3 prng))
                                 (random-bytes prng (random 200 prng))))
      (define reason (decode-revert-reason data))
      (check-true (or (string? reason) (not reason)) (bytes->hex-string data))))

  (test-case "panic descriptions"
    (for ([row (in-list panics)])
      (match-define (list code description) row)
      (check-equal? (panic-description code) description))
    (for ([code (in-list (list 2 #x99 max-word))])
      (check-false (panic-description code))))

  (define P (bytes->hex-string (encode-call "Error(string)" "oops!")))

  (define (rpc-exn message data)
    (exn:fail:web3:rpc "m" (current-continuation-marks) "eth_call" 3 message data))

  ;; data, rpc-message, expected: a string, 'empty or #f
  (define shapes
    `((,P "execution reverted: revert: oops!" "Error(\"oops!\")")
      ("0x" "execution reverted" empty)
      (#f "execution reverted" empty)
      (null "execution reverted" empty)
      (#f "nonce too low" #f)
      (,(hasheq 'data P) "any" "Error(\"oops!\")")
      (,(hasheq 'message "x" 'data P) "any" "Error(\"oops!\")")
      (,(hasheq 'data (hasheq 'data P)) "execution reverted" #f)
      (,(string-append "Reverted " P) "any" "Error(\"oops!\")")
      ("0x123" "execution reverted" #f)
      ("0xzz" "execution reverted" #f)
      (42 "execution reverted" #f)
      ((1) "execution reverted" #f)))

  (test-case "rpc-exn->revert reads the payload nodes send"
    (for ([row (in-list shapes)])
      (match-define (list data message expected) row)
      (define r (rpc-exn->revert (rpc-exn message data)))
      (define name (format "~s ~s" data message))
      (match expected
        [#f (check-false r name)]
        ['empty (check-equal? r (abi-revert #f '() #"") name)]
        [_ (check-equal? (abi-revert->string r) expected name)]))
    (define malformed (bytes-append (bytes #x08 #xc3 #x79 #xa0) (w (expt 2 64))))
    (check-equal? (rpc-exn->revert (rpc-exn "any" (bytes->hex-string malformed)))
                  (abi-revert #f '() malformed))
    (check-exn exn:fail:web3:abi:type?
               (lambda () (rpc-exn->revert (rpc-exn "execution reverted" P) #:errors '("Foo(")))))

  (test-case "rpc-exn->revert-exn keeps the RPC fields and names the revert"
    (define data (bytes->hex-string (encode-call balance 10 20)))
    (define e (rpc-exn "execution reverted" data))
    (define r (rpc-exn->revert-exn e #:errors (list balance)))
    (check-pred exn:fail:web3:rpc:revert? r)
    (check-pred exn:fail:web3:rpc? r)
    (check-equal? (exn-message r) "eth_call: reverted with InsufficientBalance(10, 20)")
    (check-equal? (exn:fail:web3:rpc-method r) "eth_call")
    (check-equal? (exn:fail:web3:rpc-code r) 3)
    (check-equal? (exn:fail:web3:rpc-rpc-message r) "execution reverted")
    (check-equal? (exn:fail:web3:rpc-data r) data)
    (check-eq? (exn-continuation-marks r) (exn-continuation-marks e))
    (check-equal? (exn:fail:web3:rpc:revert-revert r) (rpc-exn->revert e #:errors (list balance)))
    (for ([row (in-list shapes)])
      (match-define (list data message expected) row)
      (unless expected
        (check-false (rpc-exn->revert-exn (rpc-exn message data)) (format "~s" row)))))

  (test-case "the internal entry points raise under their caller's name"
    (check-equal? (string->abi-signature/who 'contract-call "f(uint)") (string->abi-signature "f(uint256)"))
    (check-exn (type-error-matching #rx"^contract-call: ")
               (lambda () (string->abi-signature/who 'contract-call "f(")))
    (check-equal? (abi-encode/who 'deploy-contract '(uint8) '(7)) (w 7))
    (check-exn (lambda (e) (and (exn:fail:web3:abi:value? e)
                                (regexp-match? #rx"^deploy-contract: " (exn-message e))))
               (lambda () (abi-encode/who 'deploy-contract '(uint8) '(256))))
    (check-equal? (encode-call/who 'contract-send "f(uint256)" '(7)) (encode-call "f(uint256)" 7))
    (check-exn (lambda (e) (and (exn:fail:web3:abi:value? e)
                                (regexp-match? #rx"^contract-send: " (exn-message e))))
               (lambda () (encode-call/who 'contract-send "f(uint256)" '(-1))))
    (check-not-exn (lambda () (check-error-signatures/who 'contract-call (list balance "Foo(uint)"))))
    (check-exn (type-error-matching #rx"^contract-call: selector collision")
               (lambda () (check-error-signatures/who
                           'contract-call '("burn(uint256)" "collate_propagate_storage(bytes16)"))))
    (check-exn (type-error-matching #rx"^contract-call: an error signature has no outputs")
               (lambda () (check-error-signatures/who 'contract-call '("Foo()(bool)")))))

  (test-case "arguments display as in abi-revert->string"
    (for ([row (in-list `(("uint256" 10 "10")
                          ("int8" -5 "-5")
                          ("bool" #t "true")
                          ("bool" #f "false")
                          ("address" ,A ,A*)
                          ("bytes" #"\1\2" "0x0102")
                          ("bytes2" #"\0\377" "0x00ff")
                          ("string" "a\"b" "\"a\\\"b\"")
                          ("uint256[]" (1 2) "[1, 2]")
                          ("uint256[]" () "[]")
                          ("(string,(bool,address))[1]" (("x" (#f ,A))) ,(format "[(\"x\", (false, ~a))]" A*))))])
      (match-define (list type v expected) row)
      (check-equal? (abi-value->display-string type v) expected)))

  (test-case "doc examples"
    (check-equal? (bytes->hex-string (error-selector balance)) "0xcf479181")
    (define data (encode-call balance 10 20))
    (check-equal? (abi-revert-arguments (decode-revert data #:errors (list balance))) '(10 20))
    (check-equal? (abi-revert->string (decode-revert data)) "unknown error 0xcf479181 with 64 bytes of data")
    (check-equal? (abi-revert->string (decode-revert (encode-call "Panic(uint256)" #x11)))
                  "Panic(0x11): arithmetic underflow or overflow")
    (check-equal? (panic-description #x32) "array out-of-bounds access")
    (check-equal? (with-handlers ([exn:fail:web3:rpc? (lambda (e) (abi-revert->string (rpc-exn->revert e)))])
                    (raise-rpc-error "eth_call" 3 "execution reverted" P))
                  "Error(\"oops!\")")))
