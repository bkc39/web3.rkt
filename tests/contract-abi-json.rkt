#lang racket/base

;; Descriptions read from JSON: the committed solc artifacts round trip,
;; agree with solc's selectors, and every artifact shape gives its bytecode.

(module+ test
  (require json
           racket/file
           racket/list
           racket/match
           racket/port
           rackunit
           web3
           "../examples/common.rkt"
           "abi-gen.rkt"
           "helpers/event-fixtures.rkt"
           "helpers/temporary.rkt"
           "util.rkt")

  (define fixtures '("Token" "Counter" "Events"))

  (define (norm j [component? #f])
    (match j
      [(? list?) (map (lambda (x) (norm x component?)) j)]
      [(? hash?)
       (for/hasheq ([(k v) (in-hash j)]
                    #:unless (eq? k 'internalType))
         (values k
                 (cond
                   [(eq? k 'components) (norm v #t)]
                   [(and component? (eq? k 'name)) ""]
                   [else (norm v #f)])))]
      [_ j]))

  (test-case "each fixture's ABI reads back as solc wrote it, with its bytecode"
    (for ([name (in-list fixtures)])
      (define j (artifact name))
      (define d (jsexpr->contract-abi j))
      (check-equal? (contract-abi->jsexpr d) (norm (hash-ref j 'abi)) name)
      (check-equal? (contract-abi-bytecode d) (hex-string->bytes (artifact-bytecode name)) name)
      (check-equal? (file->contract-abi (artifact-path name)) d name)
      (check-equal? (read-contract-abi (open-input-bytes (file->bytes (artifact-path name)))) d name)))

  (test-case "selectors and topics of the fixtures are solc's"
    (for ([name (in-list fixtures)])
      (define j (artifact name))
      (define d (jsexpr->contract-abi j))
      (define identifiers (hash-ref j 'methodIdentifiers))
      (for ([f (in-list (contract-abi-functions d))])
        (define key (string->symbol (abi-signature->string f)))
        (check-equal? (bytes->hex-string (function-selector f))
                      (string-append "0x" (hash-ref identifiers key))
                      (symbol->string key)))
      (for ([e (in-list (contract-abi-events d))])
        (define row (findf (lambda (row) (equal? (string->abi-event (known-topic-event row)) e))
                           known-topics))
        (check-not-false row (abi-event->string e))
        (when row
          (check-equal? (event-topic e) (known-topic-topic row) (abi-event->string e)))))
    (define token (jsexpr->contract-abi (artifact "Token")))
    (check-equal? (map (lambda (e) (bytes->hex-string (error-selector e))) (contract-abi-errors token))
                  '("0xcf479181" "0x8e4a23d6")))

  (test-case "an artifact listing one error and one event twice keeps each once"
    (define j (artifact "Mover"))
    (define d (jsexpr->contract-abi j))
    (check-equal? (length (hash-ref j 'abi)) 7)
    (check-equal? (contract-abi->jsexpr d) (remove-duplicates (norm (hash-ref j 'abi))))
    (check-equal? (contract-abi-error d 'Unauthorized) (string->abi-item "error Unauthorized(address who)"))
    (check-equal? (contract-abi-event d 'Moved) (string->abi-event "Moved(uint256 x)"))
    (check-equal? (file->contract-abi (artifact-path "Mover")) d))

  (test-case "every fixture item reads back from its string and its JSON"
    (for* ([name (in-list fixtures)]
           [i (in-list (contract-abi-items (jsexpr->contract-abi (artifact name))))])
      (check-equal? (string->abi-item (abi-item->string i)) i (abi-item->string i))
      (check-equal? (jsexpr->abi-item (abi-item->jsexpr i)) i (abi-item->string i))))

  (test-case "generated descriptions survive JSON, noisy JSON and JSON text"
    (define prng (seeded 6 3 6 3 1 1))
    (for ([_ (in-range 300)])
      (define d (random-contract-abi prng))
      (define j (contract-abi->jsexpr d))
      (define name (format "~s" j))
      (check-equal? (jsexpr->contract-abi j) (contract-abi (contract-abi-items d)) name)
      (check-equal? (contract-abi->jsexpr (jsexpr->contract-abi j)) j name)
      (check-equal? (jsexpr->contract-abi (noisy-jsexpr prng j)) (jsexpr->contract-abi j) name)
      (check-equal? (read-contract-abi (open-input-string (jsexpr->string j))) (jsexpr->contract-abi j)
                    name)))

  (test-case "artifacts from forge, Hardhat, solc and --combined-json give their bytecode"
    (define abi (list (hasheq 'type "function" 'name "f" 'inputs '() 'outputs '()
                              'stateMutability "nonpayable")))
    (define (bytecode j)
      (contract-abi-bytecode (jsexpr->contract-abi j)))
    (check-false (bytecode abi))
    (check-false (bytecode (hasheq 'abi abi)))
    (check-equal? (bytecode (hasheq 'abi abi 'bytecode (hasheq 'object "0x6080"))) #"\x60\x80")
    (check-false (bytecode (hasheq 'abi abi 'bytecode (hasheq 'object "0x"))))
    (check-equal? (bytecode (hasheq 'abi abi 'bytecode "0x6080")) #"\x60\x80")
    (check-equal? (bytecode (hasheq 'abi abi 'evm (hasheq 'bytecode (hasheq 'object "6080")))) #"\x60\x80")
    (check-equal? (bytecode (hasheq 'abi abi 'bin "6080")) #"\x60\x80")
    (check-equal? (length (contract-abi-items (jsexpr->contract-abi (hasheq 'abi abi)))) 1)
    (check-false (bytecode (hasheq 'abi abi 'bytecode 'null)))
    (check-equal? (bytecode (hasheq 'abi abi 'bytecode 'null 'bin "6080")) #"\x60\x80")
    (for ([j (list (hasheq 'abi abi 'bytecode "0x6080__$abcdef$__6080")
                   (hasheq 'abi (jsexpr->string abi))
                   (hasheq 'bytecode "0x6080")
                   (hasheq 'abi 5)
                   (hasheq 'abi abi 'bytecode "0x6")
                   (hasheq 'abi abi 'bytecode 5)
                   (hasheq 'abi abi 'bytecode (hasheq 'object 5))
                   (hasheq 'abi abi 'evm (hasheq 'bytecode (hasheq 'object #t)))
                   (hasheq 'abi abi 'evm 5)
                   (hasheq 'abi abi 'bin (list "6080")))])
      (check-pred exn:fail:web3:abi:type? (raised-type-error (lambda () (jsexpr->contract-abi j)))
                  (format "~s" j)))
    (check-equal? (exn-message (raised-type-error
                                (lambda () (jsexpr->contract-abi (hasheq 'abi abi 'bytecode 5)))))
                  "jsexpr->contract-abi: bytecode is not a hex string")
    (define placeholder (string-append "__$" (make-string 34 #\a) "$__"))
    (define linked
      (raised-type-error
       (lambda ()
         (jsexpr->contract-abi
          (hasheq 'abi abi 'bin (string-append "6080" placeholder (make-string 20000 #\0)))))))
    (check-equal? (exn-message linked)
                  (string-append "jsexpr->contract-abi: bytecode has unlinked library references,"
                                 " the first " placeholder))
    (check-regexp-match #rx"item 1: "
                        (exn-message (raised-type-error (lambda () (jsexpr->contract-abi
                                                             (list (first abi) (hasheq 'type "x"))))))))

  (test-case "text that is not one JSON value raises, and a leading byte order mark is skipped"
    (for ([text (list "[1," "[] []" #"\377" "" "[\"\377\"]" "[{\"anonymous\": f" "[tru" "[\"\\u12")])
      (define e (raised-type-error (lambda () (read-contract-abi (open-input-bytes (if (bytes? text)
                                                                                 text
                                                                                 (string->bytes/latin-1 text)))))))
      (check-pred exn:fail:web3:abi:type? e (format "~s" text))
      (when e
        (check-regexp-match #rx"^read-contract-abi: not JSON" (exn-message e))
        (check-equal? (exn:fail:web3:abi:type-input e) 'port)))
    (check-equal? (read-contract-abi (open-input-string "﻿[]")) (contract-abi '()))
    (check-equal? (read-contract-abi (open-input-string " [] \n")) (contract-abi '()))
    (check-equal? (parameterize ([current-input-port (open-input-string "[]")]) (read-contract-abi))
                  (contract-abi '()))
    (with-temporary-directory (dir)
      (define bad (build-path dir "bad.json"))
      (call-with-output-file bad (lambda (out) (write-string "[1," out)))
      (define e (raised-type-error (lambda () (file->contract-abi bad))))
      (check-regexp-match #rx"^file->contract-abi: not JSON" (exn-message e))
      (check-equal? (exn:fail:web3:abi:type-input e) bad)
      (check-exn exn:fail:filesystem? (lambda () (file->contract-abi (build-path dir "missing.json")))))))
