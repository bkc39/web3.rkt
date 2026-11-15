#lang racket/base

;; Descriptions against cast's Solidity interfaces and signature hashes.
;; Skipped when cast has no interface command.

(module+ test
  (require json
           racket/file
           racket/list
           racket/string
           rackunit
           web3
           "../examples/common.rkt"
           "abi-gen.rkt"
           "cast.rkt"
           "util.rkt")

  (define (has-interface?)
    (and cast-path
         (with-handlers ([exn:fail? (lambda (_) #f)])
           (run-cast "interface" "--help")
           #t)))

  ;; The items cast declares in the interface, as strings web3 reads, and
  ;; the number of lines naming a struct of cast's library block.
  (define (interface-lines path)
    (define text (run-cast "interface" "--" (path->string path)))
    (define structs (regexp-match* #px"struct ([A-Za-z0-9_]+)" text #:match-select cadr))
    (define body (cadr (regexp-match #px"interface Interface \\{(.*)\\}" text)))
    (define lines
      (for/list ([declaration (in-list (string-split body ";"))]
                 #:unless (string=? (string-trim declaration) ""))
        (for/fold ([line (string-normalize-spaces declaration)])
                  ([rule (in-list '((#px" (?:external|memory|calldata)\\b" "")
                                    (#px"\\( " "(")
                                    (#px" \\)" ")")))])
          (regexp-replace* (car rule) line (cadr rule)))))
    (partition (lambda (line)
                 (not (for/or ([s (in-list structs)]) (regexp-match? (pregexp (format "\\b~a\\b" s)) line))))
               lines))

  (define (item-key i)
    (cond
      [(abi-event? i) (list 'event (abi-signature->string (abi-event->signature i)))]
      [(abi-signature? i) (list (if (abi-error? i) 'error 'function) (abi-signature->string i))]
      [else (list (if (abi-fallback? i) 'fallback 'receive))]))

  (define (check-interface d path)
    (define-values (compared skipped) (interface-lines path))
    (define items (filter-not abi-constructor? (contract-abi-items d)))
    (for ([line (in-list compared)])
      (define item (string->abi-item line))
      (check-equal? (findf (lambda (i) (equal? (item-key i) (item-key item))) items) item line))
    (check-equal? (+ (length compared) (length skipped)) (length items) (path->string path)))

  (cond
    [(not (has-interface?))
     (displayln "skipping description differential: cast has no interface command")]
    [else
     (test-case "each fixture's items are the Solidity declarations cast writes"
       (for ([name (in-list '("Token" "Counter" "Events"))])
         (check-interface (file->contract-abi (artifact-path name)) (artifact-path name))))

     (test-case "canonical function strings hash to cast's selectors"
       (define prng (seeded 6 4 2 8 8 1))
       (define functions
         (append (append* (for/list ([name (in-list '("Token" "Counter" "Events"))])
                            (contract-abi-functions (file->contract-abi (artifact-path name)))))
                 (take (append* (for/list ([_ (in-range 200)])
                                  (contract-abi-functions (random-contract-abi prng #:cast-safe? #t))))
                       200)))
       (for ([f (in-list functions)])
         (check-equal? (run-cast "sig" "--" (abi-item->string f))
                       (bytes->hex-string (function-selector f))
                       (abi-item->string f))))

     (test-case "generated ABIs are the declarations cast writes"
       (define prng (seeded 6 4 2 8 8 2))
       (define dir (make-temporary-directory))
       (for ([i (in-range 50)])
         (define d (random-contract-abi prng #:cast-safe? #t))
         (define path (build-path dir (format "abi-~a.json" i)))
         (call-with-output-file path (lambda (out) (write-json (contract-abi->jsexpr d) out)))
         (check-interface d path))
       (delete-directory/files dir))]))
