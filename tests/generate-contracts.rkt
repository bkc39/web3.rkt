#lang racket/base

;; Recompiles examples/contracts/*.sol into examples/contracts/out/:
;;
;;   racket tests/generate-contracts.rkt

(module+ main
  (require json
           racket/file
           racket/path
           "../examples/common.rkt"
           "helpers/solc.rkt")

  (for ([source (in-list (fixture-sources))])
    (define out (artifact-path (source-name source)))
    (make-directory* (path-only out))
    (call-with-output-file out #:exists 'truncate
      (lambda (port)
        (write-json (compile-fixture source) port #:indent 2)
        (newline port)))
    (printf "wrote ~a\n" out)))
