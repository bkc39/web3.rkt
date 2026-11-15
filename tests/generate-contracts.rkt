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

  (for ([source (in-list (sort (directory-list contracts-dir #:build? #t) path<?))]
        #:when (path-has-extension? source #".sol"))
    (define name (path->string (path-replace-extension (file-name-from-path source) #"")))
    (define out (artifact-path name))
    (make-directory* (path-only out))
    (call-with-output-file out #:exists 'truncate
      (lambda (port)
        (write-json (compile-fixture source) port #:indent 2)
        (newline port)))
    (printf "wrote ~a\n" out)))
