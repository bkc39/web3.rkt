#lang racket/base

;; Compiling the fixture contracts with the pinned solc.  Other versions
;; are refused: the artifacts must be reproducible, and some releases hang
;; on fixed-size array types.

(require json
         racket/file
         racket/match
         racket/path
         racket/port
         racket/string)

(provide compile-fixture
         pinned-solc-version
         solc-path
         solc-version)

(define pinned-solc-version "0.8.33")

(define solc-seconds 300)

(define solc-path
  (let ([env (getenv "WEB3_SOLC")])
    (if (and env (not (string=? env "")))
        env
        (find-executable-path "solc"))))

;; "0.8.33+commit.64118f21", as solc's metadata records it, or #f.
(define (solc-version)
  (and solc-path
       (match (regexp-match #px"Version: ([0-9.]+\\+commit\\.[0-9a-f]+)" (run-solc #"" "--version"))
         [(list _ version) version]
         [#f #f])))

(define (pinned? version)
  (and version (string-prefix? version (string-append pinned-solc-version "+commit."))))

(define (run-solc input . args)
  (define-values (process out in err) (apply subprocess #f #f #f solc-path args))
  (define (collect port)
    (define result (box #f))
    (values result (thread (lambda () (set-box! result (port->string port))))))
  (define-values (stdout stdout-reader) (collect out))
  (define-values (stderr stderr-reader) (collect err))
  (write-bytes input in)
  (close-output-port in)
  (unless (sync/timeout solc-seconds process)
    (subprocess-kill process #t)
    (error 'solc "no result after ~as: solc ~a" solc-seconds (string-join args " ")))
  (thread-wait stdout-reader)
  (thread-wait stderr-reader)
  (close-input-port out)
  (close-input-port err)
  (unless (zero? (subprocess-status process))
    (error 'solc "solc ~a failed: ~a" (string-join args " ") (string-trim (unbox stderr))))
  (unbox stdout))

(define (standard-input file-name source)
  (hasheq 'language "Solidity"
          'sources (hash (string->symbol file-name) (hasheq 'content source))
          'settings
          (hasheq 'evmVersion "cancun"
                  'optimizer (hasheq 'enabled #f 'runs 200)
                  'outputSelection
                  (hasheq '* (hasheq '* '("abi"
                                          "evm.bytecode.object"
                                          "evm.deployedBytecode.object"
                                          "evm.methodIdentifiers"
                                          "metadata"))))))

;; The artifact of the contract named like the file, in forge's layout.
(define (compile-fixture path)
  (define version (solc-version))
  (unless (pinned? version)
    (error 'compile-fixture "solc ~a at ~a is not ~a" version solc-path pinned-solc-version))
  (define file-name (path->string (file-name-from-path path)))
  (define name (path->string (path-replace-extension (file-name-from-path path) #"")))
  (define output
    (string->jsexpr
     (run-solc (jsexpr->bytes (standard-input file-name (file->string path))) "--standard-json")))
  (define failures
    (for/list ([e (in-list (hash-ref output 'errors '()))]
               #:when (equal? (hash-ref e 'severity #f) "error"))
      (hash-ref e 'formattedMessage)))
  (unless (null? failures)
    (error 'compile-fixture "~a:\n~a" file-name (string-join failures "\n")))
  (match-define (hash-table ['abi abi] ['evm evm] ['metadata metadata])
    (hash-ref (hash-ref (hash-ref output 'contracts) (string->symbol file-name)) (string->symbol name)))
  (define (object key)
    (hasheq 'object (string-append "0x" (hash-ref (hash-ref evm key) 'object))))
  (hasheq 'abi abi
          'bytecode (object 'bytecode)
          'deployedBytecode (object 'deployedBytecode)
          'methodIdentifiers (hash-ref evm 'methodIdentifiers)
          'metadata (string->jsexpr metadata)))
