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

(define solc-path
  (let ([env (getenv "WEB3_SOLC")])
    (if (and env (not (string=? env "")))
        env
        (find-executable-path "solc"))))

(define (solc-version)
  (error 'unimplemented))

(define (compile-fixture path)
  (error 'unimplemented))
