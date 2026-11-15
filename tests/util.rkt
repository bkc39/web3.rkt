#lang racket/base

(require (for-syntax racket/base
                     (only-in racket/path file-name-from-path))
         (only-in racket/contract exn:fail:contract:blame?)
         racket/list
         racket/match
         rackunit
         syntax/parse/define
         web3/abi
         (only-in web3/rpc bytes->hex-string))

(provide check-blames-here
         encode-hex
         signature-inputs
         widths)

(define widths (range 8 257 8))

(define (signature-inputs signature)
  (abi-signature-inputs (string->abi-signature signature)))

(define (encode-hex mode signature vs)
  (bytes->hex-string
   (match mode
     ['encode (abi-encode (signature-inputs signature) vs)]
     ['packed (abi-encode-packed (signature-inputs signature) vs)]
     ['call (apply encode-call signature vs)])))

(define (blames? file-name e)
  (and (exn:fail:contract:blame? e)
       (regexp-match? (regexp (string-append "blaming: [^\n]*" (regexp-quote file-name)))
                      (exn-message e))))

;; A contract violation blaming the module this form appears in.
(define-syntax-parse-rule (check-blames-here expr)
  #:with file-name
  (datum->syntax #'expr (path->string (file-name-from-path (syntax-source this-syntax))))
  (check-exn (lambda (e) (blames? file-name e))
             (lambda () expr)
             (format "~s" 'expr)))
