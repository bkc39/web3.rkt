#lang racket/base

(require (only-in json read-json)
         (only-in racket/file file->bytes)
         (only-in racket/port port->bytes)
         (only-in racket/string string-prefix?)
         (submod "../abi/description.rkt" private)
         "exn.rkt")

(provide file->contract-abi/who
         read-contract-abi/who)

(define (bytes->jsexpr who bs input)
  (define (fail reason)
    (raise (exn:fail:web3:abi:type (format "~a: not JSON: ~a" who reason)
                                   (current-continuation-marks)
                                   input
                                   #f)))
  (define text
    (with-handlers ([exn:fail:contract? (lambda (_) (fail "the text is not UTF-8"))])
      (bytes->string/utf-8 bs)))
  (define in (open-input-string (if (string-prefix? text "﻿") (substring text 1) text)))
  (define j
    (with-handlers ([exn:fail:read? (lambda (e) (fail (exn-message e)))]
                    [exn:fail? (lambda (_) (fail "the text ends inside a value"))])
      (read-json in)))
  (cond
    [(eof-object? j) (fail "no JSON value")]
    [(regexp-match? #px"^\\s*$" in) j]
    [else (fail "text after the JSON value")]))

(define (read-contract-abi/who who in)
  (jsexpr->contract-abi/who who (bytes->jsexpr who (port->bytes in) 'port)))

(define (file->contract-abi/who who path)
  (jsexpr->contract-abi/who who (bytes->jsexpr who (file->bytes path) path)))
