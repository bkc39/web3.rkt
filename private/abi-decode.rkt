#lang racket/base

;; The decoder core.  web3/abi puts the contracts on its callers.

(require racket/contract/base)

(provide abi-decode-reason/c
         decode-arguments)

(define reason-phrases
  '((short . "data too short")
    (offset . "offset out of range")
    (overlap . "overlapping tail")
    (length . "length out of range")
    (non-canonical . "not canonically encoded")
    (utf-8 . "invalid UTF-8")
    (selector . "selector mismatch")))

(define abi-decode-reason/c
  (flat-named-contract 'abi-decode-reason/c (lambda (v) (and (assq v reason-phrases) #t))))

(define (decode-arguments who types data base)
  (error 'unimplemented))
