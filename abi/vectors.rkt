#lang racket/base

(require racket/contract/base
         (only-in racket/function curry)
         (only-in racket/port port->list)
         racket/runtime-path)

(provide (contract-out
          (struct abi-vector ([mode (or/c 'encode 'packed 'call)]
                              [name string?]
                              [signature string?]
                              [values list?]
                              [hex string?]))
          [abi-encode-vectors (listof abi-vector?)]))

(define-runtime-path encode-path "vectors/encode.rktd")

(struct abi-vector (mode name signature values hex) #:transparent)

(define abi-encode-vectors
  (call-with-input-file encode-path
    (lambda (in)
      (map (curry apply abi-vector) (port->list read in)))))
