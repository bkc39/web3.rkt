#lang racket/base

(require racket/contract/base
         (only-in racket/function curry)
         (only-in racket/list partition)
         (only-in racket/port port->list)
         racket/runtime-path)

(provide (contract-out
          (struct abi-vector ([mode (or/c 'encode 'packed 'call 'decode)]
                              [name string?]
                              [signature string?]
                              [values list?]
                              [hex string?]))
          (struct abi-reject-vector ([name string?]
                                     [signature string?]
                                     [hex string?]
                                     [reason symbol?]
                                     [offset exact-nonnegative-integer?]))
          [abi-encode-vectors (listof abi-vector?)]
          [abi-decode-vectors (listof abi-vector?)]
          [abi-reject-vectors (listof abi-reject-vector?)]))

(define-runtime-path encode-path "vectors/encode.rktd")
(define-runtime-path decode-path "vectors/decode.rktd")

(struct abi-vector (mode name signature values hex) #:transparent)
(struct abi-reject-vector (name signature hex reason offset) #:transparent)

(define (read-rows path)
  (call-with-input-file path (lambda (in) (port->list read in))))

(define abi-encode-vectors
  (map (curry apply abi-vector) (read-rows encode-path)))

(define-values (abi-decode-vectors abi-reject-vectors)
  (let-values ([(decode reject)
                (partition (lambda (row) (eq? (car row) 'decode)) (read-rows decode-path))])
    (values (map (curry apply abi-vector) decode)
            (map (lambda (row) (apply abi-reject-vector (cdr row))) reject))))
