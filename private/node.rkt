#lang racket/base

(require "../address.rkt"
         "exn.rkt"
         (only-in "hex.rkt" hex->bytes hex-data-string?))

(provide node-address->string
         node-data->bytes
         node-quantity->number
         node-status->boolean)

(define (raise-malformed who what v)
  (raise (exn:fail:web3 (format "~a: malformed ~a from the node: ~e" who what v)
                        (current-continuation-marks))))

(define (node-string? rx v)
  (and (string? v) (regexp-match? rx v)))

(define (node-data->bytes who what v)
  (cond
    [(hex-data-string? v) (hex->bytes v)]
    [else (raise-malformed who what v)]))

(define (node-quantity->number who what v)
  (cond
    [(node-string? #px"^0x[0-9a-fA-F]+$" v) (string->number (substring v 2) 16)]
    [else (raise-malformed who what v)]))

(define (node-address->string who what v)
  (cond
    [(node-string? #px"^0x[0-9a-fA-F]{40}$" v) (address->checksum-string v)]
    [else (raise-malformed who what v)]))

(define (node-status->boolean who what v)
  (cond
    [(equal? v "0x1") #t]
    [(equal? v "0x0") #f]
    [else (raise-malformed who what v)]))
