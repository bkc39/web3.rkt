#lang racket/base

;; Checked conversions of what a node returns.  The node is untrusted, so a
;; malformed value raises exn:fail:web3 under the caller's who.  No
;; transport modules: web3/eth/log reads node data through this too.

(require "../address.rkt"
         "exn.rkt"
         (only-in "hex.rkt" hex->bytes))

(provide node-address->string
         node-data->bytes
         node-quantity->number
         receipt-status)

(define (raise-malformed who what v)
  (raise (exn:fail:web3 (format "~a: malformed ~a from the node: ~e" who what v)
                        (current-continuation-marks))))

(define (node-string? rx v)
  (and (string? v) (regexp-match? rx v)))

(define (node-data->bytes who what v)
  (cond
    [(node-string? #px"^0x(?:[0-9a-fA-F]{2})*$" v) (hex->bytes v)]
    [else (raise-malformed who what v)]))

(define (node-quantity->number who what v)
  (cond
    [(node-string? #px"^0x[0-9a-fA-F]+$" v) (string->number (substring v 2) 16)]
    [else (raise-malformed who what v)]))

(define (node-address->string who what v)
  (cond
    [(node-string? #px"^0x[0-9a-fA-F]{40}$" v) (address->checksum-string v)]
    [else (raise-malformed who what v)]))

(define (receipt-status who receipt)
  (define status (hash-ref receipt 'status #f))
  (cond
    [(equal? status "0x1") 'success]
    [(equal? status "0x0") 'failure]
    [else (raise-malformed who "receipt status" status)]))
