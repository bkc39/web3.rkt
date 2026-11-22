#lang racket/base

(require (only-in (submod "../address.rkt" private) address->checksum-string)
         "exn.rkt"
         (only-in "hex.rkt" address-hex-string? hash-hex-string? hex->bytes hex-data-string?))

(provide node-address->string
         node-data->bytes
         node-hash->string
         node-natural
         node-quantity->number
         node-quantity?
         raise-node-malformed
         raise-node-missing)

(define (raise-node-malformed who what v #:origin [origin " from the node"])
  (raise (exn:fail:web3 (format "~a: malformed ~a~a: ~e" who what origin v)
                        (current-continuation-marks))))

(define (raise-node-missing who what)
  (raise-node-malformed who what 'null))

(define (node-string? rx v)
  (and (string? v) (regexp-match? rx v)))

(define (node-data->bytes who what v)
  (cond
    [(hex-data-string? v) (hex->bytes v)]
    [else (raise-node-malformed who what v)]))

;; No Ethereum quantity has more than 64 significant hex digits; a longer one is refused unread.
(define (node-quantity? v)
  (node-string? #px"^0x(?=[0-9a-fA-F])(?>0*)[0-9a-fA-F]{0,64}$" v))

(define (node-quantity->number who what v)
  (cond
    [(node-quantity? v) (string->number (regexp-replace #px"^0x0*(?=.)" v "") 16)]
    [else (raise-node-malformed who what v)]))

(define (node-natural who what v bits)
  (define n (node-quantity->number who what v))
  (if (< n (expt 2 bits)) n (raise-node-malformed who what v)))

(define (node-address->string who what v)
  (cond
    [(address-hex-string? v) (address->checksum-string v)]
    [else (raise-node-malformed who what v)]))

(define (node-hash->string who what v)
  (cond
    [(hash-hex-string? v) (string-downcase v)]
    [else (raise-node-malformed who what v)]))
