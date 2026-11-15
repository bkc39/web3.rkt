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

(define (node-data->bytes who what v)
  (error 'unimplemented))

(define (node-quantity->number who what v)
  (error 'unimplemented))

(define (node-address->string who what v)
  (error 'unimplemented))

(define (receipt-status who receipt)
  (error 'unimplemented))
