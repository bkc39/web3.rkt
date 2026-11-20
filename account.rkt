#lang racket/base

(require racket/contract/base
         (only-in libsecp256k1
                  digest/c
                  generate-secret-key
                  public-key?
                  secp256k1-order
                  secret-key->public-key
                  secret-key/c
                  secret-key?)
         (only-in "address.rkt" checksum-address?)
         "private/exn.rkt"
         "private/opaque.rkt"
         "private/signature.rkt")

(define secret-hex/c
  (opaque/c 'secret-hex/c "64 hexadecimal digits, optionally after 0x"
            (lambda (v) (and (string? v) (regexp-match? #px"^(?:0[xX])?[0-9a-fA-F]{64}$" v)))))
(define signature-scalar/c
  (opaque/c 'signature-scalar/c "an integer in [1, n)"
            (lambda (v) (and (exact-integer? v) (< 0 v secp256k1-order)))))
(define y-parity/c
  (opaque/c 'y-parity/c "0 or 1" (lambda (v) (or (eqv? v 0) (eqv? v 1)))))
(define signature-bytes/c
  (opaque/c 'signature-bytes/c "65 bytes" (lambda (v) (and (bytes? v) (= (bytes-length v) 65)))))
(define public-key/c (opaque/c 'public-key? "a public key" public-key?))
(define signature/c (opaque/c 'signature? "a signature" signature?))

(provide
 (struct-out exn:fail:web3:key)
 (struct-out exn:fail:web3:signature)
 secret-key?
 generate-secret-key
 secret-key->public-key
 public-key?
 secp256k1-order
 (contract-out
  (struct signature ([r signature-scalar/c]
                     [s signature-scalar/c]
                     [y-parity y-parity/c]))
  [hex-string->secret-key (opaque-> secret-hex/c secret-key?)]
  [secret-key->address (opaque-> secret-key/c checksum-address?)]
  [public-key->address (opaque-> public-key/c checksum-address?)]
  [sign-digest (opaque-> secret-key/c digest/c signature?)]
  [recover-address (opaque-> signature/c digest/c (or/c #f checksum-address?))]
  [signature-low-s? (opaque-> signature/c boolean?)]
  [signature-v (opaque-> signature/c (or/c 27 28))]
  [signature->bytes (opaque-> signature/c signature-bytes/c)]
  [bytes->signature (opaque-> signature-bytes/c signature?)]))

(module* private #f
  (provide recoverable->signature))

(define (hex-string->secret-key s)
  (error 'unimplemented))

(define (secret-key->address key)
  (error 'unimplemented))

(define (public-key->address key)
  (error 'unimplemented))

(define (sign-digest key digest)
  (error 'unimplemented))

(define (recoverable->signature who rsig)
  (error 'unimplemented))

(define (recover-address sig digest)
  (error 'unimplemented))

(define (signature-low-s? sig)
  (error 'unimplemented))

(define (bytes->signature bs)
  (error 'unimplemented))
