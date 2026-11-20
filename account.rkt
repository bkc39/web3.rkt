#lang racket/base

(require racket/contract/base
         (only-in keccak keccak256)
         (only-in libsecp256k1
                  digest/c
                  ecdsa-recover
                  ecdsa-recoverable-signature
                  ecdsa-recoverable-signature-id
                  ecdsa-sign-recoverable
                  ecdsa-signature-r
                  ecdsa-signature-s
                  generate-secret-key
                  integer->secret-key
                  public-key->bytes
                  public-key?
                  secp256k1-order
                  secret-key->public-key
                  secret-key/c
                  secret-key?)
         (only-in racket/match match-define)
         (only-in "address.rkt" address->checksum-string checksum-address?)
         "private/exn.rkt"
         (only-in "private/hex.rkt" bytes->hex bytes->natural)
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
  (define k (string->number (regexp-replace #rx"^0[xX]" s "") 16))
  (unless (< 0 k secp256k1-order)
    (raise (exn:fail:web3:key
            (string-append "hex-string->secret-key: invalid secret key;\n"
                           " the value is zero or not less than the curve order")
            (current-continuation-marks))))
  (integer->secret-key k))

(define (secret-key->address key)
  (public-key->address (secret-key->public-key key)))

(define (public-key->address key)
  (define point (subbytes (public-key->bytes key #:compressed? #f) 1))
  (address->checksum-string (bytes->hex (subbytes (keccak256 point) 12))))

(define (sign-digest key digest)
  (recoverable->signature 'sign-digest (ecdsa-sign-recoverable digest key)))

(define (recoverable->signature who rsig)
  (define id (ecdsa-recoverable-signature-id rsig))
  (unless (< id 2)
    (raise-signature-error "~a: the signature's recovery id is ~a; Ethereum encodes only 0 and 1"
                           who id))
  (signature (ecdsa-signature-r rsig) (ecdsa-signature-s rsig) id))

(define (recover-address sig digest)
  (match-define (signature r s y-parity) sig)
  (define key (ecdsa-recover (ecdsa-recoverable-signature r s y-parity) digest))
  (and key (public-key->address key)))

(define (signature-low-s? sig)
  (<= (signature-s sig) (quotient secp256k1-order 2)))

(define (scalar? n)
  (< 0 n secp256k1-order))

(define (bytes->signature bs)
  (define v (bytes-ref bs 64))
  (define r (bytes->natural (subbytes bs 0 32)))
  (define s (bytes->natural (subbytes bs 32 64)))
  (define (invalid fmt . args)
    (apply raise-signature-error (string-append "bytes->signature: invalid signature;\n " fmt)
           args))
  (cond
    [(not (memv v '(0 1 27 28))) (invalid "v is ~a, not 0, 1, 27 or 28" v)]
    [(not (scalar? r)) (invalid "r is zero or not less than the curve order")]
    [(not (scalar? s)) (invalid "s is zero or not less than the curve order")]
    [else (signature r s (if (>= v 27) (- v 27) v))]))

(define (raise-signature-error fmt . args)
  (raise (exn:fail:web3:signature (apply format fmt args) (current-continuation-marks))))
