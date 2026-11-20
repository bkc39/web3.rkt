#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         (only-in keccak keccak256)
         (only-in libsecp256k1
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
                  secret-key?)
         (only-in racket/match define-match-expander match-define)
         (only-in "address.rkt" address->checksum-string checksum-address?)
         (only-in (submod "address.rkt" private) digest->address)
         "private/exn.rkt"
         (only-in "private/hex.rkt" bytes->natural)
         "private/opaque.rkt"
         (rename-in "private/signature.rkt" [signature raw-signature])
         "private/who.rkt")

(define (scalar? v)
  (and (exact-integer? v) (< 0 v secp256k1-order)))

(define secret-hex/c
  (opaque/c 'secret-hex/c "64 hexadecimal digits, optionally after 0x"
            (lambda (v) (and (string? v) (regexp-match? #px"^(?:0[xX])?[0-9a-fA-F]{64}$" v)))))
(define signature-scalar/c (opaque/c 'signature-scalar/c "an integer in [1, n)" scalar?))
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
 signature
 signature?
 (contract-out
  [signature-r (opaque-> signature/c signature-scalar/c)]
  [signature-s (opaque-> signature/c signature-scalar/c)]
  [signature-y-parity (opaque-> signature/c y-parity/c)]
  [hex-string->secret-key (opaque-> secret-hex/c secret-key?)]
  [secret-key->address (opaque-> secret-key/c checksum-address?)]
  [public-key->address (opaque-> public-key/c checksum-address?)]
  [sign-digest (opaque-> secret-key/c digest/c signature?)]
  [recover-address (opaque-> signature/c digest/c (or/c #f checksum-address?))]
  [signature-low-s? (opaque-> signature/c boolean?)]
  [signature-v (opaque-> signature/c (or/c 27 28))]
  [signature->bytes (opaque-> signature/c signature-bytes/c)]
  [bytes->signature (opaque-> signature-bytes/c signature?)]
  [hash-message (opaque-> message/c digest/c)]
  [sign-message (opaque-> secret-key/c message/c signature?)]
  [recover-message-address (opaque-> signature/c message/c (or/c #f checksum-address?))]
  [message-signed-by? (opaque-> signature/c message/c address/c boolean?)]))

(module* private #f
  (provide bytes->signature/who
           recoverable->signature
           sign-digest/who))

(define-module-boundary-contract checked-signature raw-signature
  (opaque-> signature-scalar/c signature-scalar/c y-parity/c signature?)
  #:name-for-blame signature)

(define-match-expander signature
  (syntax-parser
    [(_ r s y-parity) #'(raw-signature r s y-parity)])
  (syntax-parser
    [_:id #'checked-signature]
    [(_ . args) #'(checked-signature . args)]))

(define (hex-string->secret-key s)
  (define k (string->number (regexp-replace #rx"^0[xX]" s "") 16))
  (unless (scalar? k)
    (raise-key-error "hex-string->secret-key: invalid secret key;\n ~a"
                     "the value is zero or not less than the curve order"))
  (integer->secret-key k))

(define (secret-key->address key)
  (public-key->address (secret-key->public-key key)))

(define (public-key->address key)
  (digest->address (keccak256 (subbytes (public-key->bytes key #:compressed? #f) 1))))

(define/who (sign-digest who key digest)
  (recoverable->signature who (ecdsa-sign-recoverable digest key)))

(define (recoverable->signature who rsig)
  (define id (ecdsa-recoverable-signature-id rsig))
  (unless (< id 2)
    (raise-signature-error "~a: the signature's recovery id is ~a; Ethereum encodes only 0 and 1"
                           who id))
  (raw-signature (ecdsa-signature-r rsig) (ecdsa-signature-s rsig) id))

(define (recover-address sig digest)
  (match-define (signature r s y-parity) sig)
  (define key (ecdsa-recover (ecdsa-recoverable-signature r s y-parity) digest))
  (and key (public-key->address key)))

(define (signature-low-s? sig)
  (<= (signature-s sig) (quotient secp256k1-order 2)))

(define/who (bytes->signature who bs)
  (define v (bytes-ref bs 64))
  (define r (bytes->natural bs 0 32))
  (define s (bytes->natural bs 32 64))
  (define (invalid fmt . args)
    (apply raise-signature-error (string-append "~a: invalid signature;\n " fmt) who args))
  (cond
    [(not (memv v '(0 1 27 28))) (invalid "v is ~a, not 0, 1, 27 or 28" v)]
    [(not (scalar? r)) (invalid "r is zero or not less than the curve order")]
    [(not (scalar? s)) (invalid "s is zero or not less than the curve order")]
    [else (raw-signature r s (if (>= v 27) (- v 27) v))]))

(define personal-message-prefix #"\x19Ethereum Signed Message:\n")

(define (message->bytes message)
  (if (bytes? message) message (string->bytes/utf-8 message)))

(define (hash-message message)
  (define body (message->bytes message))
  (keccak256 (bytes-append personal-message-prefix
                           (string->bytes/utf-8 (number->string (bytes-length body)))
                           body)))

(define (sign-message key message)
  (sign-digest/who 'sign-message key (hash-message message)))

(define (recover-message-address sig message)
  (recover-address sig (hash-message message)))

(define (message-signed-by? sig message address)
  (equal? (recover-message-address sig message) (address->checksum-string address)))

(define (raise-key-error fmt . args)
  (raise (exn:fail:web3:key (apply format fmt args) (current-continuation-marks))))

(define (raise-signature-error fmt . args)
  (raise (exn:fail:web3:signature (apply format fmt args) (current-continuation-marks))))
