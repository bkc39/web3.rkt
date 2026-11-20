#lang racket/base

(require (only-in keccak keccak256)
         racket/match
         web3)

(define (contract-address-preimage sender nonce)
  (rlp-encode (list (hex-string->bytes sender) (natural->rlp-scalar nonce))))

(define (preimage->address preimage)
  (address->checksum-string (bytes->hex-string (subbytes (keccak256 preimage) 12))))

(define (describe-item item)
  (match item
    [(? bytes?) (bytes->hex-string item)]
    [(list items ...) (map describe-item items)]))

(define (print-contract-addresses sender nonces)
  (for ([nonce (in-list nonces)])
    (define preimage (contract-address-preimage sender nonce))
    (printf "nonce ~a\n" nonce)
    (printf "  rlp:     ~a\n" (bytes->hex-string preimage))
    (printf "  decoded: ~s\n" (describe-item (rlp-decode preimage)))
    (printf "  address: ~a\n" (preimage->address preimage))))

(define (print-decoded hexes)
  (for ([hex (in-list hexes)])
    (with-handlers ([exn:fail:web3:rlp?
                     (lambda (e)
                       (printf "~a: ~a at byte ~a\n" hex (exn:fail:web3:rlp-reason e)
                               (exn:fail:web3:rlp-offset e)))])
      (printf "~a: ~s\n" hex (describe-item (rlp-decode (hex-string->bytes hex)))))))

(module+ main
  (print-contract-addresses "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266" '(0 1 128))
  (print-decoded '("0xc88363617483646f67" "0x8105" "0xb800" "0xc0c0" "0xc2")))
