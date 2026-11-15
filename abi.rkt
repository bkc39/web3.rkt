#lang racket/base

(require racket/contract/base
         (only-in keccak keccak256)
         (only-in racket/function negate)
         (only-in racket/list make-list)
         (only-in racket/match match struct*)
         "abi/type.rkt"
         (submod "abi/type.rkt" private)
         (only-in "private/exn.rkt"
                  exn:fail:web3:rpc?
                  exn:fail:web3:rpc:revert
                  exn:fail:web3:rpc:revert?
                  struct:exn:fail:web3:rpc:revert
                  exn:fail:web3:rpc:revert-revert))

(provide (all-from-out "abi/type.rkt")
         (struct-out exn:fail:web3:rpc:revert)
         (contract-out
          [function-selector (-> (or/c string? bytes? abi-signature?) bytes?)]
          [word->bytes (-> (integer-in 0 (sub1 (expt 2 256))) bytes?)]
          [bytes->words (-> bytes? (listof exact-nonnegative-integer?))]
          [abi-encode (-> (listof abi-type-spec/c) list? bytes?)]
          [abi-encode-packed (-> (listof abi-type-spec/c) list? bytes?)]
          [encode-call (->* ((or/c string? abi-signature?)) #:rest list? bytes?)]
          [abi-decode (-> (listof abi-type-spec/c) bytes? list?)]
          [decode-result (-> (or/c string? abi-signature?) bytes? list?)]
          [decode-call (-> (or/c string? abi-signature?) bytes? list?)]
          (struct abi-revert ([signature (or/c #f abi-signature?)]
                              [arguments list?]
                              [data bytes?]))
          [abi-revert-name (-> abi-revert? (or/c #f string?))]
          [abi-revert-selector (-> abi-revert? (or/c #f bytes?))]
          [abi-revert->string (-> abi-revert? string?)]
          [error-selector (-> error-signature/c bytes?)]
          [decode-revert (->* (bytes?) (#:errors (listof error-signature/c)) abi-revert?)]
          [panic-description (-> exact-nonnegative-integer? (or/c #f string?))]
          [rpc-exn->revert (->* (exn:fail:web3:rpc?)
                                (#:errors (listof error-signature/c))
                                (or/c #f abi-revert?))]
          [rpc-exn->revert-exn (->* (exn:fail:web3:rpc?)
                                    (#:errors (listof error-signature/c))
                                    (or/c #f exn:fail:web3:rpc:revert?))]
          [decode-revert-reason (-> bytes? (or/c string? #f))]))

(define error-signature/c
  (flat-named-contract 'error-signature/c (or/c string? abi-signature?)))

(module+ test
  (require rackunit
           (only-in "rpc.rkt" bytes->hex-string)))

(define (function-selector signature)
  (define preimage
    (match signature
      [(? bytes?) signature]
      [(? string?) (abi-signature->string (string->signature 'function-selector signature))]
      [_ (abi-signature->string signature)]))
  (subbytes (keccak256 preimage) 0 4))

(define (word->bytes n)
  (natural->bytes n 32))

(define (natural->bytes n width)
  (define bs (make-bytes width 0))
  (for ([i (in-range width)])
    (bytes-set! bs (- width i 1) (bitwise-and (arithmetic-shift n (* -8 i)) 255)))
  bs)

(define (bytes->words bs)
  (for/list ([i (in-range 0 (bytes-length bs) 32)])
    (for/fold ([n 0]) ([b (in-bytes bs i (min (bytes-length bs) (+ i 32)))])
      (+ (* n 256) b))))

(define (address->natural a)
  (string->number (substring a 2) 16))

(define (pad-right bs)
  (bytes-append bs (make-bytes (modulo (- (bytes-length bs)) 32) 0)))

(define (encode-bytes bs)
  (bytes-append (word->bytes (bytes-length bs)) (pad-right bs)))

;; Values are checked before encoding starts.
(define (encode t v)
  (match t
    [(struct* abi-uint ()) (word->bytes v)]
    [(struct* abi-int ()) (word->bytes (modulo v (expt 2 256)))]
    [(struct* abi-address ()) (word->bytes (address->natural v))]
    [(struct* abi-bool ()) (word->bytes (if v 1 0))]
    [(struct* abi-fixed-bytes ()) (pad-right v)]
    [(struct* abi-bytes ()) (encode-bytes v)]
    [(struct* abi-string ()) (encode-bytes (string->bytes/utf-8 v))]
    [(struct* abi-array ([element e]))
     (bytes-append (word->bytes (length v)) (encode-sequence (make-list (length v) e) v))]
    [(struct* abi-fixed-array ([element e] [length k])) (encode-sequence (make-list k e) v)]
    [(struct* abi-tuple ([components cs])) (encode-sequence cs v)]))

(define (encode-sequence ts vs)
  (define head-size (for/sum ([t (in-list ts)]) (or (static-size t) 32)))
  (define-values (heads tails _end)
    (for/fold ([heads '()] [tails '()] [offset head-size])
              ([t (in-list ts)]
               [v (in-list vs)])
      (define encoded (encode t v))
      (if (dynamic? t)
          (values (cons (word->bytes offset) heads) (cons encoded tails) (+ offset (bytes-length encoded)))
          (values (cons encoded heads) tails offset))))
  (apply bytes-append (append (reverse heads) (reverse tails))))

(define (encode-arguments who ts vs)
  (cond
    [(arguments-failure ts vs) => (lambda (failure) (raise-abi-value-error who failure))]
    [else (encode-sequence ts vs)]))

(define (abi-encode types vs)
  (encode-arguments 'abi-encode (types/who 'abi-encode types) vs))

(define (packable? t)
  (match t
    [(or (struct* abi-array ([element e])) (struct* abi-fixed-array ([element e])))
     (not (or (dynamic? e) (abi-fixed-array? e) (abi-tuple? e)))]
    [(struct* abi-tuple ()) #f]
    [_ #t]))

(define (encode-packed t v)
  (match t
    [(struct* abi-uint ([bits n])) (natural->bytes v (quotient n 8))]
    [(struct* abi-int ([bits n])) (natural->bytes (modulo v (expt 2 n)) (quotient n 8))]
    [(struct* abi-address ()) (natural->bytes (address->natural v) 20)]
    [(struct* abi-bool ()) (bytes (if v 1 0))]
    [(struct* abi-fixed-bytes ()) v]
    [(struct* abi-bytes ()) v]
    [(struct* abi-string ()) (string->bytes/utf-8 v)]
    [(or (struct* abi-array ([element e])) (struct* abi-fixed-array ([element e])))
     (apply bytes-append (map (lambda (x) (encode e x)) v))]))

(define (abi-encode-packed types vs)
  (define ts (types/who 'abi-encode-packed types))
  (cond
    [(findf (negate packable?) ts)
     => (lambda (t)
          (raise-abi-type-error 'abi-encode-packed "not supported in packed encoding"
                                (type->string t) #f))]
    [(arguments-failure ts vs)
     => (lambda (failure) (raise-abi-value-error 'abi-encode-packed failure))]
    [else (apply bytes-append (map encode-packed ts vs))]))

(define (encode-call signature . vs)
  (define sig
    (if (string? signature) (string->signature 'encode-call signature) signature))
  (bytes-append (function-selector sig)
                (encode-arguments 'encode-call (abi-signature-inputs sig) vs)))

(define (abi-decode types data)
  (error 'unimplemented))

(define (decode-result signature data)
  (error 'unimplemented))

(define (decode-call signature data)
  (error 'unimplemented))

(struct abi-revert (signature arguments data) #:transparent)

(define (abi-revert-name r)
  (error 'unimplemented))

(define (abi-revert-selector r)
  (error 'unimplemented))

(define (abi-revert->string r)
  (error 'unimplemented))

(define (error-selector signature)
  (error 'unimplemented))

(define (decode-revert data #:errors [errors '()])
  (error 'unimplemented))

(define (panic-description code)
  (error 'unimplemented))

(define (rpc-exn->revert e #:errors [errors '()])
  (error 'unimplemented))

(define (rpc-exn->revert-exn e #:errors [errors '()])
  (error 'unimplemented))

(define (decode-revert-reason data)
  (error 'unimplemented))

;; Uncontracted entry points for web3's own modules, which raise under
;; the name of the function their caller called.
(module* internal #f
  (provide abi-decode/who
           abi-encode/who
           abi-value->display-string
           check-error-signatures/who
           decode-result/who
           encode-call/who
           string->abi-signature/who))

(define (string->abi-signature/who who signature)
  (error 'unimplemented))

(define (abi-encode/who who types vs)
  (error 'unimplemented))

(define (encode-call/who who signature vs)
  (error 'unimplemented))

(define (abi-decode/who who types data base)
  (error 'unimplemented))

(define (decode-result/who who signature data)
  (error 'unimplemented))

(define (check-error-signatures/who who errors)
  (error 'unimplemented))

(define (abi-value->display-string type v)
  (error 'unimplemented))

(module+ test
  (check-equal? (bytes->hex-string (function-selector "transfer(address,uint256)")) "0xa9059cbb")
  (check-equal? (bytes->hex-string (function-selector "inc()")) "0x371303c0")
  (check-equal? (bytes->words (bytes-append (word->bytes 1) (word->bytes (sub1 (expt 2 256)))))
                (list 1 (sub1 (expt 2 256))))
  (check-equal? (bytes-length (encode-call "f(uint256,uint256)" 1 2)) 68))
