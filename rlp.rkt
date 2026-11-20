#lang racket/base

(require racket/contract/base
         (only-in racket/match match)
         "private/exn.rkt"
         (only-in "private/hex.rkt" bytes->hex bytes->natural natural->bytes)
         "private/who.rkt")

(provide (struct-out exn:fail:web3:rlp)
         (contract-out
          [rlp-item? (-> any/c boolean?)]
          [rlp-encode (-> rlp-item? bytes?)]
          [rlp-decode (-> bytes? rlp-item?)]
          [natural->rlp-scalar (-> exact-nonnegative-integer? bytes?)]
          [rlp-scalar->natural (-> rlp-item? exact-nonnegative-integer?)]))

(module* internal #f
  (provide rlp-decode/who
           rlp-scalar->natural/who
           rlp-reason/c))

(define rlp-reason/c (or/c 'short 'single-byte 'size 'leading-zero 'trailing 'list))

(define phrases
  #hasheq((short . "input too short")
          (single-byte . "non-canonical single byte")
          (size . "non-canonical size")
          (leading-zero . "leading zero")
          (trailing . "trailing bytes")
          (list . "unexpected list")))

(define (raise-rlp-error who reason offset detail)
  (raise (exn:fail:web3:rlp (format "~a: ~a: ~a~a" who (hash-ref phrases reason) detail
                                    (if offset (format "\n  offset: ~a" offset) ""))
                            (current-continuation-marks)
                            offset
                            reason)))

(define (byte-count n)
  (format "~a byte~a" n (if (= n 1) "" "s")))

(define (rlp-item? v)
  (or (bytes? v) (rlp-list? v)))

(define (rlp-list? v)
  (match v
    ['() #t]
    [(cons x rest) (and (rlp-item? x) (rlp-list? rest))]
    [_ #f]))

(define (natural->rlp-scalar n)
  (natural->bytes n (quotient (+ (integer-length n) 7) 8)))

(define (rlp-encode item)
  (define-values (pieces size) (encode-onto item '()))
  (define out (make-bytes size))
  (for/fold ([at 0]) ([piece (in-list pieces)])
    (bytes-copy! out at piece)
    (+ at (bytes-length piece)))
  out)

;; The pieces of item's encoding consed in order onto rest, and their total size.
(define (encode-onto item rest)
  (match item
    [(? bytes?)
     #:when (and (= (bytes-length item) 1) (< (bytes-ref item 0) #x80))
     (values (cons item rest) 1)]
    [(? bytes?) (with-header #x80 (cons item rest) (bytes-length item))]
    [_
     (define-values (pieces payload)
       (for/fold ([pieces rest] [payload 0]) ([x (in-list (reverse item))])
         (define-values (more size) (encode-onto x pieces))
         (values more (+ payload size))))
     (with-header #xc0 pieces payload)]))

(define (with-header base pieces payload)
  (define header
    (if (< payload 56)
        (bytes (+ base payload))
        (let ([size (natural->rlp-scalar payload)])
          (bytes-append (bytes (+ base 55 (bytes-length size))) size))))
  (values (cons header pieces) (+ (bytes-length header) payload)))

(define/who (rlp-decode who bs)
  (define end (bytes-length bs))
  (define-values (item next) (decode-item who bs 0 end))
  (when (< next end)
    (raise-rlp-error who 'trailing next (format "~a after the item" (byte-count (- end next)))))
  item)

(define (decode-item who bs i e)
  (define-values (nested? start length) (read-header who bs i e))
  (define end (+ start length))
  (values (if nested? (decode-children who bs start end) (subbytes bs start end)) end))

(define (decode-children who bs start end)
  (let loop ([i start] [items '()])
    (cond
      [(= i end) (reverse items)]
      [else
       (define-values (item next) (decode-item who bs i end))
       (loop next (cons item items))])))

;; Returns only once the payload is known to end by e: nothing is read or allocated for a claimed
;; length before that.
(define (read-header who bs i e)
  (define (fault reason detail)
    (raise-rlp-error who reason i detail))
  (define (short needed)
    (fault 'short (format "~a needed, ~a left" (byte-count needed) (byte-count (- e i)))))
  (define (payload nested? start length)
    (if (> (+ start length) e)
        (short (- (+ start length) i))
        (values nested? start length)))
  (define (long nested? k)
    (define start (+ i 1 k))
    (define length (and (<= start e) (bytes->natural bs (add1 i) start)))
    (cond
      [(not length) (short (add1 k))]
      [(zero? (bytes-ref bs (add1 i))) (fault 'leading-zero "in the length of the item")]
      [(< length 56) (fault 'size (format "a length of ~a takes the short form" length))]
      [else (payload nested? start length)]))
  (define p (and (< i e) (bytes-ref bs i)))
  (define single (and p (= p #x81) (< (add1 i) e) (bytes-ref bs (add1 i))))
  (cond
    [(not p) (short 1)]
    [(< p #x80) (values #f i 1)]
    [(and single (< single #x80))
     (fault 'single-byte (format "~a is its own encoding" (bytes->hex (bytes single))))]
    [(<= p #xb7) (payload #f (add1 i) (- p #x80))]
    [(<= p #xbf) (long #f (- p #xb7))]
    [(<= p #xf7) (payload #t (add1 i) (- p #xc0))]
    [else (long #t (- p #xf7))]))

(define/who (rlp-scalar->natural who item)
  (match item
    [(? bytes?)
     #:when (and (positive? (bytes-length item)) (zero? (bytes-ref item 0)))
     (raise-rlp-error who 'leading-zero #f
                      (format "a scalar of ~a starts with 0x00" (byte-count (bytes-length item))))]
    [(? bytes?) (bytes->natural item)]
    [_ (raise-rlp-error who 'list #f "a scalar is a byte string")]))
