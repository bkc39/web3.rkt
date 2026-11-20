#lang racket/base

(require racket/contract/base
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

(define reason-phrases
  (hasheq 'short "input too short"
          'single-byte "non-canonical single byte"
          'size "non-canonical size"
          'leading-zero "leading zero"
          'trailing "trailing bytes"
          'list "unexpected list"))

(define rlp-reason/c
  (flat-named-contract 'rlp-reason/c (apply or/c (hash-keys reason-phrases #t))))

(define (raise-rlp-error who reason offset detail)
  (raise (exn:fail:web3:rlp (format "~a: ~a: ~a~a" who (hash-ref reason-phrases reason) detail
                                    (if offset (format "\n  offset: ~a" offset) ""))
                            (current-continuation-marks)
                            offset
                            reason)))

(define (byte-count n)
  (format "~a byte~a" n (if (= n 1) "" "s")))

(define (rlp-item? v)
  (let item? ([v v] [enclosing #hasheq()])
    (or (bytes? v)
        (and (list? v)
             (not (hash-ref enclosing v #f))
             (let ([enclosing (hash-set enclosing v #t)])
               (for/and ([element (in-list v)])
                 (item? element enclosing)))))))

(define (natural->rlp-scalar n)
  (natural->bytes n (quotient (+ (integer-length n) 7) 8)))

(define (rlp-encode item)
  (define-values (pieces _) (encode-onto item '()))
  (define out (open-output-bytes))
  (for ([piece (in-list pieces)])
    (write-bytes piece out))
  (get-output-bytes out))

(define (encode-onto item rest)
  (cond
    [(bytes? item)
     (if (and (= (bytes-length item) 1) (< (bytes-ref item 0) #x80))
         (values (cons item rest) 1)
         (with-header #x80 (cons item rest) (bytes-length item)))]
    [else
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

(define (rlp-decode bs)
  (rlp-decode/who 'rlp-decode bs))

(define (rlp-decode/who who bs [start 0])
  (define end (bytes-length bs))
  (define-values (item next) (decode-item who bs start end))
  (when (< next end)
    (raise-rlp-error who 'trailing next (format "~a after the item" (byte-count (- end next)))))
  item)

(define (decode-item who bs start end)
  (define-values (nested? payload-start payload-end) (read-header who bs start end))
  (values (if nested?
              (decode-children who bs payload-start payload-end)
              (subbytes bs payload-start payload-end))
          payload-end))

(define (decode-children who bs start end)
  (let loop ([start start] [items '()])
    (cond
      [(= start end) (reverse items)]
      [else
       (define-values (item next) (decode-item who bs start end))
       (loop next (cons item items))])))

;; Returns only once the payload is known to fit before end: nothing is read or allocated for a
;; claimed size before that.
(define (read-header who bs start end)
  (define prefix (and (< start end) (bytes-ref bs start)))
  (define single (and prefix (= prefix #x81) (< (add1 start) end) (bytes-ref bs (add1 start))))
  (cond
    [(not prefix) (raise-short who start end 1)]
    [(< prefix #x80) (values #f start (add1 start))]
    [(and single (< single #x80))
     (raise-rlp-error who 'single-byte start
                      (format "~a is its own encoding" (bytes->hex (bytes single))))]
    [(<= prefix #xb7) (payload who start end #f (add1 start) (- prefix #x80))]
    [(<= prefix #xbf) (long-payload who bs start end #f (- prefix #xb7))]
    [(<= prefix #xf7) (payload who start end #t (add1 start) (- prefix #xc0))]
    [else (long-payload who bs start end #t (- prefix #xf7))]))

(define (raise-short who start end needed)
  (raise-rlp-error who 'short start
                   (format "~a needed, ~a left" (byte-count needed) (byte-count (- end start)))))

(define (payload who start end nested? payload-start size)
  (define payload-end (+ payload-start size))
  (if (> payload-end end)
      (raise-short who start end (- payload-end start))
      (values nested? payload-start payload-end)))

(define (long-payload who bs start end nested? size-length)
  (define payload-start (+ start 1 size-length))
  (define size (and (<= payload-start end) (bytes->natural bs (add1 start) payload-start)))
  (cond
    [(not size) (raise-short who start end (add1 size-length))]
    [(zero? (bytes-ref bs (add1 start)))
     (raise-rlp-error who 'leading-zero start "in the length of the item")]
    [(< size 56)
     (raise-rlp-error who 'size start (format "a length of ~a takes the short form" size))]
    [else (payload who start end nested? payload-start size)]))

(define/who (rlp-scalar->natural who item)
  (cond
    [(not (bytes? item)) (raise-rlp-error who 'list #f "a scalar is a byte string")]
    [(and (positive? (bytes-length item)) (zero? (bytes-ref item 0)))
     (raise-rlp-error who 'leading-zero #f
                      (format "a scalar of ~a starts with 0x00" (byte-count (bytes-length item))))]
    [else (bytes->natural item)]))
