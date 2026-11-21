#lang racket/base

;; Unchecked conversions: callers check their input.  Hex strings here always start with 0x.

(provide bytes->hex
         bytes->natural
         hex->bytes
         hex-data-string?
         natural->bytes
         natural->hex)

(define (hex-data-string? v)
  (and (string? v) (regexp-match? #px"^0x(?:[0-9a-fA-F]{2})*$" v)))

(define (bytes->hex bs)
  (string-append
   "0x"
   (apply string-append
          (for/list ([b (in-bytes bs)])
            (string-append (if (< b 16) "0" "") (number->string b 16))))))

(define (natural->hex n)
  (string-append "0x" (number->string n 16)))

(define (hex->bytes h)
  (apply bytes
         (for/list ([i (in-range 2 (string-length h) 2)])
           (string->number (substring h i (+ i 2)) 16))))

;; Byte-at-a-time loops are quadratic on bignums: above this many bytes both conversions split
;; in halves, so each level of the split costs time linear in the whole.
(define split-above 64)

(define (natural->bytes n width)
  (define bs (make-bytes width 0))
  (let fill! ([n (bitwise-bit-field n 0 (* 8 width))] [start 0] [end width])
    (cond
      [(<= (- end start) split-above)
       (for ([i (in-range start end)])
         (define shift (* 8 (- end i 1)))
         (bytes-set! bs i (bitwise-bit-field n shift (+ shift 8))))]
      [else
       (define middle (quotient (+ start end) 2))
       (define low-bits (* 8 (- end middle)))
       (fill! (arithmetic-shift n (- low-bits)) start middle)
       (fill! (bitwise-bit-field n 0 low-bits) middle end)]))
  bs)

(define (bytes->natural bs [start 0] [end (bytes-length bs)])
  (let natural ([start start] [end end])
    (cond
      [(<= (- end start) split-above)
       (for/fold ([n 0]) ([b (in-bytes bs start end)])
         (+ (* n 256) b))]
      [else
       (define middle (quotient (+ start end) 2))
       (+ (arithmetic-shift (natural start middle) (* 8 (- end middle)))
          (natural middle end))])))
