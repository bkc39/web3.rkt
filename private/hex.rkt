#lang racket/base

;; Unchecked conversions shared by web3/rpc, web3/abi and web3/account; callers check
;; their input.  Hex strings here always start with 0x.

(provide bytes->hex
         bytes->natural
         hex->bytes
         hex-data-string?
         natural->bytes)

(define (hex-data-string? v)
  (and (string? v) (regexp-match? #px"^0x(?:[0-9a-fA-F]{2})*$" v)))

(define (bytes->hex bs)
  (string-append
   "0x"
   (apply string-append
          (for/list ([b (in-bytes bs)])
            (string-append (if (< b 16) "0" "") (number->string b 16))))))

(define (hex->bytes h)
  (apply bytes
         (for/list ([i (in-range 2 (string-length h) 2)])
           (string->number (substring h i (+ i 2)) 16))))

(define (natural->bytes n width)
  (define bs (make-bytes width 0))
  (for ([i (in-range width)])
    (bytes-set! bs (- width i 1) (bitwise-and (arithmetic-shift n (* -8 i)) 255)))
  bs)

(define (bytes->natural bs)
  (for/fold ([n 0]) ([b (in-bytes bs)])
    (+ (* n 256) b)))
