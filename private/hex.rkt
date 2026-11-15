#lang racket/base

;; Unchecked conversions shared by web3/rpc and web3/abi; callers check
;; their input.  Hex strings here always start with 0x.

(provide bytes->hex
         hex->bytes
         hex-data-string?)

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
