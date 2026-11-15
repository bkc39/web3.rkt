#lang racket/base

;; Unchecked conversions shared by web3/rpc and web3/abi; callers check
;; their input.

(provide bytes->hex
         hex-digits->bytes)

(define (bytes->hex bs)
  (string-append
   "0x"
   (apply string-append
          (for/list ([b (in-bytes bs)])
            (string-append (if (< b 16) "0" "") (number->string b 16))))))

(define (hex-digits->bytes h)
  (apply bytes
         (for/list ([i (in-range 0 (string-length h) 2)])
           (string->number (substring h i (+ i 2)) 16))))
