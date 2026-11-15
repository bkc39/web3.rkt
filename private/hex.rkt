#lang racket/base

;; Unchecked conversions shared by web3/rpc and web3/abi; callers check
;; their input.  Hex strings here always start with 0x.

(provide bytes->hex
         hex->bytes)

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
