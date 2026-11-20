#lang racket/base

(require racket/list
         racket/string
         rackunit
         (only-in web3/private/hex bytes->hex bytes->natural))

(provide check-no-leak
         leaks?
         marker
         marker-hex
         marker-integer)

;; A secret no message has reason to contain: the bytes #x11 ... #x30.
(define marker (list->bytes (range #x11 #x31)))
(define marker-hex (substring (bytes->hex marker) 2))
(define marker-integer (bytes->natural marker))

(define (windows s width)
  (for/list ([i (in-range (add1 (- (string-length s) width)))])
    (string-downcase (substring s i (+ i width)))))

(define (leak-patterns secret)
  (remove-duplicates
   (append (windows (substring (bytes->hex secret) 2) 8)
           (windows (format "~s" secret) 8)
           (windows (number->string (bytes->natural secret)) 8))))

(define (leaks? text [secret marker])
  (define folded (string-downcase text))
  (for/or ([pattern (in-list (leak-patterns secret))])
    (string-contains? folded pattern)))

(define (check-no-leak text [name ""] #:secret [secret marker])
  (check-false (leaks? text secret) name))
