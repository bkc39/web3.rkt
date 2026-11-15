#lang racket/base

;; Shared by the examples.  `show` prints an expression and then its value,
;; so running an example reads like a REPL transcript; `show-words` prints
;; ABI-encoded bytes one 32-byte word per line.

(require racket/format
         racket/pretty
         racket/string
         syntax/parse/define
         (only-in web3/rpc bytes->hex-string))

(provide show
         show-words
         section)

(define (section title)
  (printf "\n;;;; ~a\n\n" title))

(define (echo datum)
  (define lines (string-split (pretty-format datum (- (pretty-print-columns) 2) #:mode 'write) "\n"))
  (printf "> ~a\n" (string-join lines "\n  ")))

(define-syntax-parse-rule (show e:expr)
  (begin
    (echo 'e)
    (call-with-values
     (lambda () e)
     (lambda vs
       (for ([v (in-list vs)]
             #:unless (void? v))
         (pretty-print v))))))

(define (print-words bs)
  (define start (if (= (modulo (bytes-length bs) 32) 4) 4 0))
  (when (= start 4)
    (printf "selector ~a\n" (bytes->hex-string (subbytes bs 0 4))))
  (for ([i (in-range start (bytes-length bs) 32)])
    (printf "0x~a  ~a\n"
            (~r (- i start) #:base 16 #:min-width 3 #:pad-string "0")
            (substring (bytes->hex-string (subbytes bs i (+ i 32))) 2))))

(define-syntax-parse-rule (show-words e:expr)
  (begin
    (echo 'e)
    (print-words e)))
