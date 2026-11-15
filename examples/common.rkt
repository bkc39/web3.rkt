#lang racket/base

(require (for-syntax racket/base)
         racket/format
         racket/match
         racket/pretty
         racket/string
         syntax/parse/define
         (only-in web3/rpc bytes->hex-string))

(provide show
         show-error
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

(define (exn-name e)
  (regexp-replace #rx"^struct:" (symbol->string (vector-ref (struct->vector e) 0)) ""))

(define (print-error e)
  (printf "; ~a\n" (exn-name e))
  (for ([line (in-list (string-split (exn-message e) "\n"))])
    (printf "; ~a\n" line)))

(define (call-and-show-error datum thunk)
  (echo datum)
  (define e (with-handlers ([exn:fail? values]) (thunk) #f))
  (unless e
    (error 'show-error "~s raised no exception" datum))
  (print-error e)
  e)

(define-syntax-parse-rule (show-error e:expr (~optional (~seq #:as id:id)))
  (begin
    (~? (define id (call-and-show-error 'e (lambda () e)))
        (void (call-and-show-error 'e (lambda () e))))))

(define (print-words bs)
  (define-values (selector body)
    (match (modulo (bytes-length bs) 32)
      [4 (values (subbytes bs 0 4) (subbytes bs 4))]
      [_ (values #f bs)]))
  (when selector
    (printf "selector ~a\n" (bytes->hex-string selector)))
  (for ([i (in-range 0 (bytes-length body) 32)])
    (printf "0x~a  ~a\n"
            (~r i #:base 16 #:min-width 3 #:pad-string "0")
            (substring (bytes->hex-string (subbytes body i (+ i 32))) 2))))

(define-syntax-parse-rule (show-words e:expr)
  (begin
    (echo 'e)
    (print-words e)))
