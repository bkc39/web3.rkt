#lang racket/base

(require (only-in racket/match match match-define)
         (only-in racket/string string-trim)
         (only-in "../util.rkt" pick)
         (only-in "accounts.rkt" random-natural))

(provide canonical
         garbage-string
         model-precision-index
         model-value
         random-amount-string
         random-imprecise-string
         random-wei
         syntax-rows
         unit-decimals
         units)

(define units '(wei gwei ether 0 1 2 6 8 9 18 19 36 77 255))

(define unit-decimals-table #hasheq((wei . 0) (gwei . 9) (ether . 18)))

(define (unit-decimals u)
  (hash-ref unit-decimals-table u u))

(define syntax-rows
  '((" 1" 0) ("1 " 1) ("+1" 0) ("1e3" 1) ("1,000" 1) ("1_000" 1) ("0x10" 1) ("1.2.3" 3)
    ("--1" 1) ("1-" 1) ("1/2" 1) ("#e1.5" 0) ("Infinity" 0) ("+inf.0" 0) ("1\n" 1)
    ("١" 0) ("1.5 ether" 3) (".-1" 1)))

(define amount-pattern #px"^(-?)([0-9]*)(?:\\.([0-9]*))?$")

(define (digits->natural s)
  (for/fold ([n 0]) ([c (in-string s)])
    (+ (* n 10) (- (char->integer c) 48))))

(define (model-parts s)
  (match (regexp-match amount-pattern s)
    [(list _ sign int frac)
     #:when (positive? (+ (string-length int) (string-length (or frac ""))))
     (list sign int (or frac ""))]
    [_ #f]))

(define (model-value s)
  (match (model-parts s)
    [(list sign int frac)
     (define q (+ (digits->natural int)
                  (/ (digits->natural frac) (expt 10 (string-length frac)))))
     (if (string=? sign "-") (- q) q)]
    [#f #f]))

(define (model-precision-index s d)
  (match (model-parts s)
    [(list _ _ frac)
     (define frac-start (- (string-length s) (string-length frac)))
     (for/first ([i (in-range (+ frac-start d) (string-length s))]
                 #:unless (char=? (string-ref s i) #\0))
       i)]
    [#f #f]))

(define (canonical s)
  (match-define (list sign digits fraction) (model-parts s))
  (define int (string-trim digits "0" #:right? #f #:repeat? #t))
  (define frac (string-trim fraction "0" #:left? #f #:repeat? #t))
  (define nothing? (and (string=? int "") (string=? frac "")))
  (string-append (if (and (string=? sign "-") (not nothing?)) "-" "")
                 (if (string=? int "") "0" int)
                 (if (string=? frac "") "" (string-append "." frac))))


(define (random-digits prng n)
  (build-string n (lambda (_) (integer->char (+ 48 (random 10 prng))))))

(define (random-int-part prng fraction?)
  (if (and fraction? (zero? (random 10 prng)))
      ""
      (string-append (make-string (random 4 prng) #\0)
                     (random-digits prng (add1 (random 40 prng))))))

(define (random-sign prng)
  (if (zero? (random 4 prng)) "-" ""))

(define (random-amount-string prng d)
  (define point? (< (random 10 prng) 7))
  (define frac (if point?
                   (string-append (random-digits prng (random (add1 d) prng))
                                  (make-string (random 4 prng) #\0))
                   ""))
  (string-append (random-sign prng)
                 (random-int-part prng (positive? (string-length frac)))
                 (if point? "." "")
                 frac))

(define (random-imprecise-string prng d)
  (define extra (random-digits prng (add1 (random 5 prng))))
  (define nonzero (integer->char (+ 49 (random 9 prng))))
  (define at (random (string-length extra) prng))
  (string-append (random-sign prng)
                 (random-int-part prng #t)
                 "."
                 (random-digits prng d)
                 (substring extra 0 at)
                 (string nonzero)
                 (substring extra (add1 at))))

(define (random-wei prng d)
  (define one (expt 10 d))
  (define special
    (list 0 1 -1 (sub1 one) (- 1 one) one (- one) (add1 one) (- (add1 one))
          (sub1 (expt 2 256)) (- (expt 2 255)) (* one (random 1000 prng))))
  (if (zero? (random 2 prng))
      (pick prng special)
      (* (if (zero? (random 2 prng)) 1 -1)
         (modulo (random-natural prng 38) (expt 2 300)))))

(define garbage-chars
  (string->list "0123456789.-+eE,_x/# \n١"))

(define (garbage-string prng)
  (build-string (random 31 prng)
                (lambda (_)
                  (if (zero? (random 3 prng))
                      (integer->char (random #x3000 prng))
                      (pick prng garbage-chars)))))
