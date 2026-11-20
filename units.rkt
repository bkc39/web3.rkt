#lang racket/base

(require racket/contract/base
         "private/exn.rkt")

(define amount/c (or/c string? (and/c rational? exact?)))
(define unit/c (or/c 'wei 'gwei 'ether (integer-in 0 255)))

(provide (struct-out exn:fail:web3:units)
         (contract-out
          [units->wei (-> amount/c unit/c exact-integer?)]
          [ether->wei (-> amount/c exact-integer?)]
          [gwei->wei (-> amount/c exact-integer?)]
          [wei->string (->* (exact-integer?)
                            (unit/c #:min-fraction-digits (integer-in 0 255))
                            string?)]))

(define reason-phrases
  (hasheq 'syntax "not a decimal number"
          'precision "too many decimals"))

(define (raise-units-error who reason index detail)
  (raise (exn:fail:web3:units (format "~a: ~a: ~a" who (hash-ref reason-phrases reason) detail)
                              (current-continuation-marks)
                              index
                              reason)))

(define (unit-decimals unit)
  (case unit
    [(wei) 0]
    [(gwei) 9]
    [(ether) 18]
    [else unit]))

(define (unit-name unit)
  (if (symbol? unit) unit "the unit"))

(define (units->wei amount unit)
  (amount->wei 'units->wei amount unit))

(define (ether->wei amount)
  (amount->wei 'ether->wei amount 'ether))

(define (gwei->wei amount)
  (amount->wei 'gwei->wei amount 'gwei))

(define (amount->wei who amount unit)
  (if (string? amount)
      (decimal->wei who amount unit)
      (rational->wei who amount unit)))

(define (rational->wei who q unit)
  (define n (* q (expt 10 (unit-decimals unit))))
  (if (integer? n)
      n
      (raise-units-error who 'precision #f
                         (format "~a has ~a, but the amount needs more"
                                 (unit-name unit) (unit-decimals unit)))))

(define (digit? c)
  (char<=? #\0 c #\9))

(define (char-at s i)
  (and (< i (string-length s)) (string-ref s i)))

(define (digits-end s start)
  (let loop ([i start])
    (define c (char-at s i))
    (if (and c (digit? c)) (loop (add1 i)) i)))

(define (digits->natural s start end)
  (if (= start end) 0 (string->number (substring s start end) 10)))

(define (decimal->wei who s unit)
  (define d (unit-decimals unit))
  (define minus? (eqv? (char-at s 0) #\-))
  (define int-start (if minus? 1 0))
  (define int-end (digits-end s int-start))
  (define point? (eqv? (char-at s int-end) #\.))
  (define frac-start (if point? (add1 int-end) int-end))
  (define frac-end (digits-end s frac-start))
  (define unexpected (and (< frac-end (string-length s)) frac-end))
  (define kept (min (- frac-end frac-start) d))
  (define excess (for/first ([i (in-range (+ frac-start kept) frac-end)]
                             #:unless (char=? (string-ref s i) #\0))
                   i))
  (cond
    [unexpected
     (raise-units-error who 'syntax unexpected
                        (format "unexpected ~s at index ~a"
                                (substring s unexpected (add1 unexpected)) unexpected))]
    [(and (= int-start int-end) (= frac-start frac-end))
     (raise-units-error who 'syntax (string-length s) "no digits")]
    [excess
     (raise-units-error who 'precision excess
                        (format "~a has ~a, but decimal ~a is ~a"
                                (unit-name unit) d (- excess frac-start -1) (string-ref s excess)))]
    [else
     (define whole (digits->natural s int-start int-end))
     (define part (digits->natural s frac-start (+ frac-start kept)))
     (define m (+ (* whole (expt 10 d)) (* part (expt 10 (- d kept)))))
     (if minus? (- m) m)]))

(define (wei->string n [unit 'ether] #:min-fraction-digits [k 0])
  (define d (unit-decimals unit))
  (define-values (whole part) (quotient/remainder (abs n) (expt 10 d)))
  (define fraction (pad-right (fraction-digits part d) k))
  (string-append (if (negative? n) "-" "")
                 (number->string whole)
                 (if (string=? fraction "") "" (string-append "." fraction))))

(define (fraction-digits part d)
  (if (zero? part)
      ""
      (let* ([digits (number->string part)]
             [padded (string-append (make-string (- d (string-length digits)) #\0) digits)])
        (substring padded 0 (trimmed-end padded)))))

(define (trimmed-end s)
  (let loop ([end (string-length s)])
    (if (and (positive? end) (char=? (string-ref s (sub1 end)) #\0))
        (loop (sub1 end))
        end)))

(define (pad-right s k)
  (if (< (string-length s) k)
      (string-append s (make-string (- k (string-length s)) #\0))
      s))
