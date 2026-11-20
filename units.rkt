#lang racket/base

(require racket/contract/base
         "private/exn.rkt")

(define unit-decimals-table
  (hasheq 'wei 0 'gwei 9 'ether 18))

(define amount/c (or/c string? (and/c rational? exact?)))
(define unit/c
  (apply or/c (append (sort (hash-keys unit-decimals-table) <
                            #:key (lambda (name) (hash-ref unit-decimals-table name)))
                      (list (integer-in 0 255)))))

(provide (struct-out exn:fail:web3:units)
         (contract-out
          [units->wei (-> amount/c unit/c exact-integer?)]
          [ether->wei (-> amount/c exact-integer?)]
          [gwei->wei (-> amount/c exact-integer?)]
          [wei->string (->* (exact-integer?)
                            (unit/c #:min-fraction-digits (integer-in 0 255))
                            string?)]))

(module* internal #f
  (provide units-reason/c))

(define reason-phrases
  (hasheq 'syntax "not a decimal number"
          'precision "too many decimals"))

(define units-reason/c
  (flat-named-contract 'units-reason/c (apply or/c (hash-keys reason-phrases #t))))

(define (raise-units-error who reason index detail)
  (raise (exn:fail:web3:units (format "~a: ~a: ~a" who (hash-ref reason-phrases reason) detail)
                              (current-continuation-marks)
                              index
                              reason)))

(define (unit-decimals unit)
  (hash-ref unit-decimals-table unit unit))

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

(define (rational->wei who amount unit)
  (define wei (* amount (expt 10 (unit-decimals unit))))
  (if (integer? wei)
      wei
      (raise-units-error who 'precision #f
                         (format "~a has ~a, but the amount needs more"
                                 (unit-name unit) (unit-decimals unit)))))

(define (digit? char)
  (char<=? #\0 char #\9))

(define (char-at text index)
  (and (< index (string-length text)) (string-ref text index)))

(define (digits-end text start)
  (let loop ([index start])
    (define char (char-at text index))
    (if (and char (digit? char)) (loop (add1 index)) index)))

(define (digits->natural text start end)
  (if (= start end) 0 (string->number (substring text start end) 10)))

(define (nonzero-index text start end)
  (for/first ([index (in-range start end)]
              #:unless (char=? (string-ref text index) #\0))
    index))

(define (decimal-bounds text)
  (define int-start (if (eqv? (char-at text 0) #\-) 1 0))
  (define int-end (digits-end text int-start))
  (define frac-start (if (eqv? (char-at text int-end) #\.) (add1 int-end) int-end))
  (values int-start int-end frac-start (digits-end text frac-start)))

(define (decimal->wei who text unit)
  (define decimals (unit-decimals unit))
  (define-values (int-start int-end frac-start frac-end) (decimal-bounds text))
  (define kept (min (- frac-end frac-start) decimals))
  (define excess (nonzero-index text (+ frac-start kept) frac-end))
  (cond
    [(< frac-end (string-length text))
     (raise-units-error who 'syntax frac-end
                        (format "unexpected ~s at index ~a"
                                (substring text frac-end (add1 frac-end)) frac-end))]
    [(and (= int-start int-end) (= frac-start frac-end))
     (raise-units-error who 'syntax (string-length text) "no digits")]
    [excess
     (raise-units-error who 'precision excess
                        (format "~a has ~a, but decimal ~a is ~a"
                                (unit-name unit) decimals (add1 (- excess frac-start))
                                (string-ref text excess)))]
    [else
     (define whole (digits->natural text int-start int-end))
     (define part (digits->natural text frac-start (+ frac-start kept)))
     (define wei (+ (* whole (expt 10 decimals)) (* part (expt 10 (- decimals kept)))))
     (if (zero? int-start) wei (- wei))]))

(define (wei->string amount [unit 'ether] #:min-fraction-digits [min-digits 0])
  (define decimals (unit-decimals unit))
  (define-values (whole part) (quotient/remainder (abs amount) (expt 10 decimals)))
  (define fraction (pad-right (fraction-digits part decimals) min-digits))
  (string-append (if (negative? amount) "-" "")
                 (number->string whole)
                 (if (string=? fraction "") "" (string-append "." fraction))))

(define (fraction-digits part decimals)
  (if (zero? part)
      ""
      (let* ([digits (number->string part)]
             [padded (string-append (make-string (- decimals (string-length digits)) #\0)
                                    digits)])
        (substring padded 0 (trimmed-end padded)))))

(define (trimmed-end text)
  (let loop ([end (string-length text)])
    (if (and (positive? end) (char=? (string-ref text (sub1 end)) #\0))
        (loop (sub1 end))
        end)))

(define (pad-right text width)
  (if (< (string-length text) width)
      (string-append text (make-string (- width (string-length text)) #\0))
      text))
