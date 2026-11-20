#lang racket/base

(module+ test
  (require racket/match
           (only-in racket/string string-contains?)
           rackunit
           web3/units
           "cast.rkt"
           "helpers/units.rkt"
           (only-in "util.rkt" seeded))

  (define cast-units '(wei gwei ether 0 1 6 8 18 36 77))

  (define bound (expt 2 255))

  (define (in-range? n)
    (< (abs n) bound))

  (define (cast-read s u)
    (if (symbol? u)
        (run-cast "to-wei" "--" s (symbol->string u))
        (run-cast "parse-units" "--" s (number->string u))))

  (define (cast-sample make prng u n)
    (define d (unit-decimals u))
    (let loop ([found '()] [tries 0])
      (cond
        [(or (= (length found) n) (= tries 100000)) found]
        [else
         (define s (make prng d))
         (loop (if (in-range? (truncate (* (model-value s) (expt 10 d)))) (cons s found) found)
               (add1 tries))])))

  (define syntax-inputs
    (for*/list ([row (in-list syntax-rows)]
                [s (in-value (match row [(list s _) s]))]
                #:unless (string-contains? s "_"))
      s))

  (cond
    [(not cast-path) (displayln "SKIPPED: units differential: cast not found")]
    [else
     (test-case "C1: cast to-wei and parse-units read every amount as units->wei does"
       (define prng (seeded 7 3 1 5 2 9))
       (for* ([u (in-list cast-units)]
              [s (in-list (cast-sample random-amount-string prng u 60))])
         (check-equal? (cast-read s u) (number->string (units->wei s u)) (format "~s ~a" s u))))

     (test-case "C2: cast truncates the digits we refuse"
       (define prng (seeded 9 2 6 5 3 5))
       (for* ([u (in-list cast-units)]
              [s (in-list (cast-sample random-imprecise-string prng u 10))])
         (define name (format "~s ~a" s u))
         (define e (with-handlers ([exn:fail:web3:units? values]) (units->wei s u)))
         (check-pred exn:fail:web3:units? e name)
         (check-eq? (and (exn:fail:web3:units? e) (exn:fail:web3:units-reason e)) 'precision name)
         (check-equal? (cast-read s u)
                       (number->string (truncate (* (model-value s) (expt 10 (unit-decimals u)))))
                       name))
       (check-equal? (cast-read "-1.0000000000000000019" 'ether) "-1000000000000000001")
       (check-equal? (cast-read "1.9999999" 6) "1999999"))

     (test-case "C3: cast refuses the strings we refuse, except those without digits"
       (for ([s (in-list syntax-inputs)])
         (define e (with-handlers ([exn:fail:web3:units? values]) (units->wei s 'ether)))
         (check-eq? (and (exn:fail:web3:units? e) (exn:fail:web3:units-reason e)) 'syntax
                    (format "~s" s))
         (match (cast-outcome "to-wei" "--" s "ether")
           [(list 'failed stderr) (check-regexp-match #rx"invalid digit" stderr (format "~s" s))]
           [other (fail (format "cast accepted ~s: ~s" s other))]))
       (for ([s (in-list '("" "." "-" "-."))])
         (check-equal? (cast-read s 'ether) "0" (format "~s" s))))

     (define (wei-samples prng unit-list per-unit)
       (for*/list ([u (in-list unit-list)]
                   [_ (in-range per-unit)]
                   [n (in-value (random-wei prng (unit-decimals u)))]
                   #:when (in-range? n))
         (list n u)))

     (test-case "C4: cast from-wei writes every digit of the unit's decimals"
       (for ([sample (in-list (wei-samples (seeded 5 8 2 4 6 1) '(wei gwei ether) 40))])
         (match-define (list n u) sample)
         (check-equal? (run-cast "from-wei" "--" (number->string n) (symbol->string u))
                       (wei->string n u #:min-fraction-digits (max (unit-decimals u) 1))
                       (format "~a ~a" n u))))

     (test-case "C5: cast format-units and to-unit drop an integral fraction, else write it all"
       (for ([sample (in-list (wei-samples (seeded 6 2 8 3 1 8) cast-units 20))])
         (match-define (list n u) sample)
         (define d (unit-decimals u))
         (define k (if (zero? (remainder n (expt 10 d))) 0 d))
         (define expected (wei->string n d #:min-fraction-digits k))
         (unless (and (negative? n) (= d 77))
           (check-equal? (run-cast "format-units" "--" (number->string n) (number->string d))
                         expected
                         (format "format-units ~a ~a" n d)))
         (when (and (symbol? u) (>= n 0))
           (check-equal? (run-cast "to-unit" (number->string n) (symbol->string u))
                         expected
                         (format "to-unit ~a ~a" n u)))))

     (test-case "C5: divergences: cast drops a digit of a negative amount with 77 decimals"
       (check-equal? (run-cast "format-units" "--" "-1" "77")
                     (string-append "-0." (make-string 75 #\0) "1"))
       (check-equal? (wei->string -1 77) (string-append "-0." (make-string 76 #\0) "1"))
       (check-match (cast-outcome "to-unit" "--" "-5" "gwei") (list 'failed _)))]))
