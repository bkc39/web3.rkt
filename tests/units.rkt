#lang racket/base

(module+ test
  (require racket/match
           rackunit
           (only-in web3/rpc exn:fail:web3?)
           web3/units
           "bounded.rkt"
           "helpers/units.rkt"
           "modules.rkt"
           (only-in "util.rkt" check-blames-here seeded))

  (define (units-fault thunk)
    (with-handlers ([exn:fail:web3:units? values])
      (thunk)
      #f))

  (define (outcome thunk)
    (with-handlers ([exn:fail:web3:units?
                     (lambda (e)
                       (list 'raised (exn:fail:web3:units-reason e) (exn:fail:web3:units-index e)
                             (exn-message e)))])
      (list 'ok (thunk))))

  (define car-of (match-lambda [(cons x _) x]))

  (define second-of (match-lambda [(list _ x) x]))

  (define (unit-name u)
    (if (symbol? u) (symbol->string u) "the unit"))

  (define (syntax-message who c i)
    (format "~a: not a decimal number: unexpected ~s at index ~a" who (string c) i))

  (define (precision-message who u j digit)
    (format "~a: too many decimals: ~a has ~a, but decimal ~a is ~a"
            who (unit-name u) (unit-decimals u) j digit))

  (define (check-fault thunk reason index message name)
    (define e (units-fault thunk))
    (check-pred exn:fail:web3:units? e name)
    (when e
      (check-equal? (exn:fail:web3:units-reason e) reason name)
      (check-equal? (exn:fail:web3:units-index e) index name)
      (check-equal? (exn-message e) message name)))

  (define (sample-strings make seed n)
    (define prng (apply seeded seed))
    (for*/list ([u (in-list units)]
                [_ (in-range n)])
      (list u (make prng (unit-decimals u)))))

  (define precise (sample-strings random-amount-string '(3 1 4 1 5 9) 300))
  (define imprecise (sample-strings random-imprecise-string '(2 7 1 8 2 8) 300))

  (define pinned
    `(("1.5" ether 1500000000000000000)
      ("30" gwei 30000000000)
      (".5" ether 500000000000000000)
      ("5." ether 5000000000000000000)
      ("001.500" gwei 1500000000)
      ("-1" ether -1000000000000000000)
      ("-.5" 6 -500000)
      ("-0" ether 0)
      ("1.500000000" 6 1500000)
      ("0.25" gwei 250000000)
      ("1234.5" 6 1234500000)
      ("1" 77 ,(expt 10 77))
      ("0.000000000000000001" ether 1)
      ("1.000000000000000001" ether 1000000000000000001)
      ("9007199254740993" wei 9007199254740993)
      ("1.50000000000000000000" ether 1500000000000000000)))

  (test-case "P1: a decimal string is its exact value times 10^d"
    (for ([sample (in-list precise)])
      (match-define (list u s) sample)
      (check-equal? (units->wei s u) (* (model-value s) (expt 10 (unit-decimals u)))
                    (format "~s ~a" s u)))
    (for ([sample (in-list imprecise)])
      (match-define (list u s) sample)
      (define e (units-fault (lambda () (units->wei s u))))
      (check-pred exn:fail:web3:units? e (format "~s ~a" s u))
      (when e
        (check-equal? (exn:fail:web3:units-reason e) 'precision (format "~s ~a" s u))
        (check-equal? (exn:fail:web3:units-index e)
                      (model-precision-index s (unit-decimals u))
                      (format "~s ~a" s u)))))

  (test-case "P2: pinned amounts"
    (for ([row (in-list pinned)])
      (match-define (list s u n) row)
      (check-equal? (units->wei s u) n (format "~s ~a" s u))))

  (test-case "P3: exact rationals"
    (check-equal? (units->wei 3/2 'ether) 1500000000000000000)
    (check-equal? (units->wei 30 'gwei) 30000000000)
    (check-equal? (units->wei -1/4 'gwei) -250000000)
    (check-equal? (units->wei (/ 1 (expt 10 255)) 255) 1)
    (for ([row (in-list `((1/3 gwei) (1/2 wei) (,(/ 1 (expt 10 19)) ether)))])
      (match-define (list q u) row)
      (check-fault (lambda () (units->wei q u)) 'precision #f
                   (format "units->wei: too many decimals: ~a has ~a, but the amount needs more"
                           (unit-name u) (unit-decimals u))
                   (format "~a ~a" q u))))

  (define (normalized o)
    (match o
      [(list 'raised reason index message)
       (list 'raised reason index
             (regexp-replace #px"(?:the unit|gwei|wei|ether) has"
                             (regexp-replace #px"^[a-z]+->wei: " message "")
                             "UNIT has"))]
      [_ o]))

  (test-case "P4: ether->wei and gwei->wei are units->wei with 'ether and 'gwei, or 18 and 9"
    (for ([s (in-list (append (map second-of precise)
                              (map second-of imprecise)
                              (map car-of pinned)))])
      (check-equal? (normalized (outcome (lambda () (ether->wei s))))
                    (normalized (outcome (lambda () (units->wei s 'ether))))
                    s)
      (check-equal? (normalized (outcome (lambda () (ether->wei s))))
                    (normalized (outcome (lambda () (units->wei s 18))))
                    s)
      (check-equal? (normalized (outcome (lambda () (gwei->wei s))))
                    (normalized (outcome (lambda () (units->wei s 'gwei))))
                    s)
      (check-equal? (normalized (outcome (lambda () (gwei->wei s))))
                    (normalized (outcome (lambda () (units->wei s 9))))
                    s)))

  (define syntax-rows
    '((" 1" 0) ("1 " 1) ("+1" 0) ("1e3" 1) ("1,000" 1) ("1_000" 1) ("0x10" 1) ("1.2.3" 3)
      ("--1" 1) ("1-" 1) ("1/2" 1) ("#e1.5" 0) ("Infinity" 0) ("+inf.0" 0) ("1\n" 1)
      ("١" 0) ("1.5 ether" 3) (".-1" 1)))

  (define no-digit-rows '(("" 0) ("." 1) ("-" 1) ("-." 2)))

  (define precision-rows
    '(("0.0000000000000000001" ether 20 19 1)
      ("1.0000000000000000009" ether 20 19 9)
      ("0.00000000000000000010" ether 20 19 1)
      ("-0.0000000000000000001" ether 21 19 1)
      ("1.0000000005" gwei 11 10 5)
      ("1.0000001000" 6 8 7 1)
      ("1.5" wei 2 1 5)
      ("1.5" 0 2 1 5)))

  (test-case "S1: a character outside the grammar is refused where it stands"
    (for* ([row (in-list syntax-rows)]
           [u (in-list units)])
      (match-define (list s i) row)
      (check-fault (lambda () (units->wei s u)) 'syntax i
                   (syntax-message 'units->wei (string-ref s i) i)
                   (format "~s ~a" s u))))

  (test-case "S2: a string without digits is refused at its end"
    (for* ([row (in-list no-digit-rows)]
           [u (in-list units)])
      (match-define (list s i) row)
      (check-fault (lambda () (units->wei s u)) 'syntax i
                   "units->wei: not a decimal number: no digits"
                   (format "~s ~a" s u))))

  (test-case "S3: a nonzero digit past the unit's decimals is refused at the first one"
    (for ([row (in-list precision-rows)])
      (match-define (list s u i j digit) row)
      (check-fault (lambda () (units->wei s u)) 'precision i
                   (precision-message 'units->wei u j digit)
                   (format "~s ~a" s u))))

  (test-case "S4: syntax is checked over the whole string before precision"
    (check-fault (lambda () (units->wei "0.0000000000000000001," 'ether)) 'syntax 21
                 (syntax-message 'units->wei #\, 21)
                 "S4"))

  (define formatted
    `(((1500000000000000000) "1.5")
      ((0) "0")
      ((0 ether 2) "0.00")
      ((1 gwei) "0.000000001")
      ((30000000000 gwei) "30")
      ((1500000 6) "1.5")
      ((-630000000000000) "-0.00063")
      ((-1) "-0.000000000000000001")
      ((5 wei) "5")
      ((5 wei 1) "5.0")
      ((,(expt 10 18) ether 18) "1.000000000000000000")
      ((1234567 6 2) "1.234567")
      ((1500000 6 9) "1.500000000")
      ((1000000000000000001) "1.000000000000000001")
      ((,(sub1 (expt 2 256)) ether 0)
       "115792089237316195423570985008687907853269984665640564039457.584007913129639935")
      ((123 0) "123")
      ((1 255) ,(string-append "0." (make-string 254 #\0) "1"))))

  (define (format-wei args)
    (match args
      [(list n) (wei->string n)]
      [(list n u) (wei->string n u)]
      [(list n u k) (wei->string n u #:min-fraction-digits k)]))

  (test-case "F1: pinned formatting"
    (for ([row (in-list formatted)])
      (match-define (list args expected) row)
      (check-equal? (format-wei args) expected (format "~s" args))))

  (define wei-samples
    (let ([prng (seeded 1 6 1 8 0 3)])
      (for*/list ([u (in-list units)]
                  [_ (in-range 500)])
        (define d (unit-decimals u))
        (list (random-wei prng d) u (min 255 (list-ref (list 0 1 d (+ d 3)) (random 4 prng)))))))

  (define (trailing-zeros n)
    (if (and (positive? n) (zero? (remainder n 10))) (add1 (trailing-zeros (quotient n 10))) 0))

  (define (fraction-length s)
    (match (regexp-match #rx"\\.([0-9]*)$" s)
      [(list _ digits) (string-length digits)]
      [#f 0]))

  (test-case "F2: the output is a canonical decimal with every digit it needs, padded to k"
    (for ([sample (in-list wei-samples)])
      (match-define (list n u k) sample)
      (define d (unit-decimals u))
      (define s (wei->string n u #:min-fraction-digits k))
      (define r (remainder (abs n) (expt 10 d)))
      (define name (format "~a ~a ~a" n u k))
      (check-regexp-match #px"^-?(0|[1-9][0-9]*)(\\.[0-9]+)?$" s name)
      (check-false (and (zero? n) (regexp-match? #rx"^-" s)) name)
      (check-equal? (fraction-length s) (max k (if (zero? r) 0 (- d (trailing-zeros r)))) name)))

  (test-case "F3: the output reads as n / 10^d"
    (for ([sample (in-list wei-samples)])
      (match-define (list n u k) sample)
      (check-equal? (model-value (wei->string n u #:min-fraction-digits k))
                    (/ n (expt 10 (unit-decimals u)))
                    (format "~a ~a ~a" n u k))))

  (test-case "I1: units->wei reads back what wei->string writes"
    (for ([sample (in-list wei-samples)])
      (match-define (list n u k) sample)
      (check-equal? (units->wei (wei->string n u #:min-fraction-digits k) u) n
                    (format "~a ~a ~a" n u k))))

  (test-case "I2: wei->string writes back the canonical form of what units->wei reads"
    (for ([sample (in-list precise)])
      (match-define (list u s) sample)
      (check-equal? (wei->string (units->wei s u) u) (canonical s) (format "~s ~a" s u))))

  (test-case "I3: a string and its exact value read the same"
    (for ([sample (in-list precise)])
      (match-define (list u s) sample)
      (check-equal? (units->wei (model-value s) u) (units->wei s u) (format "~s ~a" s u)))
    (for ([sample (in-list imprecise)])
      (match-define (list u s) sample)
      (define name (format "~s ~a" s u))
      (match-define (list 'raised reason-s _ _) (outcome (lambda () (units->wei s u))))
      (match-define (list 'raised reason-q _ _) (outcome (lambda () (units->wei (model-value s) u))))
      (check-eq? reason-s 'precision name)
      (check-eq? reason-q 'precision name)))

  (test-case "B1: arguments outside the contracts blame the caller"
    (check-blames-here (ether->wei 1.5))
    (check-blames-here (ether->wei 1.0))
    (check-blames-here (ether->wei +nan.0))
    (check-blames-here (ether->wei 1+2i))
    (check-blames-here (ether->wei #"1"))
    (check-blames-here (ether->wei 'one))
    (check-blames-here (gwei->wei +inf.0))
    (check-blames-here (units->wei "1" 'finney))
    (check-blames-here (units->wei "1" 'Ether))
    (check-blames-here (units->wei "1" 256))
    (check-blames-here (units->wei "1" -1))
    (check-blames-here (units->wei "1" 6.0))
    (check-blames-here (units->wei "1" "ether"))
    (check-blames-here (wei->string 1.5))
    (check-blames-here (wei->string 3/2))
    (check-blames-here (wei->string "15"))
    (check-blames-here (wei->string 1 'eth))
    (check-blames-here (wei->string 1 'ether #:min-fraction-digits 256))
    (check-blames-here (wei->string 1 'ether #:min-fraction-digits -1)))

  (define (arity-error? name)
    (lambda (e)
      (and (exn:fail:contract:arity? e)
           (regexp-match? (pregexp (format "^~a: arity mismatch" (regexp-quote name)))
                          (exn-message e)))))

  (define (names? name)
    (lambda (e)
      (and (exn:fail:contract? e) (regexp-match? (regexp-quote name) (exn-message e)))))

  (test-case "B1: a wrong arity or an unknown keyword is a contract error naming the procedure"
    (check-exn (arity-error? "ether->wei") (lambda () (ether->wei)))
    (check-exn (arity-error? "units->wei") (lambda () (units->wei "1")))
    (check-exn (arity-error? "wei->string") (lambda () (wei->string)))
    (check-exn (arity-error? "wei->string") (lambda () (wei->string 1 'ether 2)))
    (check-exn (names? "ether->wei") (lambda () (ether->wei "1" #:unit 'gwei)))
    (check-exn (names? "wei->string") (lambda () (wei->string 1 #:digits 2))))

  (define fuzz-corpus
    (let ([prng (seeded 4 4 2 1 9 7)])
      (append (map car-of syntax-rows)
              (map car-of no-digit-rows)
              (map car-of precision-rows)
              '("0.0000000000000000001,")
              (for/list ([_ (in-range 5000)]) (garbage-string prng))
              (for/list ([_ (in-range 2000)])
                (random-amount-string prng (unit-decimals (list-ref units (random 14 prng))))))))

  (test-case "B2, B3: any string reads as an integer or raises a units error with a short message"
    (for ([s (in-list fuzz-corpus)])
      (define name (format "~s" s))
      (define results
        (call-bounded (lambda ()
                        (for/list ([u (in-list units)])
                          (outcome (lambda () (units->wei s u)))))))
      (match results
        [(list 'ok outcomes)
         (for ([o (in-list outcomes)])
           (match o
             [(list 'ok n) (check-pred exact-integer? n name)]
             [(list 'raised reason index message)
              (check-not-false (memq reason '(syntax precision)) name)
              (check-true (and (exact-integer? index) (<= index (string-length s))) name)
              (check-regexp-match
               #px"^(units|ether|gwei)->wei: (not a decimal number|too many decimals): "
               message)
              (check-true (< (string-length message) 200) name)]))]
        [other (fail (format "~a: ~e" name other))])))

  (test-case "B3: units errors are web3 errors"
    (check-pred exn:fail:web3? (units-fault (lambda () (ether->wei ""))))
    (check-pred exn:fail:web3? (units-fault (lambda () (ether->wei 1/3)))))

  (test-case "B4: the pinned messages"
    (for ([row (in-list
                `((,(lambda () (ether->wei "")) "ether->wei: not a decimal number: no digits")
                  (,(lambda () (ether->wei "1,5"))
                   "ether->wei: not a decimal number: unexpected \",\" at index 1")
                  (,(lambda () (ether->wei "0.0000000000000000001"))
                   "ether->wei: too many decimals: ether has 18, but decimal 19 is 1")
                  (,(lambda () (gwei->wei "1.0000000005"))
                   "gwei->wei: too many decimals: gwei has 9, but decimal 10 is 5")
                  (,(lambda () (units->wei "1.0000005" 6))
                   "units->wei: too many decimals: the unit has 6, but decimal 7 is 5")
                  (,(lambda () (units->wei "1.5" 'wei))
                   "units->wei: too many decimals: wei has 0, but decimal 1 is 5")
                  (,(lambda () (gwei->wei 1/3))
                   "gwei->wei: too many decimals: gwei has 9, but the amount needs more")))])
      (match-define (list thunk message) row)
      (define e (units-fault thunk))
      (check-equal? (and e (exn-message e)) message)))

  (test-case "H1: long inputs and outputs finish in bounded time and memory"
    (define digits (make-string 100000 #\7))
    (match (call-bounded (lambda () (ether->wei digits)))
      [(list 'ok n) (check-true (= n (* (string->number digits) (expt 10 18))))]
      [other (fail (format "~.200e" other))])
    (define tiny (string-append "0." (make-string 100000 #\0) "1"))
    (match (call-bounded (lambda () (units-fault (lambda () (ether->wei tiny)))))
      [(list 'ok (? exn:fail:web3:units? e))
       (check-equal? (exn:fail:web3:units-reason e) 'precision)
       (check-equal? (exn:fail:web3:units-index e) 100002)]
      [other (fail (format "~e" other))])
    (match (call-bounded (lambda () (wei->string (- (expt 10 100000)) 255)))
      [(list 'ok s)
       (check-true (equal? s (string-append "-1" (make-string (- 100000 255) #\0))))]
      [other (fail (format "~.200e" other))]))

  (test-case "E1: web3/units needs no network, JSON, hashing or signing modules"
    (define names (filter values (map module-name (import-closure 'web3/units))))
    (check-equal? (filter (lambda (n) (member (collection n) '("net" "json" "keccak" "libsecp256k1")))
                          names)
                  '())
    (check-equal? (sort (filter (lambda (n) (equal? (collection n) "web3")) names) string<?)
                  '("web3/private/exn" "web3/units"))))
