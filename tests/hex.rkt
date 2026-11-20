#lang racket/base

(module+ test
  (require (only-in racket/list range)
           (only-in racket/match match-define)
           rackunit
           (only-in web3/private/hex bytes->natural natural->bytes)
           (only-in web3/rpc hex-string->number number->hex-string)
           (only-in "abi-gen.rkt" random-bytes)
           (only-in "helpers/accounts.rkt" random-natural)
           (only-in "util.rkt" check-blames-here seeded))

  (define (fold-bytes->natural bs start end)
    (for/fold ([n 0]) ([b (in-bytes bs start end)])
      (+ (* n 256) b)))

  (define (loop-natural->bytes n width)
    (apply bytes (for/list ([i (in-range (sub1 width) -1 -1)])
                   (bitwise-and (arithmetic-shift n (* -8 i)) 255))))

  (define samples
    (let ([prng (seeded 8 1 2 3 5 8)])
      (define sizes (append (range 301) (for/list ([_ (in-range 300)]) (random 301 prng))))
      (for/list ([size (in-list sizes)])
        (define bs (random-bytes prng size))
        (define start (random (add1 (bytes-length bs)) prng))
        (list bs start (+ start (random (add1 (- (bytes-length bs) start)) prng))))))

  (test-case "bytes->natural is the big-endian fold, over the whole and over sub-ranges"
    (for ([sample (in-list samples)])
      (match-define (list bs start end) sample)
      (check-equal? (bytes->natural bs) (fold-bytes->natural bs 0 (bytes-length bs))
                    (format "~s" bs))
      (check-equal? (bytes->natural bs start) (fold-bytes->natural bs start (bytes-length bs))
                    (format "~s ~a" bs start))
      (check-equal? (bytes->natural bs start end) (fold-bytes->natural bs start end)
                    (format "~s ~a ~a" bs start end))))

  (test-case "natural->bytes is the big-endian loop, padding or truncating to the width"
    (for ([sample (in-list samples)])
      (match-define (list bs _ _) sample)
      (define size (bytes-length bs))
      (define n (fold-bytes->natural bs 0 size))
      (for ([width (in-list (list 0 1 (quotient size 2) size (+ size 7)))])
        (check-equal? (natural->bytes n width) (loop-natural->bytes n width)
                      (format "~a ~a" n width)))))

  (test-case "X1: a quantity without 0x is refused, even when its digits are decimal"
    (check-blames-here (hex-string->number "10"))
    (check-blames-here (hex-string->number "16"))
    (check-blames-here (hex-string->number "0"))
    (check-blames-here (hex-string->number "123")))

  (define (hex-blame? e)
    (and (exn:fail:contract? e) (regexp-match? #rx"expected: hex-string\\?" (exn-message e))))

  (test-case "X2: malformed input blames the caller, expecting hex-string?"
    (for ([v (in-list '("0xzz" "0X10" "" " 0x10" "0x10 " "#x10" "1.5" 16 #f))])
      (check-blames-here (hex-string->number v))
      (check-exn hex-blame? (lambda () (hex-string->number v)) (format "~s" v)))
    (check-exn (lambda (e)
                 (and (exn:fail:contract:arity? e)
                      (regexp-match? #rx"^hex-string->number: arity mismatch" (exn-message e))))
               (lambda () (hex-string->number))))

  (test-case "X3: quantities read as the numbers number->hex-string writes"
    (for ([row (in-list '(("0x" 0) ("0x0" 0) ("0x10" 16) ("0xff" 255) ("0xFF" 255)
                          ("0xDeadBeef" 3735928559)))])
      (match-define (list s n) row)
      (check-equal? (hex-string->number s) n s))
    (check-equal? (hex-string->number (string-append "0x" (make-string 63 #\0) "7")) 7)
    (define prng (seeded 1 0 0 1 0 1))
    (for ([n (in-list (append (list 0 1 15 16 255 256 (sub1 (expt 2 64)) (sub1 (expt 2 256))
                                    (expt 2 256))
                              (for/list ([_ (in-range 200)])
                                (modulo (random-natural prng 38) (expt 2 300)))))])
      (check-equal? (hex-string->number (number->hex-string n)) n (number->string n)))))
