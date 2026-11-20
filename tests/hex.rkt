#lang racket/base

(module+ test
  (require (only-in racket/list range)
           (only-in racket/match match-define)
           rackunit
           (only-in web3/private/hex bytes->natural natural->bytes)
           (only-in "abi-gen.rkt" random-bytes)
           (only-in "util.rkt" seeded))

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
                      (format "~a ~a" n width))))))
