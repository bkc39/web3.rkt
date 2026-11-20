#lang racket/base

(require (only-in json read-json)
         (only-in racket/list make-list)
         (only-in racket/match match)
         racket/runtime-path
         (only-in racket/string string-join)
         rackunit
         (only-in web3/rlp exn:fail:web3:rlp-offset exn:fail:web3:rlp-reason exn:fail:web3:rlp?
                  rlp-decode)
         (only-in web3/rpc bytes->hex-string)
         (only-in "../abi-gen.rkt" random-bytes))

(provide boundary-items
         check-decode-fault
         decode-fault?
         deep-item
         input-name
         item->cast-argument
         item->cast-output
         mutations
         prefix-boundaries
         random-input
         random-rlp-item
         rlp-fault
         rlp-vector-path
         rlp-vectors)

(define-runtime-path vector-dir "../vectors/rlp")

(define (rlp-vector-path name)
  (build-path vector-dir name))

(define (rlp-vectors name)
  (call-with-input-file* (rlp-vector-path name) read-json))

(define (rlp-fault thunk)
  (with-handlers ([exn:fail:web3:rlp? values])
    (thunk)
    #f))

(define (input-name bs)
  (if (<= (bytes-length bs) 64)
      (bytes->hex-string bs)
      (format "~a... (~a bytes)" (bytes->hex-string (subbytes bs 0 32)) (bytes-length bs))))

(define (decode-fault? bs reason offset)
  (match (rlp-fault (lambda () (rlp-decode bs)))
    [#f #f]
    [e (and (eq? (exn:fail:web3:rlp-reason e) reason) (eqv? (exn:fail:web3:rlp-offset e) offset))]))

(define (check-decode-fault bs reason offset [message #f] #:name [name (input-name bs)])
  (define e (rlp-fault (lambda () (rlp-decode bs))))
  (check-pred exn:fail:web3:rlp? e name)
  (when e
    (check-equal? (exn:fail:web3:rlp-reason e) reason name)
    (check-equal? (exn:fail:web3:rlp-offset e) offset name)
    (when message
      (check-equal? (exn-message e) message name))))

(define (pick prng xs)
  (list-ref xs (random (length xs) prng)))

(define edge-bytes '(#x00 #x01 #x7f #x80 #x81 #xff))

(define edge-lengths '(0 1 1 1 2 54 55 56 57 255 256))

(define prefix-boundaries
  '(#x00 #x7f #x80 #x81 #xb7 #xb8 #xb9 #xbf #xc0 #xc1 #xf7 #xf8 #xf9 #xff))

(define (random-rlp-bytes prng)
  (define n (if (zero? (random 2 prng)) (pick prng edge-lengths) (random 81 prng)))
  (if (and (= n 1) (zero? (random 2 prng)))
      (bytes (pick prng edge-bytes))
      (random-bytes prng n)))

(define (random-rlp-item prng #:depth [depth 4] #:width [width 6])
  (if (or (zero? depth) (< (random 10 prng) 6))
      (random-rlp-bytes prng)
      (for/list ([_ (in-range (random (add1 width) prng))])
        (random-rlp-item prng #:depth (sub1 depth) #:width width))))

(define boundary-items
  (append (list #"")
          (map bytes edge-bytes)
          (for/list ([n (in-list '(2 55 56 57 255 256 1024 65535 65536))])
            (make-bytes n #x61))
          (for/list ([n (in-list '(0 1 55 56 57 255 256 65536))])
            (make-list n #"\1"))
          (list '(()) '(() (()) (() (()))))))

(define (deep-item n)
  (for/fold ([item '()]) ([_ (in-range n)])
    (list item)))

(define (random-input prng)
  (define n (random 65 prng))
  (define bs (random-bytes prng n))
  (when (and (positive? n) (< (random 4 prng) 3))
    (bytes-set! bs 0 (min 255 (max 0 (+ (pick prng prefix-boundaries) (random 3 prng) -1)))))
  bs)

(define (splice bs at removed inserted)
  (bytes-append (subbytes bs 0 at) inserted (subbytes bs (+ at removed))))

(define (mutations prng bs)
  (define n (bytes-length bs))
  (define (somewhere) (random n prng))
  (append
   (if (zero? n)
       '()
       (list (let ([at (somewhere)])
               (splice bs at 1 (bytes (bitwise-xor (bytes-ref bs at) (add1 (random 255 prng))))))
             (splice bs (somewhere) 1 #"")
             (subbytes bs 0 (somewhere))
             (splice bs 0 1 (bytes (random 256 prng)))))
   (list (splice bs (random (add1 n) prng) 0 (bytes (random 256 prng)))
         (bytes-append bs (random-bytes prng (add1 (random 8 prng)))))))

(define (item->cast-argument item)
  (match item
    [(? bytes?) (bytes->hex-string item)]
    [_ (list->cast item)]))

(define (item->cast-output item)
  (match item
    [(? bytes?) (format "\"~a\"" (bytes->hex-string item))]
    [_ (list->cast item)]))

(define (list->cast items)
  (format "[~a]" (string-join (map item->cast-output items) ",")))
