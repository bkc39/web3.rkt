#lang racket/base

;; Positions are absolute indices into the data.  Every region is checked
;; against the end of the data before anything of it is read or allocated,
;; and the tails of a tuple must follow its head and each other in order,
;; so each byte belongs to at most one value.

(require racket/contract/base
         (only-in racket/list make-list)
         (only-in racket/match match struct*)
         "../abi/type.rkt"
         (submod "../abi/type.rkt" private)
         (only-in "address.rkt" address-modulus bytes->address)
         "exn.rkt"
         "hex.rkt")

(provide abi-decode-reason/c
         decode-arguments
         (contract-out
          [raise-abi-decode-error
           (->* (symbol?
                 abi-decode-reason/c
                 (or/c abi-type? (listof abi-type?))
                 exact-nonnegative-integer?)
                ((or/c #f string?))
                none/c)]))

(define reason-phrases
  (hasheq 'short "data too short"
          'offset "offset out of range"
          'overlap "overlapping tail"
          'length "length out of range"
          'non-canonical "not canonically encoded"
          'utf-8 "invalid UTF-8"
          'selector "selector mismatch"
          'topic "topics do not match"))

(define abi-decode-reason/c
  (flat-named-contract 'abi-decode-reason/c (apply or/c (hash-keys reason-phrases #t))))

(define word-modulus (expt 2 256))
(define sign-bit (expt 2 255))

(define (raise-abi-decode-error who reason type offset [detail #f])
  (raise (exn:fail:web3:abi:decode
          (format "~a: ~a~a\n  type: ~a\n  offset: ~a"
                  who
                  (hash-ref reason-phrases reason)
                  (if detail (string-append ": " detail) "")
                  (type-label type)
                  offset)
          (current-continuation-marks)
          type
          offset
          reason)))

(define (word-at data i)
  (bytes->natural data i (+ i 32)))

(define (head-size ts)
  (for/sum ([t (in-list ts)]) (or (static-size t) 32)))

(define (region-size t)
  (match t
    [(struct* abi-tuple ([components cs])) (head-size cs)]
    [(struct* abi-fixed-array ([element e] [length k])) (* k (or (static-size e) 32))]
    [_ 32]))

(define (decode-arguments who ts data base)
  (define end (+ base (head-size ts)))
  (cond
    [(> end (bytes-length data))
     (raise-abi-decode-error who 'short ts base
                             (format "the head needs ~a bytes, ~a remain"
                                     (- end base) (max 0 (- (bytes-length data) base))))]
    [else (let-values ([(vs _) (decode-sequence who data ts base)]) vs)]))

(define (decode-sequence who data ts base)
  (for/fold ([vs '()]
             [cursor (+ base (head-size ts))]
             [slot base]
             #:result (values (reverse vs) cursor))
            ([t (in-list ts)])
    (define size (static-size t))
    (define offset (and (not size) (word-at data slot)))
    (define target (and offset (+ base offset)))
    (define-values (v extent)
      (cond
        [size (decode-at who data t slot)]
        [(> (+ target (region-size t)) (bytes-length data))
         (raise-abi-decode-error who 'offset t slot (format "~a from byte ~a" offset base))]
        [(< target cursor)
         (raise-abi-decode-error who 'overlap t slot
                                 (format "byte ~a is before byte ~a" target cursor))]
        [else (decode-at who data t target)]))
    (values (cons v vs) (if size cursor extent) (+ slot (or size 32)))))

(define (decode-at who data t at)
  (define word (word-at data at))
  (define body (+ at 32))
  (define remain (- (bytes-length data) body))
  (define (check-length unit)
    (when (> (* word unit) remain)
      (raise-abi-decode-error who 'length t at
                              (if (= unit 1)
                                  (format "~a bytes claimed, ~a remain" word remain)
                                  (format "~a elements of ~a bytes claimed, ~a bytes remain"
                                          word unit remain)))))
  (match t
    [(struct* abi-bytes ())
     (check-length 1)
     (values (subbytes data body (+ body word)) (+ body word))]
    [(struct* abi-string ())
     (check-length 1)
     (unless (bytes-utf-8-length data #f body (+ body word))
       (raise-abi-decode-error who 'utf-8 t body))
     (values (bytes->string/utf-8 data #f body (+ body word)) (+ body word))]
    [(struct* abi-array ([element e]))
     (check-length (or (static-size e) 32))
     (decode-sequence who data (make-list word e) body)]
    [(struct* abi-fixed-array ([element e] [length k])) (decode-sequence who data (make-list k e) at)]
    [(struct* abi-tuple ([components cs])) (decode-sequence who data cs at)]
    [_ (values (decode-word who data t at word) body)]))

(define (decode-word who data t at word)
  (define word-end (+ at 32))
  (define (non-canonical)
    (raise-abi-decode-error who 'non-canonical t at (bytes->hex (subbytes data at word-end))))
  (match t
    [(struct* abi-uint ([bits n]))
     (if (< word (expt 2 n)) word (non-canonical))]
    [(struct* abi-int ([bits n]))
     (define v (if (< word sign-bit) word (- word word-modulus)))
     (if (<= (- (expt 2 (sub1 n))) v (sub1 (expt 2 (sub1 n)))) v (non-canonical))]
    [(struct* abi-address ())
     (if (< word address-modulus)
         (bytes->address (subbytes data (+ at 12) word-end))
         (non-canonical))]
    [(struct* abi-bool ())
     (case word
       [(0) #f]
       [(1) #t]
       [else (non-canonical)])]
    [(struct* abi-fixed-bytes ([size n]))
     (if (zero? (bitwise-and word (sub1 (expt 2 (* 8 (- 32 n))))))
         (subbytes data at (+ at n))
         (non-canonical))]))
