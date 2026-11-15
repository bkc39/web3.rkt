#lang racket/base

;; The decoder core.  web3/abi puts the contracts on its callers.
;;
;; Positions are absolute indices into the data.  Every region is checked
;; against the end of the data before anything of it is read or allocated,
;; and the tails of a tuple must follow its head and each other in order,
;; so each byte belongs to at most one value.

(require racket/contract/base
         (only-in racket/list make-list)
         (only-in racket/match match struct*)
         "../abi/type.rkt"
         (submod "../abi/type.rkt" private)
         "../address.rkt"
         "exn.rkt"
         "hex.rkt")

(provide abi-decode-reason/c
         decode-arguments
         raise-abi-decode-error)

(define reason-phrases
  '((short . "data too short")
    (offset . "offset out of range")
    (overlap . "overlapping tail")
    (length . "length out of range")
    (non-canonical . "not canonically encoded")
    (utf-8 . "invalid UTF-8")
    (selector . "selector mismatch")))

(define abi-decode-reason/c
  (flat-named-contract 'abi-decode-reason/c (lambda (v) (and (assq v reason-phrases) #t))))

(define (raise-abi-decode-error who reason type offset [detail #f])
  (raise (exn:fail:web3:abi:decode
          (format "~a: ~a~a\n  type: ~a\n  offset: ~a"
                  who
                  (cdr (assq reason reason-phrases))
                  (if detail (string-append ": " detail) "")
                  (type-label type)
                  offset)
          (current-continuation-marks)
          type
          offset
          reason)))

(define (word-at data i)
  (for/fold ([n 0]) ([b (in-bytes data i (+ i 32))])
    (+ (* n 256) b)))

(define (head-size ts)
  (for/sum ([t (in-list ts)]) (or (static-size t) 32)))

;; The bytes a dynamic value needs at its offset before it can be read.
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
    [else
     (define-values (vs _extent) (decode-sequence who data ts base))
     vs]))

;; The values of ts encoded as a tuple whose head starts at base, and the
;; end of the last byte read.
(define (decode-sequence who data ts base)
  (define end (bytes-length data))
  (for/fold ([vs '()]
             [cursor (+ base (head-size ts))]
             [slot base]
             #:result (values (reverse vs) cursor))
            ([t (in-list ts)])
    (define size (static-size t))
    (cond
      [size
       (define-values (v _extent) (decode-at who data t slot))
       (values (cons v vs) cursor (+ slot size))]
      [else
       (define offset (word-at data slot))
       (define target (+ base offset))
       (cond
         [(> (+ target (region-size t)) end)
          (raise-abi-decode-error who 'offset t slot (format "~a from byte ~a" offset base))]
         [(< target cursor)
          (raise-abi-decode-error who 'overlap t slot
                                  (format "byte ~a is before byte ~a" target cursor))]
         [else
          (define-values (v extent) (decode-at who data t target))
          (values (cons v vs) extent (+ slot 32))])])))

;; The value of t at, whose region fits, and the end of the last byte read.
(define (decode-at who data t at)
  (define remain (- (bytes-length data) (+ at 32)))
  (define (check-length n unit)
    (when (> (* n unit) remain)
      (raise-abi-decode-error who 'length t at
                              (if (= unit 1)
                                  (format "~a bytes claimed, ~a remain" n remain)
                                  (format "~a elements of ~a bytes claimed, ~a bytes remain"
                                          n unit remain)))))
  (match t
    [(struct* abi-bytes ())
     (define n (word-at data at))
     (check-length n 1)
     (values (subbytes data (+ at 32) (+ at 32 n)) (+ at 32 n))]
    [(struct* abi-string ())
     (define n (word-at data at))
     (check-length n 1)
     (unless (bytes-utf-8-length data #f (+ at 32) (+ at 32 n))
       (raise-abi-decode-error who 'utf-8 t (+ at 32)))
     (values (bytes->string/utf-8 data #f (+ at 32) (+ at 32 n)) (+ at 32 n))]
    [(struct* abi-array ([element e]))
     (define n (word-at data at))
     (check-length n (or (static-size e) 32))
     (decode-sequence who data (make-list n e) (+ at 32))]
    [(struct* abi-fixed-array ([element e] [length k])) (decode-sequence who data (make-list k e) at)]
    [(struct* abi-tuple ([components cs])) (decode-sequence who data cs at)]
    [_ (values (decode-word who data t at) (+ at 32))]))

(define (decode-word who data t at)
  (define w (word-at data at))
  (define (non-canonical)
    (raise-abi-decode-error who 'non-canonical t at (bytes->hex (subbytes data at (+ at 32)))))
  (match t
    [(struct* abi-uint ([bits n]))
     (if (< w (expt 2 n)) w (non-canonical))]
    [(struct* abi-int ([bits n]))
     (define v (if (< w (expt 2 255)) w (- w (expt 2 256))))
     (if (<= (- (expt 2 (sub1 n))) v (sub1 (expt 2 (sub1 n)))) v (non-canonical))]
    [(struct* abi-address ())
     (if (< w (expt 2 160))
         (address->checksum-string (bytes->hex (subbytes data (+ at 12) (+ at 32))))
         (non-canonical))]
    [(struct* abi-bool ())
     (case w
       [(0) #f]
       [(1) #t]
       [else (non-canonical)])]
    [(struct* abi-fixed-bytes ([size n]))
     (if (zero? (bitwise-and w (sub1 (expt 2 (* 8 (- 32 n))))))
         (subbytes data at (+ at n))
         (non-canonical))]))
