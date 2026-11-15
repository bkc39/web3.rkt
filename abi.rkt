#lang racket/base

;; The contract ABI: selectors, the standard and packed encodings, call
;; data, and the conventional revert payloads (Error(string), Panic(uint256)).

(require racket/contract/base
         (only-in keccak keccak256)
         "abi/type.rkt")

(provide (all-from-out "abi/type.rkt")
         (contract-out
          [function-selector (-> (or/c string? bytes? abi-signature?) bytes?)]
          [word->bytes (-> exact-nonnegative-integer? bytes?)]
          [bytes->words (-> bytes? (listof exact-nonnegative-integer?))]
          [abi-encode (-> (listof abi-type-spec/c) list? bytes?)]
          [abi-encode-packed (-> (listof abi-type-spec/c) list? bytes?)]
          [encode-call (->* ((or/c string? abi-signature?)) #:rest list? bytes?)]
          [decode-revert-reason (-> bytes? (or/c string? #f))]))

(module+ test
  (require rackunit
           (only-in "rpc.rkt" bytes->hex-string)))

(define (function-selector signature) (error 'unimplemented))

(define (word->bytes n)
  (unless (< n (expt 2 256))
    (raise-argument-error 'word->bytes "a value below 2^256" n))
  (apply bytes (for/list ([i (in-range 31 -1 -1)])
                 (bitwise-and (arithmetic-shift n (* -8 i)) 255))))

;; Big-endian 32-byte words; a short trailing chunk is read as-is.
(define (bytes->words bs)
  (for/list ([i (in-range 0 (bytes-length bs) 32)])
    (for/fold ([n 0]) ([b (in-bytes bs i (min (bytes-length bs) (+ i 32)))])
      (+ (* n 256) b))))

(define (abi-encode types vs) (error 'unimplemented))
(define (abi-encode-packed types vs) (error 'unimplemented))
(define (encode-call signature . vs) (error 'unimplemented))

(define error-selector (bytes #x08 #xc3 #x79 #xa0)) ; Error(string)
(define panic-selector (bytes #x4e #x48 #x7b #x71)) ; Panic(uint256)

;; A readable reason for Error(string) and Panic(uint256) payloads, or #f.
(define (decode-revert-reason data)
  (define n (bytes-length data))
  (define (word-at i) (car (bytes->words (subbytes data i (+ i 32)))))
  (cond
    [(and (>= n 68) (equal? (subbytes data 0 4) error-selector))
     (define len (word-at 36))
     (and (>= n (+ 68 len))
          (format "Error(~s)" (bytes->string/utf-8 (subbytes data 68 (+ 68 len)) #\?)))]
    [(and (= n 36) (equal? (subbytes data 0 4) panic-selector))
     (format "Panic(0x~a)" (number->string (word-at 4) 16))]
    [else #f]))

(module+ test
  (check-equal? (bytes->hex-string (function-selector "transfer(address,uint256)")) "0xa9059cbb")
  (check-equal? (bytes->hex-string (function-selector "inc()")) "0x371303c0")
  (check-equal? (bytes->words (bytes-append (word->bytes 1) (word->bytes (sub1 (expt 2 256)))))
                (list 1 (sub1 (expt 2 256))))
  (check-equal? (bytes-length (encode-call "f(uint256,uint256)" 1 2)) 68)
  (check-equal? (decode-revert-reason
                 (bytes-append error-selector (word->bytes 32) (word->bytes 5) #"oops!"
                               (make-bytes 27 0)))
                "Error(\"oops!\")")
  (check-equal? (decode-revert-reason (bytes-append panic-selector (word->bytes #x11)))
                "Panic(0x11)")
  (check-false (decode-revert-reason #"")))
