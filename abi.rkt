#lang racket/base

(require racket/contract/base
         (only-in keccak keccak256)
         (only-in racket/function negate)
         (only-in racket/list make-list)
         (only-in racket/match == match match* match-define struct*)
         (only-in racket/string string-join string-prefix?)
         "abi/type.rkt"
         (submod "abi/type.rkt" private)
         "address.rkt"
         "private/abi-decode.rkt"
         (only-in "private/exn.rkt"
                  exn:fail:web3:rpc
                  exn:fail:web3:rpc?
                  exn:fail:web3:rpc:revert
                  exn:fail:web3:rpc:revert?
                  exn:fail:web3:rpc:revert-revert
                  struct:exn:fail:web3:rpc:revert)
         (only-in "private/hex.rkt" bytes->hex hex-digits->bytes))

(provide (all-from-out "abi/type.rkt")
         (struct-out exn:fail:web3:rpc:revert)
         (contract-out
          [function-selector (-> (or/c string? bytes? abi-signature?) bytes?)]
          [word->bytes (-> (integer-in 0 (sub1 (expt 2 256))) bytes?)]
          [bytes->words (-> bytes? (listof exact-nonnegative-integer?))]
          [abi-encode (-> (listof abi-type-spec/c) list? bytes?)]
          [abi-encode-packed (-> (listof abi-type-spec/c) list? bytes?)]
          [encode-call (->* ((or/c string? abi-signature?)) #:rest list? bytes?)]
          [abi-decode (-> (listof abi-type-spec/c) bytes? list?)]
          [decode-result (-> (or/c string? abi-signature?) bytes? list?)]
          [decode-call (-> (or/c string? abi-signature?) bytes? list?)]
          (struct abi-revert ([signature (or/c #f abi-signature?)]
                              [arguments list?]
                              [data bytes?]))
          [abi-revert-name (-> abi-revert? (or/c #f string?))]
          [abi-revert-selector (-> abi-revert? (or/c #f bytes?))]
          [abi-revert->string (-> abi-revert? string?)]
          [error-selector (-> error-signature/c bytes?)]
          [decode-revert (->* (bytes?) (#:errors (listof error-signature/c)) abi-revert?)]
          [panic-description (-> exact-nonnegative-integer? (or/c #f string?))]
          [rpc-exn->revert (->* (exn:fail:web3:rpc?)
                                (#:errors (listof error-signature/c))
                                (or/c #f abi-revert?))]
          [rpc-exn->revert-exn (->* (exn:fail:web3:rpc?)
                                    (#:errors (listof error-signature/c))
                                    (or/c #f exn:fail:web3:rpc:revert?))]
          [decode-revert-reason (-> bytes? (or/c string? #f))]))

(define error-signature/c
  (flat-named-contract 'error-signature/c (or/c string? abi-signature?)))

(module+ test
  (require rackunit
           (only-in "rpc.rkt" bytes->hex-string)))

(define (function-selector signature)
  (match signature
    [(? bytes?) (subbytes (keccak256 signature) 0 4)]
    [_ (signature-selector (string->abi-signature/who 'function-selector signature))]))

(define (signature-selector sig)
  (subbytes (keccak256 (abi-signature->string sig)) 0 4))

(define (word->bytes n)
  (natural->bytes n 32))

(define (natural->bytes n width)
  (define bs (make-bytes width 0))
  (for ([i (in-range width)])
    (bytes-set! bs (- width i 1) (bitwise-and (arithmetic-shift n (* -8 i)) 255)))
  bs)

(define (bytes->words bs)
  (for/list ([i (in-range 0 (bytes-length bs) 32)])
    (for/fold ([n 0]) ([b (in-bytes bs i (min (bytes-length bs) (+ i 32)))])
      (+ (* n 256) b))))

(define (address->natural a)
  (string->number (substring a 2) 16))

(define (pad-right bs)
  (bytes-append bs (make-bytes (modulo (- (bytes-length bs)) 32) 0)))

(define (encode-bytes bs)
  (bytes-append (word->bytes (bytes-length bs)) (pad-right bs)))

;; Values are checked before encoding starts.
(define (encode t v)
  (match t
    [(struct* abi-uint ()) (word->bytes v)]
    [(struct* abi-int ()) (word->bytes (modulo v (expt 2 256)))]
    [(struct* abi-address ()) (word->bytes (address->natural v))]
    [(struct* abi-bool ()) (word->bytes (if v 1 0))]
    [(struct* abi-fixed-bytes ()) (pad-right v)]
    [(struct* abi-bytes ()) (encode-bytes v)]
    [(struct* abi-string ()) (encode-bytes (string->bytes/utf-8 v))]
    [(struct* abi-array ([element e]))
     (bytes-append (word->bytes (length v)) (encode-sequence (make-list (length v) e) v))]
    [(struct* abi-fixed-array ([element e] [length k])) (encode-sequence (make-list k e) v)]
    [(struct* abi-tuple ([components cs])) (encode-sequence cs v)]))

(define (encode-sequence ts vs)
  (define head-size (for/sum ([t (in-list ts)]) (or (static-size t) 32)))
  (define-values (heads tails _end)
    (for/fold ([heads '()] [tails '()] [offset head-size])
              ([t (in-list ts)]
               [v (in-list vs)])
      (define encoded (encode t v))
      (if (dynamic? t)
          (values (cons (word->bytes offset) heads) (cons encoded tails) (+ offset (bytes-length encoded)))
          (values (cons encoded heads) tails offset))))
  (apply bytes-append (append (reverse heads) (reverse tails))))

(define (encode-arguments who ts vs)
  (cond
    [(arguments-failure ts vs) => (lambda (failure) (raise-abi-value-error who failure))]
    [else (encode-sequence ts vs)]))

(define (abi-encode types vs)
  (abi-encode/who 'abi-encode types vs))

(define (abi-encode/who who types vs)
  (encode-arguments who (types/who who types) vs))

(define (packable? t)
  (match t
    [(or (struct* abi-array ([element e])) (struct* abi-fixed-array ([element e])))
     (not (or (dynamic? e) (abi-fixed-array? e) (abi-tuple? e)))]
    [(struct* abi-tuple ()) #f]
    [_ #t]))

(define (encode-packed t v)
  (match t
    [(struct* abi-uint ([bits n])) (natural->bytes v (quotient n 8))]
    [(struct* abi-int ([bits n])) (natural->bytes (modulo v (expt 2 n)) (quotient n 8))]
    [(struct* abi-address ()) (natural->bytes (address->natural v) 20)]
    [(struct* abi-bool ()) (bytes (if v 1 0))]
    [(struct* abi-fixed-bytes ()) v]
    [(struct* abi-bytes ()) v]
    [(struct* abi-string ()) (string->bytes/utf-8 v)]
    [(or (struct* abi-array ([element e])) (struct* abi-fixed-array ([element e])))
     (apply bytes-append (map (lambda (x) (encode e x)) v))]))

(define (abi-encode-packed types vs)
  (define ts (types/who 'abi-encode-packed types))
  (cond
    [(findf (negate packable?) ts)
     => (lambda (t)
          (raise-abi-type-error 'abi-encode-packed "not supported in packed encoding"
                                (type->string t) #f))]
    [(arguments-failure ts vs)
     => (lambda (failure) (raise-abi-value-error 'abi-encode-packed failure))]
    [else (apply bytes-append (map encode-packed ts vs))]))

(define (encode-call signature . vs)
  (encode-call/who 'encode-call signature vs))

(define (encode-call/who who signature vs)
  (define sig (string->abi-signature/who who signature))
  (bytes-append (signature-selector sig) (encode-arguments who (abi-signature-inputs sig) vs)))

;;;; decoding

(define (string->abi-signature/who who signature)
  (if (string? signature) (string->signature who signature) signature))

(define (abi-decode types data)
  (abi-decode/who 'abi-decode types data 0))

(define (abi-decode/who who types data base)
  (decode-arguments who (types/who who types) data base))

(define (decode-result signature data)
  (decode-result/who 'decode-result signature data))

(define (decode-result/who who signature data)
  (define sig (string->abi-signature/who who signature))
  (match (abi-signature-outputs sig)
    [#f (raise-abi-type-error who "signature has no outputs; write name(...)(...)"
                              (abi-signature->string sig) #f)]
    [outputs (decode-arguments who outputs data 0)]))

(define (decode-call signature data)
  (define sig (string->abi-signature/who 'decode-call signature))
  (define selector (signature-selector sig))
  (define given (subbytes data 0 (min 4 (bytes-length data))))
  (cond
    [(bytes=? given selector) (decode-arguments 'decode-call (abi-signature-inputs sig) data 4)]
    [else
     (raise-abi-decode-error 'decode-call 'selector (abi-signature-inputs sig) 0
                             (format "expected ~a, given ~a" (bytes->hex selector) (bytes->hex given)))]))

;;;; reverts

(struct abi-revert (signature arguments data) #:transparent)

(define (abi-revert-name r)
  (define sig (abi-revert-signature r))
  (and sig (abi-signature-name sig)))

(define (abi-revert-selector r)
  (define data (abi-revert-data r))
  (and (>= (bytes-length data) 4) (subbytes data 0 4)))

(define builtin-errors
  (map (lambda (s) (string->signature 'decode-revert s)) '("Error(string)" "Panic(uint256)")))

(define (error-signature/who who signature)
  (define sig (string->abi-signature/who who signature))
  (cond
    [(abi-signature-outputs sig)
     (raise-abi-type-error who "an error signature has no outputs"
                           (abi-signature->string sig #:outputs? #t) #f)]
    [else sig]))

(define (error-selector signature)
  (signature-selector (error-signature/who 'error-selector signature)))

;; The built-in errors and errors by selector.
(define (check-error-signatures/who who errors)
  (for/fold ([lookup (hash)])
            ([sig (in-list (append builtin-errors
                                   (map (lambda (s) (error-signature/who who s)) errors)))])
    (define selector (signature-selector sig))
    (match (hash-ref lookup selector #f)
      [#f (hash-set lookup selector sig)]
      [(== sig) lookup]
      [other
       (raise-abi-type-error who
                             (format "selector collision: ~a and ~a"
                                     (abi-signature->string other) (abi-signature->string sig))
                             errors
                             #f)])))

(define (lookup-revert who lookup data)
  (cond
    [(and (>= (bytes-length data) 4) (hash-ref lookup (subbytes data 0 4) #f))
     => (lambda (sig) (abi-revert sig (decode-arguments who (abi-signature-inputs sig) data 4) data))]
    [else (abi-revert #f '() data)]))

(define (decode-revert data #:errors [errors '()])
  (lookup-revert 'decode-revert (check-error-signatures/who 'decode-revert errors) data))

(define panic-descriptions
  (hash #x00 "generic/unspecified error"
        #x01 "assertion failed"
        #x11 "arithmetic underflow or overflow"
        #x12 "division or modulo by zero"
        #x21 "failed to convert value into enum type"
        #x22 "storage byte array incorrectly encoded"
        #x31 "called `.pop()` on an empty array"
        #x32 "array out-of-bounds access"
        #x41 "memory allocation error"
        #x51 "called an invalid internal function"))

(define (panic-description code)
  (hash-ref panic-descriptions code #f))

(define (abi-value->display-string type v)
  (let loop ([t (abi-type/who 'abi-value->display-string type)] [v v])
    (match t
      [(or (struct* abi-uint ()) (struct* abi-int ())) (number->string v)]
      [(struct* abi-bool ()) (if v "true" "false")]
      [(struct* abi-address ()) (address->checksum-string v)]
      [(or (struct* abi-bytes ()) (struct* abi-fixed-bytes ())) (bytes->hex v)]
      [(struct* abi-string ()) (format "~s" v)]
      [(or (struct* abi-array ([element e])) (struct* abi-fixed-array ([element e])))
       (format "[~a]" (string-join (map (lambda (x) (loop e x)) v) ", "))]
      [(struct* abi-tuple ([components cs])) (format "(~a)" (string-join (map loop cs v) ", "))])))

(define (abi-revert->string r)
  (match r
    [(struct* abi-revert ([signature (? abi-signature? sig)] [arguments args]))
     (match* ((abi-signature->string sig) args)
       [("Panic(uint256)" (list code))
        (define description (panic-description code))
        (format "Panic(0x~a)~a"
                (number->string code 16)
                (if description (string-append ": " description) ""))]
       [(_ _)
        (format "~a(~a)"
                (abi-signature-name sig)
                (string-join (map abi-value->display-string (abi-signature-inputs sig) args) ", "))])]
    [(struct* abi-revert ([data data]))
     (define n (bytes-length data))
     (cond
       [(zero? n) "no revert data"]
       [(< n 4) (format "revert data ~a" (bytes->hex data))]
       [else
        (format "unknown error ~a with ~a bytes of data" (bytes->hex (subbytes data 0 4)) (- n 4))])]))

(define (hex-payload v)
  (match v
    [(? string? (pregexp #px"^(?:Reverted )?0x((?:[0-9a-fA-F]{2})*)$" (list _ digits)))
     (hex-digits->bytes digits)]
    [_ #f]))

;; The revert data a node put in an error response, or #f.
(define (revert-payload e)
  (match-define (struct* exn:fail:web3:rpc ([rpc-message message] [data data])) e)
  (match data
    [(or #f 'null) (and (string-prefix? message "execution reverted") #"")]
    [(? hash?) (hex-payload (hash-ref data 'data #f))]
    [_ (hex-payload data)]))

(define (rpc-exn->revert/who who e errors)
  (define lookup (check-error-signatures/who who errors))
  (define payload (revert-payload e))
  (and payload
       (with-handlers ([exn:fail:web3:abi:decode? (lambda (_) (abi-revert #f '() payload))])
         (lookup-revert who lookup payload))))

(define (rpc-exn->revert e #:errors [errors '()])
  (rpc-exn->revert/who 'rpc-exn->revert e errors))

(define (rpc-exn->revert-exn e #:errors [errors '()])
  (match-define
    (struct* exn:fail:web3:rpc ([method method] [code code] [rpc-message message] [data data]))
    e)
  (define r (rpc-exn->revert/who 'rpc-exn->revert-exn e errors))
  (and r
       (exn:fail:web3:rpc:revert (format "~a: reverted with ~a" method (abi-revert->string r))
                                 (exn-continuation-marks e)
                                 method
                                 code
                                 message
                                 data
                                 r)))

(define (decode-revert-reason data)
  (with-handlers ([exn:fail:web3:abi:decode? (lambda (_) #f)])
    (define r (decode-revert data))
    (match r
      [(struct* abi-revert ([signature (struct* abi-signature ([name "Error"]))]))
       (abi-revert->string r)]
      [(struct* abi-revert ([signature (struct* abi-signature ([name "Panic"]))]
                            [arguments (list code)]))
       (format "Panic(0x~a)" (number->string code 16))]
      [_ #f])))

;; Uncontracted entry points for web3's own modules, which raise under
;; the name of the function their caller called.
(module* internal #f
  (provide abi-decode/who
           abi-encode/who
           abi-value->display-string
           check-error-signatures/who
           decode-result/who
           encode-call/who
           string->abi-signature/who))

(module+ test
  (check-equal? (bytes->hex-string (function-selector "transfer(address,uint256)")) "0xa9059cbb")
  (check-equal? (bytes->hex-string (function-selector "inc()")) "0x371303c0")
  (check-equal? (bytes->words (bytes-append (word->bytes 1) (word->bytes (sub1 (expt 2 256)))))
                (list 1 (sub1 (expt 2 256))))
  (check-equal? (bytes-length (encode-call "f(uint256,uint256)" 1 2)) 68))
