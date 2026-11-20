#lang racket/base

(require (for-syntax racket/base
                     (only-in racket/struct-info extract-struct-info prop:struct-info)
                     syntax/parse)
         racket/contract/base
         (only-in keccak keccak256)
         (only-in racket/function negate)
         (only-in racket/list make-list)
         (only-in racket/match == match match-define struct*)
         (only-in racket/string string-join string-prefix?)
         "abi/description.rkt"
         "abi/event-type.rkt"
         "abi/type.rkt"
         (submod "abi/type.rkt" private)
         "address.rkt"
         "private/abi-decode.rkt"
         "private/exn.rkt"
         (only-in "private/hex.rkt" bytes->hex bytes->natural hex->bytes natural->bytes)
         "private/who.rkt")

(provide (all-from-out "abi/description.rkt" "abi/event-type.rkt" "abi/type.rkt")
         (struct-out exn:fail:web3:rpc:revert)
         abi-revert
         struct:abi-revert
         (contract-out
          [function-selector (-> (or/c string? bytes? abi-signature?) bytes?)]
          [word->bytes (-> (integer-in 0 (sub1 (expt 2 256))) bytes?)]
          [bytes->words (-> bytes? (listof exact-nonnegative-integer?))]
          [abi-encode (-> (listof abi-type-spec/c) list? bytes?)]
          [abi-encode-packed (-> (listof abi-type-spec/c) list? bytes?)]
          [encode-call (->* (abi-signature-spec/c) #:rest list? bytes?)]
          [abi-decode (-> (listof abi-type-spec/c) bytes? list?)]
          [decode-result (-> abi-signature-spec/c bytes? list?)]
          [decode-call (-> abi-signature-spec/c bytes? list?)]
          [abi-revert? (-> any/c boolean?)]
          [abi-revert-signature (-> abi-revert? (or/c #f abi-signature?))]
          [abi-revert-arguments (-> abi-revert? list?)]
          [abi-revert-data (-> abi-revert? bytes?)]
          [abi-revert-name (-> abi-revert? (or/c #f string?))]
          [abi-revert-selector (-> abi-revert? (or/c #f bytes?))]
          [abi-revert->string (-> abi-revert? string?)]
          [error-selector (-> abi-signature-spec/c bytes?)]
          [decode-revert (->* (bytes?) (#:errors (listof abi-signature-spec/c)) abi-revert?)]
          [panic-description (-> exact-nonnegative-integer? (or/c #f string?))]
          [rpc-exn->revert (->* (exn:fail:web3:rpc?)
                                (#:errors (listof abi-signature-spec/c))
                                (or/c #f abi-revert?))]
          [rpc-exn->revert-exn (->* (exn:fail:web3:rpc?)
                                    (#:errors (listof abi-signature-spec/c))
                                    (or/c #f exn:fail:web3:rpc:revert?))]
          [decode-revert-reason (-> bytes? (or/c string? #f))]
          [exn->abi-revert (-> exn? (or/c abi-revert? #f))]))

(module+ test
  (require rackunit
           (only-in "rpc.rkt" bytes->hex-string)))

(define (function-selector signature)
  (cond
    [(bytes? signature) (subbytes (keccak256 signature) 0 4)]
    [else (signature-selector (string->abi-signature/who 'function-selector signature))]))

(define (signature-selector sig)
  (subbytes (keccak256 (abi-signature->string sig)) 0 4))

(define (word->bytes n)
  (natural->bytes n 32))

(define (bytes->words bs)
  (for/list ([i (in-range 0 (bytes-length bs) 32)])
    (bytes->natural bs i (min (bytes-length bs) (+ i 32)))))

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

(define/who (abi-encode who types vs)
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

(define/who (encode-call who signature . vs)
  (define sig (string->abi-signature/who who signature))
  (bytes-append (signature-selector sig) (encode-arguments who (abi-signature-inputs sig) vs)))

(define (string->abi-signature/who who signature)
  (if (string? signature) (string->signature who signature) signature))

(define/who (abi-decode who types data [base #:public 0])
  (decode-arguments who (types/who who types) data base))

(define (result-signature/who who signature)
  (define sig (string->abi-signature/who who signature))
  (cond
    [(abi-signature-outputs sig) sig]
    [else (raise-abi-type-error who "signature has no outputs; write name(...)(...)"
                                (abi-signature->string sig) #f)]))

(define/who (decode-result who signature data)
  (define sig (result-signature/who who signature))
  (decode-arguments who (abi-signature-outputs sig) data 0))

(define (decode-call signature data)
  (define sig (string->abi-signature/who 'decode-call signature))
  (define selector (signature-selector sig))
  (define given (subbytes data 0 (min 4 (bytes-length data))))
  (cond
    [(bytes=? given selector) (decode-arguments 'decode-call (abi-signature-inputs sig) data 4)]
    [else
     (raise-abi-decode-error 'decode-call 'selector (abi-signature-inputs sig) 0
                             (format "expected ~a, given ~a" (bytes->hex selector) (bytes->hex given)))]))

;; abi-revert is the struct's static information, for match and struct*,
;; and as an expression the constructor under a contract that a field
;; contract cannot state: the arguments are values of the signature's
;; inputs.  This module builds reverts it decoded with raw-abi-revert.
(struct abi-revert (signature arguments data) #:transparent
  #:name abi-revert-info
  #:constructor-name raw-abi-revert)

(define (revert-arguments/c signature)
  (cond
    [signature
     (define inputs (abi-signature-inputs signature))
     (flat-named-contract `(arguments-of ,(abi-signature->string signature))
                          (lambda (vs) (and (list? vs) (not (arguments-failure inputs vs)))))]
    [else list?]))

(define-module-boundary-contract checked-abi-revert raw-abi-revert
  (->i ([signature (or/c #f abi-signature?)]
        [arguments (signature) (revert-arguments/c signature)]
        [data bytes?])
       [_ abi-revert?])
  #:name-for-blame abi-revert)

(begin-for-syntax
  (struct constructor-info (info)
    #:property prop:struct-info (lambda (self) (constructor-info-info self))
    #:property prop:procedure
    (lambda (self stx)
      (syntax-parse stx
        [(_ . args) (syntax/loc stx (checked-abi-revert . args))]
        [_:id #'checked-abi-revert]))))

(define-syntax abi-revert
  (constructor-info
   (let ([info (extract-struct-info (syntax-local-value #'abi-revert-info))])
     (list* (car info) #'checked-abi-revert (cddr info)))))

(define (abi-revert-name r)
  (match r
    [(struct* abi-revert ([signature (struct* abi-signature ([name name]))])) name]
    [_ #f]))

(define (abi-revert-selector r)
  (match-define (struct* abi-revert ([data data])) r)
  (and (>= (bytes-length data) 4) (subbytes data 0 4)))

(define error-signature (string->signature 'decode-revert "Error(string)"))
(define panic-signature (string->signature 'decode-revert "Panic(uint256)"))

(define (error-signature/who who signature)
  (define sig (string->abi-signature/who who signature))
  (cond
    [(abi-signature-outputs sig)
     (raise-abi-type-error who "an error signature has no outputs"
                           (abi-signature->string sig #:outputs? #t) #f)]
    [else sig]))

(define (error-selector signature)
  (signature-selector (error-signature/who 'error-selector signature)))

(define (same-signature? a b)
  (equal? (abi-signature->string a) (abi-signature->string b)))

(define (check-error-signatures/who who errors)
  (for/fold ([lookup (hash)])
            ([sig (in-list (list* error-signature
                                  panic-signature
                                  (map (lambda (s) (error-signature/who who s)) errors)))])
    (define selector (signature-selector sig))
    (match (hash-ref lookup selector #f)
      [#f (hash-set lookup selector sig)]
      [other
       #:when (same-signature? other sig)
       lookup]
      [other
       (raise-abi-type-error who
                             (format "selector collision: ~a and ~a"
                                     (abi-signature->string other) (abi-signature->string sig))
                             errors
                             #f)])))

(define (lookup-revert who lookup data)
  (cond
    [(and (>= (bytes-length data) 4) (hash-ref lookup (subbytes data 0 4) #f))
     => (lambda (sig) (raw-abi-revert sig (decode-arguments who (abi-signature-inputs sig) data 4) data))]
    [else (raw-abi-revert #f '() data)]))

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

(define (panic-label code)
  (format "Panic(0x~a)" (number->string code 16)))

(define (value->display-string type v)
  (fold-abi-value
   (lambda (t v)
     (match t
       [(or (struct* abi-uint ()) (struct* abi-int ())) (number->string v)]
       [(struct* abi-bool ()) (if v "true" "false")]
       [(struct* abi-address ()) (address->checksum-string v)]
       [(or (struct* abi-bytes ()) (struct* abi-fixed-bytes ())) (bytes->hex v)]
       [(struct* abi-string ()) (format "~s" v)]))
   (lambda (t parts)
     (define joined (string-join parts ", "))
     (if (abi-tuple? t) (format "(~a)" joined) (format "[~a]" joined)))
   type
   v))

(define (abi-value->display-string type v)
  (value->display-string (abi-type/who 'abi-value->display-string type) v))

(define (abi-revert->string r)
  (match r
    [(struct* abi-revert ([signature (== panic-signature)] [arguments (list code)]))
     (match (panic-description code)
       [#f (panic-label code)]
       [description (format "~a: ~a" (panic-label code) description)])]
    [(struct* abi-revert ([signature (struct* abi-signature ([name name] [inputs ts]))]
                          [arguments args]))
     (format "~a(~a)" name (string-join (map value->display-string ts args) ", "))]
    [(struct* abi-revert ([data data]))
     (define n (bytes-length data))
     (cond
       [(zero? n) "no revert data"]
       [(< n 4) (format "revert data ~a" (bytes->hex data))]
       [else
        (format "unknown error ~a with ~a bytes of data" (bytes->hex (subbytes data 0 4)) (- n 4))])]))

(define (hex-payload v)
  (match v
    [(? string? (pregexp #px"^(?:Reverted )?(0x(?:[0-9a-fA-F]{2})*)$" (list _ hex)))
     (hex->bytes hex)]
    [_ #f]))

(define (revert-payload e)
  (match-define (struct* exn:fail:web3:rpc ([rpc-message message] [data data])) e)
  (match data
    [(or #f 'null) (and (string-prefix? message "execution reverted") #"")]
    [(? hash?) (hex-payload (hash-ref data 'data #f))]
    [_ (hex-payload data)]))

(define/who (rpc-exn->revert who e #:errors [errors '()])
  (define lookup (check-error-signatures/who who errors))
  (define payload (revert-payload e))
  (and payload
       (with-handlers ([exn:fail:web3:abi:decode? (lambda (_) (raw-abi-revert #f '() payload))])
         (lookup-revert who lookup payload))))

(define (exn->abi-revert e)
  (match e
    [(struct* exn:fail:web3:rpc:revert ([revert revert])) revert]
    [(struct* exn:fail:web3:transaction ([revert revert])) revert]
    [_ #f]))

(define (rpc-exn->revert-exn e #:errors [errors '()])
  (match-define
    (struct* exn:fail:web3:rpc ([method method] [code code] [rpc-message message] [data data]))
    e)
  (define r (rpc-exn->revert/who 'rpc-exn->revert-exn e #:errors errors))
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
    (match (decode-revert data)
      [(and r (struct* abi-revert ([signature (== error-signature)]))) (abi-revert->string r)]
      [(struct* abi-revert ([signature (== panic-signature)] [arguments (list code)]))
       (panic-label code)]
      [_ #f])))

(module* internal #f
  (provide abi-decode/who
           abi-encode/who
           abi-value->display-string
           check-error-signatures/who
           decode-result/who
           encode-call/who
           result-signature/who
           string->abi-signature/who))

(module+ test
  (check-equal? (bytes->hex-string (function-selector "transfer(address,uint256)")) "0xa9059cbb")
  (check-equal? (bytes->hex-string (function-selector "inc()")) "0x371303c0")
  (check-equal? (bytes->words (bytes-append (word->bytes 1) (word->bytes (sub1 (expt 2 256)))))
                (list 1 (sub1 (expt 2 256))))
  (check-equal? (bytes-length (encode-call "f(uint256,uint256)" 1 2)) 68))
