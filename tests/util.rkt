#lang racket/base

(require (for-syntax racket/base
                     (only-in racket/list take-right)
                     (only-in racket/string string-join))
         (only-in racket/contract exn:fail:contract:blame?)
         racket/list
         racket/match
         rackunit
         syntax/parse/define
         web3/abi
         (only-in (submod web3/abi/type private) map-abi-value)
         web3/address
         (only-in web3/rpc bytes->hex-string))

(provide alice
         alice-checksummed
         anvil-alice
         anvil-bob
         anvil-carol
         check-blames-here
         check-decode-error
         check-decoded
         checksummed
         downcased
         decode-error
         decoded-size
         encode-hex
         filled-hash
         insufficient-balance
         max-word
         raised
         raised-type-error
         seeded
         signature-inputs
         signature-string
         widths
         word
         zero-address)

(define widths (range 8 257 8))

(define max-word (sub1 (expt 2 256)))

(define alice "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed")
(define alice-checksummed "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed")

(define anvil-alice "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")
(define anvil-bob "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")
(define anvil-carol "0x3C44CdDdB6a900fa2b585dd299e03d12FA4293BC")

(define insufficient-balance "InsufficientBalance(uint256,uint256)")

(define zero-address "0x0000000000000000000000000000000000000000")

(define (word n)
  (word->bytes n))

(define (filled-hash c)
  (string-append "0x" (make-string 64 c)))

(define (seeded . seed)
  (vector->pseudo-random-generator (list->vector seed)))

(define (signature-string name inputs [outputs #f])
  (abi-signature->string (abi-signature name inputs outputs) #:outputs? #t))

(define (signature-inputs signature)
  (abi-signature-inputs (string->abi-signature signature)))

(define (encode-hex mode signature vs)
  (bytes->hex-string
   (match mode
     ['encode (abi-encode (signature-inputs signature) vs)]
     ['packed (abi-encode-packed (signature-inputs signature) vs)]
     ['call (apply encode-call signature vs)])))

(define (blames? file-name e)
  (and (exn:fail:contract:blame? e)
       (regexp-match? (regexp (string-append "blaming: [^\n]*" (regexp-quote file-name)))
                      (exn-message e))))

;; A contract violation blaming the module this form appears in, named by its directory and file
;; so that tests/account.rkt is not mistaken for web3/account.rkt.
(define-syntax-parse-rule (check-blames-here expr)
  #:with file-name
  (datum->syntax #'expr (string-join (map path->string
                                          (take-right (explode-path (syntax-source this-syntax)) 2))
                                     "/"))
  (check-exn (lambda (e) (blames? file-name e))
             (lambda () expr)
             (format "~s" 'expr)))

;; v with f applied to every address in it.
(define (map-addresses f type v)
  (map-abi-value (lambda (t x) (if (abi-address? t) (f x) x)) (abi-type type) v))

;; vs with every address in its EIP-55 form, as the decoders return them.
(define (checksummed types vs)
  (map (lambda (t v) (map-addresses address->checksum-string t v)) types vs))

(define (downcased types vs)
  (map (lambda (t v) (map-addresses string-downcase t v)) types vs))

;; What every successful decode of data as types promises of its result:
;; values of the types, addresses checksummed, and no more bytes than the
;; input.
(define (check-decoded types data vs [name ""])
  (check-equal? (length vs) (length types) name)
  (for ([t (in-list types)]
        [v (in-list vs)])
    (check-true (abi-value? t v) name)
    (check-equal? (map-addresses (lambda (a) (and (checksum-address? a) a)) t v) v name))
  (check-true (<= (for/sum ([t (in-list types)] [v (in-list vs)]) (decoded-size t v))
                  (bytes-length data))
              name))

;; The bytes a decoded value accounts for: a word per static leaf, and
;; the content of each bytes and string.
(define (decoded-size type v)
  (match (abi-type type)
    [(struct* abi-bytes ()) (bytes-length v)]
    [(struct* abi-string ()) (bytes-length (string->bytes/utf-8 v))]
    [(or (struct* abi-array ([element e])) (struct* abi-fixed-array ([element e])))
     (for/sum ([x (in-list v)]) (decoded-size e x))]
    [(struct* abi-tuple ([components cs])) (for/sum ([t (in-list cs)] [x (in-list v)]) (decoded-size t x))]
    [_ 32]))

(define (raised thunk)
  (with-handlers ([exn:fail? values])
    (thunk)
    #f))

(define (raised-type-error thunk)
  (with-handlers ([exn:fail:web3:abi:type? values])
    (thunk)
    #f))

(define (decode-error thunk)
  (with-handlers ([exn:fail:web3:abi:decode? values])
    (thunk)
    #f))

;; expr raises exn:fail:web3:abi:decode with this reason and offset, a
;; message under who, and (when given) this type: a type string, or a
;; list of them for a whole head.
(define-syntax-parse-rule (check-decode-error expr who:expr reason:expr offset:expr
                                              (~optional (~seq #:type type:expr)))
  (let ([e (decode-error (lambda () expr))]
        [name (format "~s" 'expr)])
    (check-pred exn:fail:web3:abi:decode? e name)
    (when e
      (check-regexp-match (regexp (string-append "^" (regexp-quote (symbol->string who)) ": "))
                          (exn-message e)
                          name)
      (check-equal? (exn:fail:web3:abi:decode-reason e) reason name)
      (check-equal? (exn:fail:web3:abi:decode-offset e) offset name)
      (~? (check-equal? (exn:fail:web3:abi:decode-type e) (expected-type type) name)))))

(define (expected-type spec)
  (if (list? spec) (map abi-type spec) (abi-type spec)))
