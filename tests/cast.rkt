#lang racket/base

(require (only-in json string->jsexpr)
         (only-in racket/function curry)
         racket/list
         racket/match
         racket/port
         racket/string
         web3/abi
         (only-in (submod web3/abi/type private) map-abi-value)
         (only-in web3/rpc bytes->hex-string)
         "util.rkt")

(provide cast-decode
         cast-encode
         cast-path
         cast-succeeds?
         cast-version
         run-cast
         value->cast-argument
         value->cast-json)

(define cast-path
  (let ([env (getenv "WEB3_CAST")])
    (if (and env (not (string=? env "")))
        env
        (find-executable-path "cast"))))

;; cast reads a nested string between double quotes with no escapes, and
;; trims trailing whitespace from a top-level one.
(define (value->cast-argument type v [top? #t])
  (match (abi-type type)
    [(or (struct* abi-uint ()) (struct* abi-int ())) (number->string v)]
    [(struct* abi-bool ()) (if v "true" "false")]
    [(struct* abi-address ()) v]
    [(or (struct* abi-bytes ()) (struct* abi-fixed-bytes ())) (bytes->hex-string v)]
    [(struct* abi-string ())
     #:when (and top? (regexp-match? #px"\\s$" v))
     (error 'value->cast-argument "cast trims trailing whitespace: ~s" v)]
    [(struct* abi-string ()) #:when top? v]
    [(struct* abi-string ())
     #:when (or (string-contains? v "\"") (string-contains? v "\\"))
     (error 'value->cast-argument "cast cannot quote ~s" v)]
    [(struct* abi-string ()) (format "\"~a\"" v)]
    [(or (struct* abi-array ([element e])) (struct* abi-fixed-array ([element e])))
     (format "[~a]" (join-arguments (make-list (length v) e) v))]
    [(struct* abi-tuple ([components cs])) (format "(~a)" (join-arguments cs v))]))

(define (join-arguments types vs)
  (string-join (map (lambda (t v) (value->cast-argument t v #f)) types vs) ","))

;; Arguments go as UTF-8 bytes: as strings they would be converted through
;; the locale, which in a build sandbox turns non-ASCII into "?".
(define (call-cast args)
  (define-values (process out in err)
    (apply subprocess #f #f #f cast-path (map string->bytes/utf-8 args)))
  (close-output-port in)
  (define stdout (port->string out))
  (define stderr (port->string err))
  (subprocess-wait process)
  (close-input-port out)
  (close-input-port err)
  (values (subprocess-status process) stdout stderr))

(define (run-cast . args)
  (define-values (status stdout stderr) (call-cast args))
  (unless (zero? status)
    (error 'cast "~a failed: ~a" (string-join args " ") (string-trim stderr)))
  (string-trim stdout))

(define (cast-succeeds? . args)
  (define-values (status _stdout _stderr) (call-cast args))
  (zero? status))

(define (cast-version)
  (match (regexp-match #px"Version: (\\S+)" (run-cast "--version"))
    [(list _ version) version]
    [#f "(unknown version)"]))

;; cast's --packed left-pads every array element to 32 bytes, so negative
;; integers lose their sign extension and bytesN elements are padded on the
;; wrong side; solc pads each element as in the standard encoding.  Packed
;; arrays go through cast's standard mode instead.
(define (packed-argument-hex t v)
  (define hex
    (match t
      [(or (struct* abi-array ([element e])) (struct* abi-fixed-array ([element e])))
       (apply run-cast "abi-encode" "--"
              (format "f(~a)" (string-join (make-list (length v) (abi-type->string e)) ","))
              (map (curry value->cast-argument e) v))]
      [_
       (run-cast "abi-encode" "--packed" "--" (format "f(~a)" (abi-type->string t))
                 (value->cast-argument t v))]))
  (substring hex 2))

(define (cast-encode mode signature vs)
  (define inputs (signature-inputs signature))
  (define arguments (map value->cast-argument inputs vs))
  (string-downcase
   (match mode
     ['encode (apply run-cast "abi-encode" "--" signature arguments)]
     ['packed (string-append "0x" (apply string-append (map packed-argument-hex inputs vs)))]
     ['call (apply run-cast "calldata" "--" signature arguments)])))

;; cast --json writes integers of up to 64 bits as numbers and wider ones
;; as decimal strings.
(define (value->cast-json type v)
  (map-abi-value (lambda (t x)
                   (match t
                     [(or (struct* abi-uint ([bits n])) (struct* abi-int ([bits n])))
                      (if (<= n 64) x (number->string x))]
                     [(or (struct* abi-bytes ()) (struct* abi-fixed-bytes ())) (bytes->hex-string x)]
                     [_ x]))
                 (abi-type type)
                 v))

;; The values cast decodes from hex, as JSON: the outputs of "f()(T,...)"
;; in mode 'result, the inputs of a signature in mode 'call.
(define (cast-decode mode signature hex)
  (define out
    (match mode
      ['result (run-cast "abi-decode" "--json" "--" signature hex)]
      ['call (run-cast "calldata-decode" "--json" "--" signature hex)]))
  (hash-ref (string->jsexpr out) 'data))
