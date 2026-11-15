#lang racket/base

;; Foundry's cast as an oracle for the ABI encoders.

(require racket/list
         racket/match
         racket/port
         racket/string
         web3/abi
         (only-in web3/rpc bytes->hex-string))

(provide cast-path
         value->cast-argument
         cast-version
         cast-encode)

(define cast-path
  (let ([env (getenv "WEB3_CAST")])
    (if (and env (not (string=? env "")))
        env
        (find-executable-path "cast"))))

;; cast reads a nested string between double quotes with no escapes, and
;; trims trailing whitespace from a top-level one.
(define (value->cast-argument type v [top? #t])
  (define t (abi-type type))
  (cond
    [(or (abi-uint? t) (abi-int? t)) (number->string v)]
    [(abi-bool? t) (if v "true" "false")]
    [(abi-address? t) v]
    [(or (abi-bytes? t) (abi-fixed-bytes? t)) (bytes->hex-string v)]
    [(and (abi-string? t) top? (regexp-match? #px"\\s$" v))
     (error 'value->cast-argument "cast trims trailing whitespace: ~s" v)]
    [(and (abi-string? t) top?) v]
    [(and (abi-string? t) (or (string-contains? v "\"") (string-contains? v "\\")))
     (error 'value->cast-argument "cast cannot quote ~s" v)]
    [(abi-string? t) (format "\"~a\"" v)]
    [(abi-array? t) (format "[~a]" (join-arguments (make-list (length v) (abi-array-element t)) v))]
    [(abi-fixed-array? t)
     (format "[~a]" (join-arguments (make-list (length v) (abi-fixed-array-element t)) v))]
    [else (format "(~a)" (join-arguments (abi-tuple-components t) v))]))

(define (join-arguments types vs)
  (string-join (map (lambda (t v) (value->cast-argument t v #f)) types vs) ","))

(define (run-cast . args)
  (define-values (process out in err) (apply subprocess #f #f #f cast-path args))
  (close-output-port in)
  (define stdout (port->string out))
  (define stderr (port->string err))
  (subprocess-wait process)
  (close-input-port out)
  (close-input-port err)
  (unless (zero? (subprocess-status process))
    (error 'cast "~a failed: ~a" (string-join args " ") (string-trim stderr)))
  (string-trim stdout))

(define (cast-version)
  (match (regexp-match #px"Version: (\\S+)" (run-cast "--version"))
    [(list _ version) version]
    [#f "(unknown version)"]))

;; mode is 'encode, 'packed or 'call; signature is "name(T,...)".
(define (cast-encode mode signature vs)
  (define inputs (abi-signature-inputs (string->abi-signature signature)))
  (define arguments (map value->cast-argument inputs vs))
  (string-downcase
   (match mode
     ['encode (apply run-cast "abi-encode" "--" signature arguments)]
     ['packed (apply run-cast "abi-encode" "--packed" "--" signature arguments)]
     ['call (apply run-cast "calldata" "--" signature arguments)])))
