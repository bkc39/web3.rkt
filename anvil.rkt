#lang racket/base

;; Local development nodes: anvil's own RPC methods, debug tracing, and
;; starting / stopping an anvil process.
;;
;;   (with-anvil ()                       ; fresh node, current-provider set
;;     (eth-accounts))
;;
;;   (define node (start-anvil #:hardfork "osaka"))
;;   (with-provider (anvil-provider node) ...)
;;   (stop-anvil node)

(require json
         racket/contract
         racket/file
         racket/tcp
         syntax/parse/define
         "eth.rkt"
         "rpc.rkt")

(provide anvil-set-code
         anvil-set-balance
         anvil-set-storage-at
         anvil-mine
         anvil-impersonate-account
         anvil-stop-impersonating-account
         evm-snapshot
         evm-revert
         debug-trace-call
         debug-trace-transaction
         current-anvil-path
         (struct-out anvil)
         start-anvil
         stop-anvil
         anvil-running?
         with-anvil)

;;;; anvil / hardhat-style methods

(define-jsonrpc-endpoint anvil_setCode (-> string? hex-string? any/c))
(define-jsonrpc-endpoint anvil_setBalance
  (-> string? [exact-nonnegative-integer? number->hex-string] any/c))
(define-jsonrpc-endpoint anvil_setStorageAt
  (-> string? [exact-nonnegative-integer? number->hex-string] hex-string? any/c))
(define-jsonrpc-endpoint anvil_mine (-> any/c))
(define-jsonrpc-endpoint anvil_impersonateAccount (-> string? any/c))
(define-jsonrpc-endpoint anvil_stopImpersonatingAccount (-> string? any/c))
(define-jsonrpc-endpoint evm_snapshot (-> hex-string?))
(define-jsonrpc-endpoint evm_revert (-> hex-string? boolean?))

;; Struct-log traces (pc, op, gas, gasCost, stack, ...).  Options follow
;; geth's tracer config, e.g. (hasheq 'enableMemory #t 'disableStack #f).
(define-jsonrpc-endpoint debug_traceCall
  (-> (and/c hash? jsexpr?) [block-param/c block-param->string] (and/c hash? jsexpr?) hash?))
(define-jsonrpc-endpoint debug_traceTransaction
  (-> hex-string? (and/c hash? jsexpr?) hash?))

;;;; processes

;; Which anvil binary to run: $WEB3_ANVIL, else `anvil` on PATH.  An empty
;; WEB3_ANVIL counts as unset.
(define current-anvil-path
  (make-parameter (let ([env (getenv "WEB3_ANVIL")])
                    (if (and env (not (string=? env "")))
                        env
                        (find-executable-path "anvil")))))

(struct anvil (process port log-file provider))

(define (anvil-running? a)
  (eq? 'running (subprocess-status (anvil-process a))))

(define (free-port)
  (define listener (tcp-listen 0 4 #t "127.0.0.1"))
  (define-values (_host port _rhost _rport) (tcp-addresses listener #t))
  (tcp-close listener)
  port)

;; Start anvil on 127.0.0.1 and return once it answers RPC.  The process is
;; registered with the current custodian (shutting it down kills anvil) and
;; killed when Racket exits.  Output goes to a log file.
(define/contract (start-anvil #:path [path (current-anvil-path)]
                              #:port [port (free-port)]
                              #:hardfork [hardfork #f]
                              #:args [args '()]
                              #:timeout [timeout 20.0])
  (->* () (#:path (or/c #f path-string?)
           #:port (integer-in 1 65535)
           #:hardfork (or/c #f string?)
           #:args (listof string?)
           #:timeout real?)
       anvil?)
  (unless path
    (raise (exn:fail:web3 "start-anvil: anvil not found; set WEB3_ANVIL or current-anvil-path"
                          (current-continuation-marks))))
  (define log-file (make-temporary-file "web3-anvil-~a.log"))
  (define log-out (open-output-file log-file #:exists 'truncate))
  (define-values (proc _stdout stdin _stderr)
    (parameterize ([current-subprocess-custodian-mode 'kill])
      (apply subprocess log-out #f 'stdout path
             "--host" "127.0.0.1"
             "--port" (number->string port)
             (append (if hardfork (list "--hardfork" hardfork) '()) args))))
  (close-output-port stdin)
  (close-output-port log-out)
  (plumber-add-flush! (current-plumber)
                      (lambda (_handle)
                        (when (eq? 'running (subprocess-status proc))
                          (subprocess-kill proc #t))))
  (define deadline (+ (current-inexact-milliseconds) (* 1000 timeout)))
  (let loop ()
    (cond
      [(not (eq? 'running (subprocess-status proc)))
       (raise (exn:fail:web3 (format "start-anvil: anvil exited during startup:\n~a"
                                     (file->string log-file))
                             (current-continuation-marks)))]
      [(with-handlers ([exn:fail? (lambda (_) #f)])
         (define p (url->http-provider (format "http://127.0.0.1:~a" port)))
         (eth-chain-id #:provider p)
         p)
       => (lambda (provider) (anvil proc port log-file provider))]
      [(> (current-inexact-milliseconds) deadline)
       (subprocess-kill proc #t)
       (raise (exn:fail:web3 (format "start-anvil: no response after ~as; log: ~a" timeout log-file)
                             (current-continuation-marks)))]
      [else
       (sleep 0.02)
       (loop)])))

(define (stop-anvil a)
  (when (anvil-running? a)
    (subprocess-kill (anvil-process a) #t)
    (subprocess-wait (anvil-process a))))

;; (with-anvil (start-anvil-option ...) body ...)
;; Runs body with a fresh anvil as current-provider, stopping it afterwards.
(define-syntax-parse-rule (with-anvil (option ...) body:expr ...+)
  (let ([node (start-anvil option ...)])
    (dynamic-wind
      void
      (lambda () (with-provider (anvil-provider node) body ...))
      (lambda () (stop-anvil node)))))
