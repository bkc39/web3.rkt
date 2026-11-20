#lang racket/base

(require (for-syntax racket/base)
         (only-in racket/port with-output-to-string)
         (only-in racket/string string-split)
         syntax/parse/define
         (only-in web3 current-account current-anvil-path current-provider))

(provide example-lines
         example-output
         example-tests
         module-output)

(define-namespace-anchor anchor)

(define (module-output module-path)
  (define namespace (make-base-empty-namespace))
  (namespace-attach-module (namespace-anchor->empty-namespace anchor) 'web3 namespace)
  (define custodian (make-custodian))
  (dynamic-wind
   void
   (lambda ()
     (parameterize ([current-namespace namespace]
                    [current-custodian custodian]
                    [current-provider #f]
                    [current-account #f])
       (with-output-to-string (lambda () (dynamic-require module-path #f)))))
   (lambda () (custodian-shutdown-all custodian))))

(define (example-output name)
  (module-output `(submod ,(string->symbol (format "web3/examples/~a" name)) main)))

(define (example-lines name)
  (string-split (example-output name) "\n"))

(define (call-with-example name needs-anvil? thunk)
  (cond
    [(and needs-anvil? (not (current-anvil-path)))
     (printf "skipping example ~a: anvil not found\n" name)]
    [else (thunk)]))

(define-syntax-parse-rule (example-tests name:str
                                         (~optional (~seq #:anvil (~bind [needs-anvil #'#t]))
                                                    #:defaults ([needs-anvil #'#f]))
                                         body:expr ...+)
  (call-with-example name needs-anvil (lambda () body ...)))
