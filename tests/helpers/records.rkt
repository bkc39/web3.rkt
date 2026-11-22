#lang racket/base

(require racket/format
         (only-in racket/file file->string)
         (only-in racket/match match)
         rackunit
         (only-in web3/private/record field-row-field field-row-provided?)
         (only-in web3/rpc exn:fail:web3?)
         (only-in "../modules.rkt" exported-names)
         (only-in "../util.rkt" raised))

(provide accessor-name
         check-fields
         check-in-web3-and-manual
         message-of
         provided-accessors)

(define (message-of thunk)
  (define e (raised thunk))
  (check-pred exn:fail:web3? e)
  (check-false (exn:fail:contract? e))
  (and e (exn-message e)))

(define (accessor-name prefix row)
  (string->symbol (~a prefix "-" (field-row-field row))))

(define (provided-accessors prefix rows)
  (for/list ([row (in-list rows)]
             #:when (field-row-provided? row))
    (accessor-name prefix row)))

(define (check-fields record prefix rows expected)
  (for ([name (in-list (provided-accessors prefix rows))]
        [row (in-list (filter field-row-provided? rows))])
    (match (assq (field-row-field row) expected)
      [(list _ value) (check-equal? ((dynamic-require 'web3 name) record) value (~a name))]
      [#f (fail (~a "no expected value for " name))])))

(define (check-in-web3-and-manual names)
  (define manual (file->string (collection-file-path "scribblings/web3.scrbl" "web3")))
  (define exported (exported-names 'web3))
  (for ([name (in-list names)])
    (check-not-false (memq name exported) (~a name))
    (check-regexp-match (regexp (~a "@defproc\\[\\(" (regexp-quote (~a name)) " ")) manual
                        (~a name))))
