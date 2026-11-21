#lang racket/base

(module+ main
  (require (only-in json string->jsexpr)
           racket/match
           racket/port
           racket/runtime-path
           (only-in racket/string string-join string-prefix? string-split))

  (define-runtime-path vectors-path "vectors/transaction.rktd")

  (define repo "ethereum/tests")
  (define commit "ff69b74f2eb44fbe1bf09627f06ebf69cc72ea67")
  (define directories
    '("ttEIP1559" "ttEIP2930" "ttSignature" "ttVValue" "ttRSValue" "ttWrongRLP" "ttNonce"
      "ttGasLimit" "ttGasPrice" "ttValue" "ttAddress" "ttData"))
  (define skipped-prefix "String10MbData")

  (define gh-path
    (or (find-executable-path "gh")
        (error 'generate-transaction-vectors "gh not found")))

  (define (gh-api . args)
    (define-values (process out in err)
      (apply subprocess #f #f #f gh-path "api" args))
    (close-output-port in)
    (define stdout (port->string out))
    (define stderr (port->string err))
    (subprocess-wait process)
    (close-input-port out)
    (close-input-port err)
    (unless (zero? (subprocess-status process))
      (error 'generate-transaction-vectors "gh api ~a: ~a" (string-join args " ") stderr))
    stdout)

  (define (contents-url path)
    (format "repos/~a/contents/TransactionTests/~a?ref=~a" repo path commit))

  (define (directory-files directory)
    (sort (for/list ([entry (in-list (string->jsexpr (gh-api (contents-url directory))))]
                     #:unless (string-prefix? (hash-ref entry 'name) skipped-prefix))
            (hash-ref entry 'name))
          string<?))

  (define (file-tests directory file)
    (string->jsexpr
     (gh-api "-H" "Accept: application/vnd.github.raw"
             (contents-url (format "~a/~a" directory file)))))

  (define (exception-kinds text)
    (string-join (for/list ([kind (in-list (string-split text "|"))])
                   (regexp-replace #rx"^TransactionException[.]" kind ""))
                 "|"))

  (define (outcome result)
    (match result
      [(hash-table ('sender sender) ('hash hash) _ ...) (list 'sender sender 'hash hash)]
      [(hash-table ('exception text) _ ...) (list 'exception (exception-kinds text))]))

  (define rows
    (for*/list ([directory (in-list directories)]
                [file (in-list (directory-files directory))]
                [(name test) (in-hash (file-tests directory file) #t)])
      (list directory file (symbol->string name) (hash-ref test 'txbytes)
            (outcome (hash-ref (hash-ref test 'result) 'Prague)))))

  (call-with-output-file vectors-path #:exists 'truncate
    (lambda (out)
      (writeln `(source (repo ,repo)
                        (commit ,commit)
                        (path "TransactionTests")
                        (licence "MIT, (c) Ethereum Foundation")
                        (generated-by "racket tests/generate-transaction-vectors.rkt"))
               out)
      (for ([row (in-list rows)])
        (writeln row out)))))
