#lang racket/base

(require racket/contract/base
         racket/match
         (only-in racket/string string-prefix?)
         "private/exn.rkt")

(define halt-table
  '((out-of-gas
     ("OutOfGas" "MemoryOOG" "MemoryLimitOOG" "PrecompileOOG" "InvalidOperandOOG"
      "ReentrancySentryOOG")
     ("out of gas" (detail "out of gas") "contract creation code storage out of gas"
      "gas uint64 overflow" #px"gas required exceeds allowance \\(\\d+\\)"))
    (stack-overflow ("StackOverflow") ("Out of stack" #px"stack limit reached \\d+ \\(\\d+\\)"))
    (stack-underflow
     ("StackUnderflow")
     ("stack underflow" #px"stack underflow \\(\\d+ <=> \\d+\\)"))
    (invalid-opcode ("InvalidFEOpcode") ("invalid opcode: INVALID"))
    (undefined-opcode
     ("OpcodeNotFound" "NotActivated")
     ("invalid opcode" (detail "invalid opcode" (except "INVALID"))))
    (invalid-jump ("InvalidJump") ("invalid jump destination"))
    (return-data-out-of-bounds ("OutOfOffset") ("return data out of bounds"))
    (precompile-failure ("PrecompileError") ("precompiled failed"))
    (write-protection
     ("StateChangeDuringStaticCall" "CallNotAllowedInsideStatic")
     ("write protection"))
    (call-depth ("CallTooDeep") ("max call depth exceeded"))
    (code-size ("CreateContractSizeLimit") ("max code size exceeded"))
    (initcode-size ("CreateInitCodeSizeLimit") ("max initcode size exceeded"))
    (invalid-code ("CreateContractStartingWithEF") ("invalid code: must not begin with 0xef"))
    (address-collision ("CreateCollision") ("contract address collision"))))

(module* internal #f
  (provide halt-table))

(define (wording->predicate wording)
  (match wording
    [(? string?)
     (define whole (string-downcase wording))
     (lambda (words) (string=? words whole))]
    [(list 'detail start (list 'except excepted) ...)
     (define prefix (string-downcase (string-append start ": ")))
     (define refused (map string-downcase excepted))
     (lambda (words)
       (define detail (and (string-prefix? words prefix) (substring words (string-length prefix))))
       (and detail
            (regexp-match? #px"^\\S" detail)
            (not (for/or ([word (in-list refused)]) (string-prefix? detail word)))))]
    [(? pregexp?)
     (define whole (pregexp (format "^(?i:~a)$" (object-name wording))))
     (lambda (words) (regexp-match? whole words))]))

(define-values (halts name-rows wording-rows)
  (for/lists (halts name-rows wording-rows) ([row (in-list halt-table)])
    (match-define (list halt names wordings) row)
    (values halt
            (for/list ([name (in-list names)]) (cons (string-downcase name) halt))
            (for/list ([wording (in-list wordings)]) (cons (wording->predicate wording) halt)))))

(define name-halts (make-immutable-hash (apply append name-rows)))

(define wording-halts (apply append wording-rows))

(define halt/c (flat-named-contract 'halt/c (apply or/c halts)))

(provide halt/c
         (contract-out
          [halts (listof halt/c)]
          [message->halt (-> string? (or/c #f halt/c))]
          [exn->halt (-> exn? (or/c #f halt/c))]))

(define (message->halt message)
  (define words (string-downcase message))
  (match words
    [(regexp #rx"^execution reverted") #f]
    [(pregexp #px"^evm error (\\w+)$" (list _ name)) (hash-ref name-halts name #f)]
    [_ (for/first ([row (in-list wording-halts)]
                   #:when ((car row) words))
         (cdr row))]))

(define (exn->halt e)
  (match e
    [(? exn:fail:web3:rpc:revert?) #f]
    [(struct* exn:fail:web3:rpc ([rpc-message (? string? message)])) (message->halt message)]
    [(struct* exn:fail:web3:transaction ([halt halt])) (and (memq halt halts) halt)]
    [_ #f]))
