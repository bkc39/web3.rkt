#lang racket/base

(require racket/contract/base
         racket/match
         "private/exn.rkt")

;; Each halt, anvil's names after `EVM error`, and the words of call tracers
;; and go-ethereum.  The words are lowercase, as a message is when it is
;; matched; a string is the whole message.
(define halt-table
  '((out-of-gas
     ("OutOfGas" "MemoryOOG" "MemoryLimitOOG" "PrecompileOOG" "InvalidOperandOOG"
      "ReentrancySentryOOG")
     ("out of gas" #px"^out of gas: " "contract creation code storage out of gas"
      "gas uint64 overflow" #px"^gas required exceeds allowance \\(\\d+\\)$"))
    (stack-overflow ("StackOverflow") ("out of stack" #px"^stack limit reached \\d+ \\(\\d+\\)$"))
    (stack-underflow
     ("StackUnderflow")
     ("stack underflow" #px"^stack underflow \\(\\d+ <=> \\d+\\)$"))
    (invalid-opcode ("InvalidFEOpcode") ("invalid opcode: invalid"))
    (undefined-opcode
     ("OpcodeNotFound" "NotActivated")
     ("invalid opcode" #px"^invalid opcode: (?!invalid$)\\S"))
    (invalid-jump ("InvalidJump") ("invalid jump destination"))
    (return-data-out-of-bounds ("OutOfOffset") ("return data out of bounds"))
    (write-protection
     ("StateChangeDuringStaticCall" "CallNotAllowedInsideStatic")
     ("write protection"))
    (call-depth ("CallTooDeep") ("max call depth exceeded"))
    (code-size ("CreateContractSizeLimit") ("max code size exceeded"))
    (initcode-size ("CreateInitCodeSizeLimit") ("max initcode size exceeded"))
    (invalid-code ("CreateContractStartingWithEF") ("invalid code: must not begin with 0xef"))
    (address-collision ("CreateCollision") ("contract address collision"))))

(define halts (map car halt-table))

(define halt/c (flat-named-contract 'halt/c (apply or/c halts)))

(provide halt/c
         (contract-out
          [halts (listof halt/c)]
          [message->halt (-> string? (or/c #f halt/c))]
          [exn->halt (-> exn? (or/c #f halt/c))]))

(define name-halts
  (for*/hash ([row (in-list halt-table)]
              #:do [(match-define (list halt names _) row)]
              [name (in-list names)])
    (values (string-downcase name) halt)))

(define word-halts
  (for*/list ([row (in-list halt-table)]
              #:do [(match-define (list halt _ wordings) row)]
              [wording (in-list wordings)])
    (cons wording halt)))

(define (names? wording words)
  (if (string? wording)
      (string=? wording words)
      (regexp-match? wording words)))

(define (message->halt message)
  (define words (string-downcase message))
  (match words
    [(regexp #rx"^execution reverted") #f]
    [(pregexp #px"^evm error (\\w+)$" (list _ name)) (hash-ref name-halts name #f)]
    [_ (for/first ([row (in-list word-halts)]
                   #:when (names? (car row) words))
         (cdr row))]))

(define (exn->halt e)
  (match e
    [(? exn:fail:web3:rpc:revert?) #f]
    [(struct* exn:fail:web3:rpc ([rpc-message (? string? message)])) (message->halt message)]
    [(struct* exn:fail:web3:transaction ([halt halt])) (and (memq halt halts) halt)]
    [_ #f]))
