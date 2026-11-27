#lang racket/base

(require (only-in web3 integer->address)
         (only-in "programs.rkt" push-address))

(provide (struct-out halting)
         bn254-pairing
         code-halts
         copy-absent-return-data
         creation-halts
         designated-invalid
         grow-the-stack
         jump-to-no-jumpdest
         jump-to-self
         pay-one-wei
         pop-empty-stack
         precompile-halts
         push-zero
         return-0xef
         return-24577-bytes
         return-a-zero-byte
         revert-empty
         static-call-to
         store-at-largest-offset
         store-one
         store-past-memory
         store-past-memory-limit
         undefined-opcode-0c)

(define jump-to-self "5b600056")
(define store-past-memory "6000630100000052")
(define store-past-memory-limit "600064ffffffffff52")
(define store-at-largest-offset
  "60007fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff52")
(define designated-invalid "fe")
(define undefined-opcode-0c "0c")
(define pop-empty-stack "50")
(define grow-the-stack "5b6000600056")
(define jump-to-no-jumpdest "600156")
(define copy-absent-return-data "6001600060003e")
(define push-zero "5f00")
(define revert-empty "60006000fd")
(define store-one "6001600055")
(define pay-one-wei "6000600060006000600160015af100")

(define return-24577-bytes "6160016000f3")
(define return-0xef "60ef60005360016000f3")
(define return-a-zero-byte "600060005360016000f3")

(define bn254-pairing (integer->address 8))

(define nothing-out-nothing-in "6000600060006000")
(define gas-static-call "5afa")
(define stop "00")

(define (static-call-to address)
  (string-append nothing-out-nothing-in (push-address address) gas-static-call stop))

(struct halting (halt target input gas words tracer))

(define (halting-rows rows)
  (for/list ([row (in-list rows)])
    (apply halting row)))

(define code-halts
  (halting-rows
   `((out-of-gas ,jump-to-self #"" 50000 "EVM error OutOfGas" "out of gas")
     (out-of-gas ,store-past-memory #"" 100000 "EVM error MemoryOOG" "out of gas: out of memory")
     (out-of-gas ,store-past-memory-limit #"" 100000 "EVM error MemoryLimitOOG"
                 "out of gas: reach memory limit")
     (out-of-gas ,store-at-largest-offset #"" 100000 "EVM error InvalidOperandOOG"
                 "out of gas: invalid operand")
     (invalid-opcode ,designated-invalid #"" 100000 "EVM error InvalidFEOpcode"
                     "invalid opcode: INVALID")
     (undefined-opcode ,undefined-opcode-0c #"" 100000 "EVM error OpcodeNotFound"
                       "invalid opcode")
     (stack-underflow ,pop-empty-stack #"" 100000 "EVM error StackUnderflow" "stack underflow")
     (stack-overflow ,grow-the-stack #"" 100000 "EVM error StackOverflow" "Out of stack")
     (invalid-jump ,jump-to-no-jumpdest #"" 100000 "EVM error InvalidJump"
                   "invalid jump destination")
     (return-data-out-of-bounds ,copy-absent-return-data #"" 100000 "EVM error OutOfOffset"
                                "return data out of bounds"))))

(define precompile-halts
  (halting-rows
   `((precompile-failure ,bn254-pairing #"\1" 100000 "EVM error PrecompileError"
                         "precompiled failed")
     (out-of-gas ,bn254-pairing #"" 22000 "EVM error PrecompileOOG" "out of gas"))))

(define creation-halts
  (halting-rows
   `((code-size ,return-24577-bytes #f 1000000 "EVM error CreateContractSizeLimit"
                "max code size exceeded")
     (invalid-code ,return-0xef #f 1000000 "EVM error CreateContractStartingWithEF"
                   "invalid code: must not begin with 0xef"))))
