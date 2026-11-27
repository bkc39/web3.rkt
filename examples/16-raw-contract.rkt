#lang racket/base

(require web3)

(define key
  (hex-string->secret-key "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))
(define signer (local-signer key))

(define echo-runtime
  (hex-string->bytes "0x6000358060005260f81c60ff14601c57602a60206000a160206000f35b60206000fd"))
(define creation-prefix (hex-string->bytes "0x602280600b6000396000f3"))
(define echo-init (bytes-append creation-prefix echo-runtime))
(define invalid-opcode (hex-string->bytes "0xfe"))

(define word (word->bytes 42))
(define refused (bytes-append (bytes #xff) (make-bytes 30 0) (bytes 42)))
(define broken (integer->address #xbad))

(module+ main
  (with-anvil (#:args '("--accounts" "0"))
    (anvil-set-balance (signer-address signer) (ether->wei 10))
    (define echo (receipt-contract-address (send-transaction/receipt signer #f #:data echo-init)))
    (printf "deployed ~a\n" echo)
    (printf "call ~a\n" (bytes->hex-string (call-transaction signer echo #:data word)))
    (define revert
      (with-handlers ([exn:fail:web3:rpc:revert? exn->abi-revert])
        (call-transaction signer echo #:data refused)))
    (printf "revert ~a\n" (bytes->hex-string (abi-revert-data revert)))
    (anvil-set-code broken invalid-opcode)
    (define halt
      (with-handlers ([exn:fail:web3:rpc? exn->halt])
        (call-transaction signer broken)))
    (printf "halt ~a\n" halt)
    (define receipt (send-transaction/receipt signer echo #:data word))
    (define trace (transaction-call-trace (receipt-transaction-hash receipt)))
    (printf "trace ~a output ~a gas used ~a\n"
            (call-trace-type trace)
            (bytes->hex-string (call-trace-output trace))
            (call-trace-gas-used trace))
    (printf "gas estimate ~a intrinsic ~a floor ~a used ~a\n"
            (estimate-transaction signer echo #:data word)
            (intrinsic-gas word)
            (floor-gas word)
            (receipt-gas-used receipt))))
