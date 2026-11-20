#lang racket/base

(require web3)

(define calls
  `(("transfer(address, uint)" "0x70997970C51812dc3A010C7d01b50e0d17dc79C8" ,(ether->wei 1))
    ("submit(string,uint256[])" "hello" (1 2))))

(define (print-call signature . arguments)
  (define data (apply encode-call signature arguments))
  (printf "~a\n" (abi-signature->string (string->abi-signature signature)))
  (printf "  selector ~a\n" (bytes->hex-string (function-selector signature)))
  (for ([word (in-list (bytes->words (subbytes data 4)))])
    (printf "  ~a\n" (bytes->hex-string (word->bytes word)))))

(module+ main
  (for ([call (in-list calls)])
    (apply print-call call)))
