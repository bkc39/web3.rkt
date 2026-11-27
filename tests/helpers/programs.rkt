#lang racket/base

(require (only-in web3 hex-string->bytes))

(provide caller-of
         echo-init
         echo-runtime
         push-address)

(define (push-address address)
  (string-append "73" (substring address 2)))

(define (program . fragments)
  (hex-string->bytes (apply string-append "0x" fragments)))

(define store-calldata-word "60003580600052")
(define first-byte-is-0xff "60f81c60ff14")
(define jump-to-revert-word-if "601c57")
(define log-word-under-topic-42 "602a60206000a1")
(define return-word "60206000f3")
(define revert-word "5b60206000fd")
(define return-34-bytes-of-code-after "602280600b6000396000f3")

(define echo-runtime
  (program store-calldata-word first-byte-is-0xff jump-to-revert-word-if log-word-under-topic-42
           return-word revert-word))

(define echo-init (bytes-append (program return-34-bytes-of-code-after) echo-runtime))

(define copy-calldata "366000600037")
(define word-out-calldata-in "60206000366000")
(define no-value "6000")
(define gas-call "5af1")
(define pop-success "50")
(define copy-returndata "3d600060003e")
(define return-returndata "3d6000f3")

(define (caller-of callee)
  (string-append copy-calldata word-out-calldata-in no-value (push-address callee) gas-call
                 pop-success copy-returndata return-returndata))
