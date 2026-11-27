#lang racket/base

(require (only-in racket/format ~r)
         (only-in web3 hex-string->bytes))

(provide caller-of
         calls-itself
         echo-init
         echo-runtime
         push-address)

(define (push-address address)
  (string-append "73" (substring address 2)))

(define (program . fragments)
  (hex-string->bytes (apply string-append "0x" fragments)))

(define (fragments-length fragments)
  (for/sum ([fragment (in-list fragments)])
    (quotient (string-length fragment) 2)))

(define jump-if-length 3)

(define (jump-if-to offset)
  (string-append "60" (~r offset #:base 16 #:min-width 2 #:pad-string "0") "57"))

(define store-calldata-word "60003580600052")
(define first-byte-is-0xff "60f81c60ff14")
(define log-word-under-topic-42 "602a60206000a1")
(define return-word "60206000f3")
(define jumpdest "5b")
(define revert-word "60206000fd")
(define return-34-bytes-of-code-after "602280600b6000396000f3")

(define echo-runtime
  (let ([before-jump (list store-calldata-word first-byte-is-0xff)]
        [after-jump (list log-word-under-topic-42 return-word)])
    (apply program
           (append before-jump
                   (list (jump-if-to (+ (fragments-length before-jump)
                                        jump-if-length
                                        (fragments-length after-jump))))
                   after-jump
                   (list jumpdest revert-word)))))

(define echo-init (bytes-append (program return-34-bytes-of-code-after) echo-runtime))

(define copy-calldata "366000600037")
(define word-out-calldata-in "60206000366000")
(define nothing-out-nothing-in "6000600060006000")
(define no-value "6000")
(define push-self "30")
(define gas-call "5af1")
(define pop-success "50")
(define stop "00")
(define copy-returndata "3d600060003e")
(define return-returndata "3d6000f3")

(define (caller-of callee)
  (string-append copy-calldata word-out-calldata-in no-value (push-address callee) gas-call
                 pop-success copy-returndata return-returndata))

(define calls-itself
  (string-append nothing-out-nothing-in no-value push-self gas-call stop))
