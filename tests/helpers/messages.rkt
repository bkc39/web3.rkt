#lang racket/base

(require (only-in keccak keccak256)
         (only-in "../abi-gen.rkt" random-bytes)
         (only-in "../util.rkt" pick))

(provide edge-messages
         message-bytes
         model-hash-message
         random-message
         random-string-message)

(define edge-messages
  `("" #"" "hello" "Hello World" "héllo" "0xdeadbeef" #"\xde\xad\xbe\xef" "0x" "0xabc"
    "hello " " hello" "a\nb" "\n" "€𝄞" ,(make-string 100 #\a) ,(make-bytes 9 1)
    ,(make-bytes 10 2) ,(make-bytes 99 3) ,(make-bytes 100 4) ,(make-bytes 1000 5)))

(define message-characters
  (append (for/list ([code (in-range 32 127)]) (integer->char code))
          (list #\é #\€ #\𝄞 #\newline)))

(define (random-string-message prng)
  (define middle
    (list->string (for/list ([_ (in-range (random 41 prng))]) (pick prng message-characters))))
  (string-append (pick prng '("" " " "  ")) middle (pick prng '("" " " "\n"))))

(define (random-byte-message prng)
  (define size
    (if (zero? (random 2 prng)) (pick prng '(0 1 9 10 99 100 999 1000)) (random 1101 prng)))
  (random-bytes prng size))

(define (random-message prng)
  (if (zero? (random 2 prng)) (random-byte-message prng) (random-string-message prng)))

(define (message-bytes message)
  (if (string? message) (string->bytes/utf-8 message) message))

(define (model-hash-message message)
  (define body (message-bytes message))
  (keccak256 (bytes-append (bytes #x19)
                           #"Ethereum Signed Message:"
                           (bytes 10)
                           (string->bytes/latin-1 (format "~a" (bytes-length body)))
                           body)))
