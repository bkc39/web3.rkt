#lang racket/base

(module+ test
  (require (only-in racket/contract exn:fail:contract:blame?)
           racket/list
           racket/match
           rackunit
           (only-in libsecp256k1 integer->secret-key)
           (only-in scribble/example make-base-eval)
           web3
           (only-in (submod web3/signer internal)
                    signer-reason/c
                    signer-sign-transaction/who)
           "helpers/accounts.rkt"
           (only-in "helpers/secrets.rkt" check-no-leak marker marker-hex marker-integer)
           "helpers/transactions.rkt"
           "modules.rkt"
           (only-in "util.rkt"
                    anvil-alice anvil-bob check-blames-here raised raised-or-value seeded))

  (match-define (list key0 key1 _ ...) (map hex-string->secret-key anvil-keys))
  (define lowercase-alice (string-downcase anvil-alice))
  (define hello-signature (sign-message key0 "hello"))
  (define example-signed (sign-transaction key0 example-eip1559))

  (define (raw-hex stx)
    (bytes->hex-string (signed-transaction->bytes stx)))

  (test-case "G1: addresses, printed forms and predicates"
    (define local (local-signer key0))
    (define node (node-signer lowercase-alice))
    (check-equal? (signer-address local) (secret-key->address key0))
    (check-equal? (signer-address node) anvil-alice)
    (for ([format-string (in-list '("~a" "~s" "~v"))])
      (check-equal? (format format-string local) (format "#<local-signer ~a>" anvil-alice))
      (check-equal? (format format-string node) (format "#<node-signer ~a>" anvil-alice)))
    (check-equal? (map signer? (list local node 5 key0 anvil-alice)) '(#t #t #f #f #f))
    (check-equal? (map local-signer? (list local node 5)) '(#t #f #f))
    (check-equal? (map node-signer? (list local node 5)) '(#f #t #f))
    (check-false (equal? (local-signer key0) (local-signer key0))))

  (test-case "G2: a local signer signs as sign-digest, sign-message and sign-transaction"
    (define prng (seeded 13 4 2 8 9 1))
    (for* ([_ (in-range 50)]
           [kind (in-list '(pre-155 legacy eip2930 eip1559))])
      (define key (random-key prng))
      (define signer (local-signer key))
      (define digest (random-digest prng))
      (define text (format "message ~a" (random 1000 prng)))
      (define tx (random-transaction prng kind #:fee-bytes 8))
      (check-equal? (signer-sign-digest signer digest) (sign-digest key digest))
      (check-equal? (signer-sign-message signer text) (sign-message key text))
      (check-equal? (signer-sign-message signer (string->bytes/utf-8 text)) (sign-message key text))
      (check-equal? (signer-sign-transaction signer tx) (sign-transaction key tx))))

  (struct recording (secret address behaviour calls) #:transparent)

  (define (call! signer name . arguments)
    (match-define (recording _ address behaviour calls) signer)
    (set-box! calls (cons name (unbox calls)))
    (match name
      ['address address]
      [_ (apply (hash-ref behaviour name) arguments)]))

  (struct address-only recording ()
    #:transparent
    #:methods gen:signer
    [(define (signer-address signer) (call! signer 'address))])

  (struct digest-signer recording ()
    #:transparent
    #:methods gen:signer
    [(define (signer-address signer) (call! signer 'address))
     (define (signer-sign-digest signer digest) (call! signer 'digest digest))])

  (struct message-signer recording ()
    #:transparent
    #:methods gen:signer
    [(define (signer-address signer) (call! signer 'address))
     (define (signer-sign-message signer message) (call! signer 'message message))])

  (struct transaction-signer recording ()
    #:transparent
    #:methods gen:signer
    [(define (signer-address signer) (call! signer 'address))
     (define (signer-sign-transaction signer tx) (call! signer 'transaction tx))])

  (struct full-signer recording ()
    #:transparent
    #:methods gen:signer
    [(define (signer-address signer) (call! signer 'address))
     (define (signer-sign-digest signer digest) (call! signer 'digest digest))
     (define (signer-sign-message signer message) (call! signer 'message message))
     (define (signer-sign-transaction signer tx) (call! signer 'transaction tx))])

  (define kinds
    (hasheq 'address-only address-only 'digest digest-signer 'message message-signer
            'transaction transaction-signer 'full full-signer))

  (define evil-digest (hash-message "evil"))

  (define rpc-failure
    (with-handlers ([exn:fail:web3:rpc? values])
      (raise-rpc-error "eth_sign" -32000 "vault offline")))

  (define behaviours
    (hasheq 'honest (hasheq 'digest (lambda (digest) (sign-digest key0 digest))
                            'message (lambda (message) (sign-message key0 message))
                            'transaction (lambda (tx) (sign-transaction key0 tx)))
            '65-bytes (hasheq 'digest (lambda (_) (make-bytes 65 1)))
            'unsigned (hasheq 'transaction values)
            'high-s (hasheq 'digest (lambda (digest) (other-s (sign-digest key0 digest))))
            'failing (hasheq 'digest (lambda (_) (raise rpc-failure)))
            'mutating (hasheq 'digest (lambda (digest)
                                        (bytes-copy! digest 0 evil-digest)
                                        (sign-digest key0 digest))
                              'message (lambda (message)
                                         (bytes-fill! message (char->integer #\x))
                                         (sign-message key0 message)))))

  (define (custom kind address behaviour)
    ((hash-ref kinds kind) marker address (hash-ref behaviours behaviour) (box '())))

  (define (run-on operation signer digest message)
    (match operation
      ['address (signer-address signer)]
      ['digest (signer-sign-digest signer digest)]
      ['message (signer-sign-message signer message)]
      ['transaction (signer-sign-transaction signer example-eip1559)]))

  (define (run operation signer)
    (run-on operation signer digest-hello "hello"))

  (test-case "a custom signer's address is respelt in EIP-55, whatever its case"
    (for ([spelling (list (string-downcase anvil-alice)
                          (string-append "0x" (string-upcase (substring anvil-alice 2)))
                          "0xF39Fd6e51aad88F6F4ce6aB8827279cffFb92266")])
      (define signer (custom 'digest spelling 'honest))
      (check-equal? (signer-address signer) anvil-alice spelling)
      (check-equal? (signer-sign-message signer "hello") hello-signature spelling)))

  (define custom-refusals
    `((address-only ,anvil-alice honest digest
                    "signer-sign-digest: not supported: the signer signs no digests")
      (address-only ,anvil-alice honest message
                    ,(string-append "signer-sign-message: not supported: "
                                    "the signer signs neither messages nor digests"))
      (address-only ,anvil-alice honest transaction
                    ,(string-append "signer-sign-transaction: not supported: "
                                    "the signer signs neither transactions nor digests"))
      (message ,anvil-alice honest digest
               "signer-sign-digest: not supported: the signer signs no digests")
      (message ,anvil-alice honest transaction
               ,(string-append "signer-sign-transaction: not supported: "
                               "the signer signs neither transactions nor digests"))
      (digest "0x12" honest address
              ,(string-append "signer-address: malformed result: "
                              "the signer's address is a string of 4 characters, not an address"))
      (digest "0x12" honest digest
              ,(string-append "signer-sign-digest: malformed result: "
                              "the signer's address is a string of 4 characters, not an address"))
      (address-only "0x12" honest message
                    ,(string-append "signer-sign-message: not supported: "
                                    "the signer signs neither messages nor digests"))
      (digest ,anvil-alice 65-bytes digest
              ,(string-append "signer-sign-digest: malformed result: "
                              "the signer returned a byte string of length 65, not a signature"))
      (transaction ,anvil-alice unsigned transaction
                   ,(string-append "signer-sign-transaction: malformed result: "
                                   "the signer returned a value of another kind, "
                                   "not a signed transaction"))
      (digest ,anvil-bob honest message
              ,(string-append "signer-sign-message: wrong signer: the signature recovers "
                              "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266, not the signer's "
                              "address 0x70997970C51812dc3A010C7d01b50e0d17dc79C8"))
      (digest ,anvil-alice high-s digest
              "signer-sign-digest: high s: s is more than half the curve order (EIP-2)")
      (digest ,anvil-alice high-s transaction
              "signer-sign-transaction: high s: s is more than half the curve order (EIP-2)")))

  (define preferences
    '((digest (digest address)) (message (message address)) (transaction (transaction address))))

  (define (custom-refusal row)
    (match-define (list kind address behaviour operation _) row)
    (define signer (custom kind address behaviour))
    (values signer (raised (lambda () (run operation signer)))))

  (test-case "G3: a message method receives bytes: the UTF-8 of a string, or the bytes given"
    (for ([message (in-list (list "héllo" #"\xde\xad"))])
      (define seen (box #f))
      (define signer
        (message-signer marker anvil-alice
                        (hasheq 'message (lambda (bs) (set-box! seen bs) (sign-message key0 bs)))
                        (box '())))
      (check-equal? (signer-sign-message signer message) (sign-message key0 message))
      (check-equal? (unbox seen)
                    (if (string? message) (string->bytes/utf-8 message) message))))

  (test-case "G9: custom signers: fallbacks, refusals in order, each method called once"
    (define vault (custom 'digest anvil-alice 'honest))
    (check-equal? (signer-sign-message vault "hello") hello-signature)
    (check-equal? (signer-sign-transaction vault example-eip1559) example-signed)
    (check-equal? (unbox (recording-calls vault)) '(digest address digest address))
    (define messenger (custom 'message anvil-alice 'honest))
    (check-equal? (signer-sign-message messenger "hello") hello-signature)
    (check-equal? (unbox (recording-calls messenger)) '(message address))
    (define notary (custom 'transaction anvil-alice 'honest))
    (check-equal? (signer-sign-transaction notary example-eip1559) example-signed)
    (check-equal? (unbox (recording-calls notary)) '(transaction address))
    (for ([row (in-list custom-refusals)])
      (match-define (list kind address _ operation expected) row)
      (define-values (signer e) (custom-refusal row))
      (check-pred exn:fail:web3:signer? e (format "~s" row))
      (when (exn:fail:web3:signer? e)
        (check-equal? (exn-message e) expected)))
    (for ([operation (in-list '(digest message transaction))])
      (define signer (custom 'address-only anvil-alice 'honest))
      (raised (lambda () (run operation signer)))
      (check-equal? (unbox (recording-calls signer)) '() (format "~a" operation)))
    (check-eq? (raised (lambda () (run 'digest (custom 'digest anvil-alice 'failing))))
               rpc-failure)
    (for ([row (in-list preferences)])
      (match-define (list operation expected) row)
      (define signer (custom 'full anvil-alice 'honest))
      (run operation signer)
      (check-equal? (unbox (recording-calls signer)) expected (format "~a" operation))))

  (define mutations
    '((digest digest) (digest message) (digest transaction) (message message)))

  (test-case "G14: a method that changes what it is given signs nothing the caller asked for"
    (for ([row (in-list mutations)])
      (match-define (list kind operation) row)
      (define digest (bytes-copy digest-hello))
      (define message (bytes-copy #"hello"))
      (define e
        (raised (lambda () (run-on operation (custom kind anvil-alice 'mutating) digest message))))
      (define name (format "~s" row))
      (check-pred exn:fail:web3:signer? e name)
      (when (exn:fail:web3:signer? e)
        (check-eq? (exn:fail:web3:signer-reason e) 'wrong-signer name))
      (check-equal? digest digest-hello name)
      (check-equal? message #"hello" name)))

  (test-case "G15: a method receives a mutable copy of the caller's bytes"
    (for* ([row (in-list '((digest digest) (message message)))]
           [given (in-list (list (bytes-copy #"hello") #"hello"))])
      (match-define (list kind operation) row)
      (define seen (box #f))
      (define honest (hash-ref (hash-ref behaviours 'honest) kind))
      (define signer
        ((hash-ref kinds kind) marker anvil-alice
                               (hasheq kind (lambda (bs) (set-box! seen bs) (honest bs)))
                               (box '())))
      (define digest (if (immutable? given) (hash-message given) (bytes-copy digest-hello)))
      (run-on operation signer digest given)
      (define passed (if (eq? operation 'digest) digest given))
      (define name (format "~s ~s" row given))
      (check-equal? (unbox seen) passed name)
      (check-false (eq? (unbox seen) passed) name)
      (check-false (immutable? (unbox seen)) name)))

  (test-case "G16: nothing a transaction method receives can be changed"
    (for ([tx (in-list example-transactions)])
      (check-true (immutable? (transaction-data tx)))
      (check-true (immutable? (transaction-to tx)))
      (for ([entry (in-list (if (legacy-transaction? tx) '() (transaction-access-list tx)))])
        (match-define (cons address keys) entry)
        (check-true (immutable? address))
        (for ([key (in-list keys)])
          (check-true (immutable? key))))))

  (struct fickle (answers)
    #:methods gen:signer
    [(define (signer-address signer)
       (match-define (cons answer more) (unbox (fickle-answers signer)))
       (set-box! (fickle-answers signer) more)
       answer)
     (define (signer-sign-digest signer digest) (sign-digest key0 digest))
     (define (signer-sign-transaction signer tx) (sign-transaction key0 tx))])

  (define fickle-rows
    `(((,anvil-alice ,anvil-bob) ok) ((,anvil-bob ,anvil-alice) wrong-signer)))

  (test-case "G17: the address is read once per operation, and checked as read"
    (for* ([row (in-list fickle-rows)]
           [operation (in-list '(digest message transaction))])
      (match-define (list answers expected) row)
      (define signer (fickle (box answers)))
      (define outcome
        (match (raised (lambda () (run operation signer)))
          [#f 'ok]
          [(? exn:fail:web3:signer? e) (exn:fail:web3:signer-reason e)]
          [e e]))
      (check-equal? outcome expected (format "~s ~a" row operation))
      (check-equal? (unbox (fickle-answers signer)) (rest answers))))

  (test-case "G10: no refusal shows the signer"
    (for ([row (in-list custom-refusals)])
      (define-values (_ e) (custom-refusal row))
      (check-no-leak (exn-message e) (format "~s" row))))

  (define ((sign-answer form) params)
    (match-define (list _ data) params)
    (define encoded (signature->bytes (sign-message key0 (hex-string->bytes data))))
    (bytes->hex-string
     (match form
       ['electrum encoded]
       ['parity (bytes-append (subbytes encoded 0 64) (bytes (- (bytes-ref encoded 64) 27)))])))

  (define (node-with handler)
    (define provider (mock-provider handler))
    (values (node-signer lowercase-alice #:provider provider) provider))

  (test-case "G5: the requests a node signer sends"
    (define-values (signer provider)
      (node-with (hash "eth_sign" (sign-answer 'electrum)
                       "eth_signTransaction" (raw-hex example-signed))))
    (signer-address signer)
    (raised (lambda () (signer-sign-digest signer digest-hello)))
    (check-equal? (mock-provider-calls provider) '())
    (signer-sign-message signer "hello")
    (check-equal? (mock-provider-calls provider) `(("eth_sign" ,anvil-alice "0x68656c6c6f")))
    (signer-sign-transaction signer example-eip1559)
    (check-equal? (rest (mock-provider-calls provider))
                  `(("eth_signTransaction"
                     ,(transaction->jsexpr example-eip1559 #:from anvil-alice)))))

  (test-case "G6: an honest node's answers, in each form"
    (for ([form (in-list '(electrum parity))])
      (define-values (signer _) (node-with (hash "eth_sign" (sign-answer form))))
      (for ([message (in-list '("hello" "" #"\xde\xad\xbe\xef"))])
        (check-equal? (signer-sign-message signer message) (sign-message key0 message))))
    (for ([tx (in-list (cons (transaction-of 'pre-155 (hasheq)) example-transactions))])
      (define stx (sign-transaction key0 tx))
      (for ([answer (list (raw-hex stx) (hasheq 'raw (raw-hex stx) 'tx (hasheq)))])
        (define-values (signer _) (node-with (hash "eth_signTransaction" answer)))
        (check-equal? (signer-sign-transaction signer tx) stx))))

  (define example-pre-155
    (transaction-of 'pre-155 (transaction-fields example-legacy)))

  (define (signature-with-v sig v)
    (bytes->hex-string (bytes-append (subbytes (signature->bytes sig) 0 64) (bytes v))))

  (define raw-refusals
    `((message "hello" ,(signature-hex (sign-message key1 "hello")) (signer wrong-signer))
      (message "hello" ,(bytes->hex-string (make-bytes 64 1)) (signer malformed))
      (message "hello" "0xabc" (signer malformed))
      (message "hello" ,(signature-with-v hello-signature 37)
               (other ,exn:fail:web3:signature? "^signer-sign-message: invalid signature"))
      (message "hello" ,(signature-hex (other-s hello-signature)) (signer high-s))
      (message "hello" 42 (other ,exn:fail:web3? "^eth_sign: unexpected result"))
      (transaction ,example-eip1559
                   ,(raw-hex (sign-transaction
                              key0 (transaction-of 'eip1559 (hash-set (transaction-fields
                                                                       example-eip1559)
                                                                      'nonce 1))))
                   (signer wrong-transaction "the signed transaction differs in nonce$"))
      (transaction ,example-pre-155 ,(raw-hex (sign-transaction key0 example-legacy))
                   (signer wrong-transaction "the signed transaction differs in chain-id$"))
      (transaction ,example-eip1559 ,(raw-hex (sign-transaction key1 example-eip1559))
                   (signer wrong-signer
                           ,(string-append "the transaction's sender is "
                                           "0x70997970C51812dc3A010C7d01b50e0d17dc79C8, not the "
                                           "signer's address "
                                           "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266$")))
      (transaction ,example-eip1559 ,(substring (raw-hex example-signed) 0 100)
                   (other ,exn:fail:web3:rlp? "^signer-sign-transaction: "))
      (transaction ,example-eip1559 "0xabc" (signer malformed))
      (transaction ,example-eip1559 (5) (other ,exn:fail:web3? "^eth_signTransaction: unexpected"))
      (transaction ,example-eip1559 "0x03"
                   (other ,(lambda (e) (and (exn:fail:web3:raw-transaction? e)
                                            (eq? (exn:fail:web3:raw-transaction-reason e) 'type)))
                          "^signer-sign-transaction: not a transaction"))))

  (define methods '((message "eth_sign") (transaction "eth_signTransaction")))

  (define (node-refusal row)
    (match-define (list operation argument answer _) row)
    (match-define (list _ method) (assq operation methods))
    (define-values (signer _provider) (node-with (hash method answer)))
    (raised (lambda ()
              (match operation
                ['message (signer-sign-message signer argument)]
                ['transaction (signer-sign-transaction signer argument)]))))

  (define (check-refusal row e)
    (match-define (list operation _ _ expected) row)
    (define name (format "~s" row))
    (define who (format "^signer-sign-~a: " operation))
    (match expected
      [(list 'signer reason detail ...)
       (check-pred exn:fail:web3:signer? e name)
       (when (exn:fail:web3:signer? e)
         (check-equal? (exn:fail:web3:signer-reason e) reason name)
         (check-regexp-match (pregexp who) (exn-message e) name)
         (for ([pattern (in-list detail)])
           (check-regexp-match (pregexp pattern) (exn-message e) name)))]
      [(list 'other kind? pattern)
       (check-true (and e (kind? e)) name)
       (when e
         (check-regexp-match (pregexp pattern) (exn-message e) name))]))

  (test-case "G7: what a node signer refuses"
    (for ([row (in-list raw-refusals)])
      (check-refusal row (node-refusal row)))
    (define-values (signer _)
      (node-with (hash "eth_signTransaction"
                       (lambda (_) (raise-rpc-error "eth_signTransaction" -32601
                                                    "Method not found")))))
    (define e (raised (lambda () (signer-sign-transaction signer example-eip1559))))
    (check-pred exn:fail:web3:rpc? e)
    (check-equal? (exn:fail:web3:rpc-method e) "eth_signTransaction")
    (check-equal? (exn:fail:web3:rpc-code e) -32601)
    (define unbound (node-signer anvil-alice))
    (define orphan
      (parameterize ([current-provider #f])
        (raised (lambda () (signer-sign-message unbound "hello")))))
    (check-pred exn:fail:web3? orphan)
    (check-regexp-match #rx"^eth_sign: no provider" (exn-message orphan)))

  (test-case "G8: the signer's own provider wins; otherwise the one current at the call"
    (define own (mock-provider (hash "eth_sign" (sign-answer 'electrum))))
    (define first-current (mock-provider (hash "eth_sign" (sign-answer 'electrum))))
    (define second-current (mock-provider (hash "eth_sign" (sign-answer 'electrum))))
    (define bound (node-signer anvil-alice #:provider own))
    (define unbound (parameterize ([current-provider first-current]) (node-signer anvil-alice)))
    (parameterize ([current-provider second-current])
      (signer-sign-message bound "hello")
      (signer-sign-message unbound "hello"))
    (check-equal? (length (mock-provider-calls own)) 1)
    (check-equal? (mock-provider-calls first-current) '())
    (check-equal? (length (mock-provider-calls second-current)) 1))

  (define signer-errors
    (append (for/list ([row (in-list custom-refusals)])
              (define-values (_ e) (custom-refusal row))
              e)
            (filter exn:fail:web3:signer? (map node-refusal raw-refusals))))

  (test-case "G11: signer errors: reasons, message shape, the caller's who"
    (check-true (pair? signer-errors))
    (for ([e (in-list signer-errors)])
      (check-true (exn:fail:web3? e))
      (check-true (signer-reason/c (exn:fail:web3:signer-reason e)))
      (check-regexp-match
       (pregexp (string-append "^(signer-address|signer-sign-digest|signer-sign-message"
                               "|signer-sign-transaction): (not supported|malformed result"
                               "|wrong signer|wrong transaction|high s): "))
       (exn-message e))
      (check-true (< (string-length (exn-message e)) 200) (exn-message e)))
    (for ([signer (list (custom 'address-only anvil-alice 'honest)
                        (custom 'digest "0x12" 'honest)
                        (custom 'digest anvil-alice 'high-s)
                        (custom 'transaction anvil-alice 'unsigned))])
      (define e (raised (lambda ()
                          (signer-sign-transaction/who 'contract-send signer example-eip1559))))
      (check-pred exn:fail:web3:signer? e)
      (check-regexp-match #rx"^contract-send: " (exn-message e)))
    (define-values (node _)
      (node-with (hash "eth_signTransaction" (substring (raw-hex example-signed) 0 100))))
    (check-regexp-match #rx"^contract-send: "
                        (exn-message
                         (raised (lambda ()
                                   (signer-sign-transaction/who 'contract-send node
                                                                example-eip1559))))))

  (define marker-key (integer->secret-key marker-integer))

  (define blamed
    `((,(lambda () (local-signer marker-hex)) "a secret key")
      (,(lambda () (local-signer)) "1 argument")
      (,(lambda () (signer-sign-message marker-hex "m")) "a signer")
      (,(lambda () (signer-sign-message (local-signer marker-key) 'm)) "a byte string or a string")
      (,(lambda () (signer-sign-digest (local-signer marker-key) (make-bytes 31))) "a 32-byte digest")
      (,(lambda () (signer-sign-transaction (local-signer marker-key) 'tx)) "a transaction")
      (,(lambda () (signer-sign-transaction (local-signer marker-key) example-signed))
       "a transaction")
      (,(lambda () (signer? 1 2)) "1 argument")
      (,(lambda () (node-signer "0x12")) "an address")
      (,(lambda () (node-signer "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD")) "an address")
      (,(lambda () (node-signer anvil-alice #:provider "http://x")) "provider")
      (,(lambda () (node-signer anvil-alice #:chain-id 1)) "keyword #:provider")))

  (test-case "G4: wrong arguments blame the caller, name their kind and show no key"
    (define signer (local-signer marker-key))
    (for ([format-string (in-list '("~a" "~s" "~v"))])
      (check-no-leak (format format-string signer)))
    (for ([row (in-list blamed)])
      (match-define (list call expected) row)
      (check-blames-here (call))
      (define message (with-handlers ([exn:fail? exn-message]) (call)))
      (check-regexp-match (regexp-quote expected) message)
      (check-no-leak message)))

  (define bad-inputs
    (list 0 -1 5 (expt 2 256) 1.5 "" "0x" "0x12" "hello" marker-hex lowercase-alice
          #"" (make-bytes 20 1) (make-bytes 31 1) (make-bytes 33 1) (make-bytes 65 1) 'm #f #t
          (void) '() (list digest-hello) (vector 1) (hasheq) (hasheq 'raw "0x") key0
          (secret-key->public-key key0) hello-signature (other-s hello-signature)
          example-eip1559 example-signed (signed-transaction->bytes example-signed)
          (node-signer anvil-alice) (local-signer key1) (mock-provider (hash)) values
          (box 1) #\a (string->path "/tmp") (make-string 132 #\0)))

  (define surface
    `((,signer? ,(local-signer key0)) (,signer-address ,(local-signer key0))
      (,signer-sign-digest ,(local-signer key0) ,digest-hello)
      (,signer-sign-message ,(local-signer key0) "hello")
      (,signer-sign-transaction ,(local-signer key0) ,example-eip1559)
      (,local-signer ,key0) (,local-signer? ,key0) (,node-signer ,anvil-alice)
      (,node-signer? ,anvil-alice)))

  (define (acceptable? e)
    (or (not e) (exn:fail:contract:blame? e) (exn:fail:web3? e)))

  (test-case "G12: bad inputs raise only blame errors and web3 errors"
    (check-equal? (length bad-inputs) 40)
    (parameterize ([current-provider (mock-provider (hash))])
      (for* ([entry (in-list surface)]
             [position (in-range (length (rest entry)))]
             [input (in-list bad-inputs)])
        (match-define (cons procedure good) entry)
        (define e (raised (lambda () (apply procedure (list-set good position input)))))
        (check-true (acceptable? e) (format "~a ~a ~a" (object-name procedure) position e)))))

  (define signer-names
    '(gen:signer signer? signer-address signer-sign-digest signer-sign-message
                 signer-sign-transaction local-signer local-signer? node-signer node-signer?
                 exn:fail:web3:signer exn:fail:web3:signer? exn:fail:web3:signer-reason
                 struct:exn:fail:web3:signer))

  (test-case "G13: web3 re-exports web3/signer, and requires without conflicts"
    (define all (exported-names 'web3))
    (for ([name (in-list signer-names)])
      (check-not-false (memq name all) (format "~a" name)))
    (check-equal? (sort (exported-names 'web3/signer) symbol<?) (sort signer-names symbol<?))
    (define ev (make-base-eval))
    (check-not-exn (lambda () (ev '(require web3))))
    (check-not-exn (lambda () (ev '(require web3 libsecp256k1))))
    (define signer-only (make-base-eval '(require web3/signer web3/account)))
    (check-equal?
     (signer-only
      `(let ()
         (struct vault (key)
           #:methods gen:signer
           [(define (signer-address v) (secret-key->address (vault-key v)))
            (define (signer-sign-digest v digest) (sign-digest (vault-key v) digest))])
         (define key (hex-string->secret-key ,(first anvil-keys)))
         (equal? (signer-sign-message (vault key) "hello") (sign-message key "hello"))))
     #t))

  (test-case "P1: eth-sign sends the address and data, and checks the answer"
    (define provider (mock-provider (hash "eth_sign" "0xab")))
    (check-equal? (eth-sign anvil-alice "0x68" #:provider provider) "0xab")
    (check-equal? (mock-provider-calls provider) `(("eth_sign" ,anvil-alice "0x68")))
    (check-exn exn:fail:web3?
               (lambda () (eth-sign anvil-alice "0x68" #:provider (mock-provider (hash "eth_sign" 5)))))
    (check-exn exn:fail:contract? (lambda () (eth-sign 5 "0x" #:provider provider))))

  (test-case "P2: eth-sign-transaction takes a string or an object's raw"
    (define tx (transaction->jsexpr example-eip1559 #:from anvil-alice))
    (define (answer result)
      (raised-or-value
       (lambda ()
         (eth-sign-transaction tx #:provider (mock-provider (hash "eth_signTransaction" result))))))
    (check-equal? (answer "0x02ab") "0x02ab")
    (check-equal? (answer (hasheq 'raw "0x02ab" 'tx (hasheq))) "0x02ab")
    (for ([result (list (hasheq 'tx (hasheq)) (hasheq 'raw 5) 5 'null)])
      (define e (answer result))
      (check-pred exn:fail:web3? e)
      (when (exn? e)
        (check-regexp-match #rx"^eth_signTransaction: unexpected result " (exn-message e))))))
