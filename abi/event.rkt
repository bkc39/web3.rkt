#lang racket/base

(require racket/contract/base
         (only-in keccak keccak256)
         (only-in racket/list check-duplicates count dropf-right filter-not remove-duplicates)
         (only-in racket/match == match match* match-define struct*)
         (only-in racket/string string-join)
         "../abi.rkt"
         (submod "../abi.rkt" internal)
         (submod "event-type.rkt" private)
         "type.rkt"
         (submod "type.rkt" private)
         "../eth/log.rkt"
         (only-in (submod "../eth/log.rkt" internal) log->list log-struct log-tail)
         (only-in "../private/abi-decode.rkt" raise-abi-decode-error)
         "../private/exn.rkt"
         (only-in "../private/argument.rkt" hash-argument/c one-or-more/c)
         (only-in "../private/field-contracts.rkt" hash-string/c)
         (only-in "../private/hex.rkt" bytes->hex hex->bytes)
         "../private/who.rkt")

(provide
 (contract-out
  [event-topic (-> abi-event-spec/c (or/c #f hash-string/c))]
  [abi-value->topic (-> abi-type-spec/c any/c hash-string/c)]
  [event-topics (->i ([event abi-event-spec/c])
                     ([where (event) (event-where/c 'event-topics event)])
                     [_ topic-positions/c])]
  [any-of (->* () #:rest (and/c pair? list?) any-of?)]
  [any-of? (-> any/c boolean?)]
  [any-of-values (-> any-of? (non-empty-listof any/c))]
  (struct indexed-hash ([topic hash-argument/c]))
  [decode-log (-> abi-event-spec/c log? event-log?)]
  [decode-logs (-> (listof abi-event-spec/c) (listof log?) (listof log?))]
  [event-log? (-> any/c boolean?)]
  [event-log-event (-> event-log? abi-event?)]
  [event-log-name (-> event-log? string?)]
  [event-log-arguments (-> event-log? list?)]
  [event-log-ref (->i ([el event-log?] [key (el) (event-log-key/c el)]) [_ any/c])]))

(define topic-positions/c
  (rename-contract (and/c (listof (or/c #f (one-or-more/c hash-argument/c
                                                          "a hash or a non-empty list of hashes")))
                          (property/c length (<=/c 4)))
                   'topic-positions/c))

;; The index of the parameter key names, by name or position, or #f.
(define (parameter-index event key)
  (define ps (abi-event-parameters event))
  (cond
    [(exact-nonnegative-integer? key) (and (< key (length ps)) key)]
    [(or (symbol? key) (string? key))
     (define name (if (symbol? key) (symbol->string key) key))
     (for/first ([p (in-list ps)]
                 [i (in-naturals)]
                 #:when (equal? (abi-event-parameter-name p) name))
       i)]
    [else #f]))

(define (where-indices event where)
  (for/list ([key (in-hash-keys where)])
    (parameter-index event key)))

(define (indexed-indices? event indices)
  (define ps (abi-event-parameters event))
  (and (andmap (lambda (i) (and i (abi-event-parameter-indexed? (list-ref ps i)))) indices)
       (not (check-duplicates indices))))

(define (event-where/c who spec)
  (define event (spec->abi-event/who who spec))
  (flat-named-contract
   `(event-where/c ,(string->symbol (abi-event-name event)))
   (lambda (where)
     (and (hash? where) (indexed-indices? event (where-indices event where))))))

(define (event-log-key/c el)
  (define event (event-log-event el))
  (flat-named-contract `(event-log-key/c ,(string->symbol (abi-event-name event)))
                       (lambda (key) (and (parameter-index event key) #t))))

;;;; topics

(define (elementary? t)
  (or (abi-uint? t) (abi-int? t) (abi-address? t) (abi-bool? t) (abi-fixed-bytes? t)))

(define (signature-topic event)
  (define preimage (abi-signature->string (abi-event->signature event)))
  (bytes->hex (keccak256 (string->bytes/utf-8 preimage))))

(define/who (event-topic who spec)
  (define event (spec->abi-event/who who spec))
  (and (not (abi-event-anonymous? event)) (signature-topic event)))

(define (pad bs)
  (bytes-append bs (make-bytes (modulo (- (bytes-length bs)) 32) 0)))

;; The encoding Solidity hashes for an indexed value of a non-elementary
;; type: no offsets or lengths, and each element padded to whole words.  v is checked.
(define (in-place t v)
  (define (element t x)
    (pad (if (elementary? t) (encode-sequence (list t) (list x)) (in-place t x))))
  (match t
    [(struct* abi-string ()) (string->bytes/utf-8 v)]
    [(struct* abi-bytes ()) v]
    [(or (struct* abi-array ([element e])) (struct* abi-fixed-array ([element e])))
     (apply bytes-append #"" (map (lambda (x) (element e x)) v))]
    [(struct* abi-tuple ([components cs])) (apply bytes-append #"" (map element cs v))]))

;; The topic of v as an indexed value of t, with value errors reported at
;; argument index.
(define (value->topic who t v index)
  (match (encoding-failure (list t) (list v))
    [#f
     (bytes->hex (if (elementary? t)
                     (encode-sequence (list t) (list v))
                     (keccak256 (in-place t v))))]
    [(list reason type value (cons _ path))
     (raise-abi-value-error who (list reason type value (cons index path)))]))

(define/who (abi-value->topic who type v)
  (value->topic who (abi-type/who who type) v 0))

(struct any-of (values)
  #:constructor-name new-any-of
  #:omit-define-syntaxes)

(define (any-of . vs)
  (new-any-of vs))

(struct indexed-hash (topic) #:transparent
  #:guard (lambda (topic _who) (string-downcase topic)))

(define (where-value->topic who p index v)
  (define t (abi-event-parameter-type p))
  (cond
    [(and (indexed-hash? v) (not (elementary? t))) (indexed-hash-topic v)]
    [else (value->topic who t v index)]))

(define (position who p index v)
  (match v
    [(? any-of?)
     (match (remove-duplicates (map (lambda (x) (where-value->topic who p index x)) (any-of-values v)))
       [(list topic) topic]
       [topics topics])]
    [_ (where-value->topic who p index v)]))

(define/who (event-topics who spec [where (hasheq)])
  (define event (spec->abi-event/who who spec))
  (define by-index
    (for/hash ([(key v) (in-hash where)])
      (values (parameter-index event key) v)))
  (define positions
    (for/list ([p (in-list (abi-event-parameters event))]
               [i (in-naturals)]
               #:when (abi-event-parameter-indexed? p))
      (and (hash-has-key? by-index i) (position who p i (hash-ref by-index i)))))
  (dropf-right (if (abi-event-anonymous? event) positions (cons (signature-topic event) positions))
               not))

;;;; decoding

(define (write-event-log el port _mode)
  (match-define (struct* event-log ([event event] [arguments arguments])) el)
  (define (argument->string p v)
    (match-define (struct* abi-event-parameter ([name name] [type t])) p)
    (string-append (if name (string-append name ": ") "")
                   (if (indexed-hash? v)
                       (string-append "hash " (indexed-hash-topic v))
                       (abi-value->display-string t v))))
  (fprintf port "#<event-log ~a(~a) ~a>"
           (abi-event-name event)
           (string-join (map argument->string (abi-event-parameters event) arguments) ", ")
           (log-tail el)))

(struct event-log log-struct (event arguments)
  #:transparent
  #:property prop:custom-write write-event-log)

(define (event-log-name el)
  (abi-event-name (event-log-event el)))

(define (event-log-ref el key)
  (list-ref (event-log-arguments el) (parameter-index (event-log-event el) key)))

(define (interleave ps indexed plain)
  (match* (ps indexed plain)
    [('() _ _) '()]
    [((cons (struct* abi-event-parameter ([indexed? #t])) ps) (cons v indexed) plain)
     (cons v (interleave ps indexed plain))]
    [((cons _ ps) indexed (cons v plain)) (cons v (interleave ps indexed plain))]))

(define (topics-match? event topics indexed)
  (match* ((abi-event-anonymous? event) topics)
    [(#t _) (= (length topics) (length indexed))]
    [(#f (cons topic _)) (and (= (length topics) (add1 (length indexed)))
                              (equal? topic (signature-topic event)))]
    [(#f '()) #f]))

;; Offsets count bytes in the topics followed by the data.
(define (decode-as who event l)
  (match-define (struct* abi-event ([parameters ps] [anonymous? anonymous?])) event)
  (match-define (struct* log-struct ([topics topics] [data data])) l)
  (define indexed (filter abi-event-parameter-indexed? ps))
  (define plain (filter-not abi-event-parameter-indexed? ps))
  (define bs (apply bytes-append (append (map hex->bytes topics) (list data))))
  (define (indexed-value p i)
    (define t (abi-event-parameter-type p))
    (if (elementary? t)
        (car (abi-decode/who who (list t) bs (* 32 i)))
        (indexed-hash (list-ref topics i))))
  (cond
    [(not (topics-match? event topics indexed))
     (raise-abi-decode-error who 'topic (map abi-event-parameter-type indexed) 0)]
    [else
     (define indexed-values
       (for/list ([p (in-list indexed)]
                  [i (in-naturals (if anonymous? 0 1))])
         (indexed-value p i)))
     (define plain-values
       (abi-decode/who who (map abi-event-parameter-type plain) bs (* 32 (length topics))))
     (apply event-log (log->list l event (interleave ps indexed-values plain-values)))]))

(define/who (decode-log who spec l)
  (decode-as who (spec->abi-event/who who spec) l))

;; A log is told apart by topic 0 and its number of topics; an anonymous
;; event only by the number.
(define (event-key event)
  (define k (count abi-event-parameter-indexed? (abi-event-parameters event)))
  (if (abi-event-anonymous? event)
      (cons 'anonymous k)
      (cons (signature-topic event) (add1 k))))

(define (event-table who specs)
  (for/fold ([table (hash)])
            ([spec (in-list specs)])
    (define event (spec->abi-event/who who spec))
    (define key (event-key event))
    (match (hash-ref table key #f)
      [#f (hash-set table key event)]
      [(== event) table]
      [other
       (raise-abi-type-error
        who
        (format "ambiguous events: ~a and ~a ~a"
                (abi-event->string other)
                (abi-event->string event)
                (if (abi-event-anonymous? event)
                    "are anonymous with the same number of topics"
                    "have the same topic 0 and number of topics"))
        specs
        #f)])))

(define (decode-known who table l)
  (define topics (log-topics l))
  (define keys
    (match topics
      ['() (list (cons 'anonymous 0))]
      [(cons topic _) (list (cons topic (length topics)) (cons 'anonymous (length topics)))]))
  (for/or ([key (in-list keys)])
    (define event (hash-ref table key #f))
    (and event
         (with-handlers ([exn:fail:web3:abi:decode? (lambda (_) #f)])
           (decode-as who event l)))))

(define (logs-decoder who specs)
  (define table (event-table who specs))
  (lambda (logs)
    (for/list ([l (in-list logs)])
      (or (decode-known who table l) l))))

(define/who (decode-logs who specs logs)
  ((logs-decoder who specs) logs))

(module* internal #f
  (provide event-topics/who
           event-where/c
           logs-decoder
           topic-positions/c))
