#lang racket/base

(require (for-syntax racket/base
                     (only-in racket/string string-join)
                     (only-in racket/syntax format-id generate-temporary)
                     syntax/parse)
         racket/contract/base
         (only-in racket/match match)
         syntax/parse/define
         (only-in "../address.rkt" checksum-address?)
         (only-in "field-contracts.rkt" access-list/c hash-string/c uint256/c uint64/c)
         (only-in "node.rkt"
                  node-address->string
                  node-data->bytes
                  node-hash->string
                  node-natural
                  node-quantity->number
                  raise-node-malformed
                  raise-node-missing)
         "who.rkt")

(provide access-list-field
         address-field
         bit-field
         boolean-field
         byte-field
         check-asked-hash
         data-field
         define-node-record
         (struct-out field-kind)
         (struct-out field-row)
         fixed-data-field
         hash-abbreviation
         hash-field
         lenient-uint64-field
         list-field
         quantity-field
         read-field
         record-field
         uint64-field)

(struct field-kind (name reader contract))

(struct field-row (field key kind mode provided?))

(define (hash-abbreviation digest)
  (string-append (substring digest 0 10) "…"))

(define (check-asked-hash who what asked received)
  (unless (equal? received (string-downcase asked))
    (raise-node-malformed who what received)))

(define (read-field kind who what v)
  ((field-kind-reader kind) who what "" v))

(define (node-kind name read contract)
  (field-kind name (lambda (who what detail v) (read who (string-append what detail) v)) contract))

(define (natural-field name bits contract)
  (node-kind name (lambda (who what v) (node-natural who what v bits)) contract))

(define (read-boolean who what v)
  (if (boolean? v) v (raise-node-malformed who what v)))

(define (read-lenient-uint64 who what v)
  (if (and (exact-nonnegative-integer? v) (< v (expt 2 64)))
      v
      (node-natural who what v 64)))

(define quantity-field (node-kind 'quantity node-quantity->number uint256/c))
(define uint64-field (natural-field 'uint64 64 uint64/c))
(define lenient-uint64-field (node-kind 'lenient-uint64 read-lenient-uint64 uint64/c))
(define byte-field (natural-field 'byte 8 byte?))
(define bit-field (natural-field 'bit 1 (or/c 0 1)))
(define boolean-field (node-kind 'boolean read-boolean boolean?))
(define address-field (node-kind 'address node-address->string checksum-address?))
(define hash-field (node-kind 'hash node-hash->string hash-string/c))
(define data-field (node-kind 'data node-data->bytes bytes?))

(define (fixed-data-field size)
  (node-kind (list 'fixed-data size)
             (lambda (who what v)
               (define bs (node-data->bytes who what v))
               (if (= (bytes-length bs) size) bs (raise-node-malformed who what v)))
             (flat-named-contract (string->symbol (format "bytes~a/c" size))
                                  (lambda (v) (and (bytes? v) (= (bytes-length v) size))))))

(define (list-field kind #:max-length [max-length +inf.0])
  (define read-element (field-kind-reader kind))
  (define element/c (field-kind-contract kind))
  (field-kind (list 'list (field-kind-name kind) max-length)
              (lambda (who what detail v)
                (if (and (list? v) (<= (length v) max-length))
                    (for/list ([element (in-list v)]
                               [index (in-naturals)])
                      (read-element who (format "~a ~a" what index) detail element))
                    (raise-node-malformed who (string-append what detail) v)))
              (if (= max-length +inf.0)
                  (listof element/c)
                  (and/c (listof element/c) (property/c length (<=/c max-length))))))

(define (record-field reader/who record?)
  (field-kind (list 'record (object-name record?))
              (lambda (who what detail v) (reader/who who v (string-append " in " what detail)))
              record?))

(define read-storage-keys (field-kind-reader (list-field (fixed-data-field 32))))

(define (read-access-entry who what detail v)
  (match v
    [(hash-table ('address address) ('storageKeys keys) _ ...)
     (cons (node-address->string who (string-append what " address" detail) address)
           (read-storage-keys who (string-append what " storage keys") detail keys))]
    [_ (raise-node-malformed who (string-append what detail) v)]))

(define access-list-field
  (field-kind 'access-list
              (field-kind-reader (list-field (field-kind 'access-entry read-access-entry any/c)))
              access-list/c))

(define absent (string->uninterned-symbol "absent"))

(define (read-row who v key kind what detail mode default)
  (define raw (hash-ref v key absent))
  (cond
    [(and (eq? mode 'optional) (memq raw (list absent 'null))) #f]
    [(and (eq? mode 'default) (eq? raw absent)) default]
    [(eq? raw absent) (raise-node-missing who (string-append what detail))]
    [else ((field-kind-reader kind) who what detail raw)]))

(define (row-contract kind mode)
  (if (eq? mode 'optional) (or/c #f (field-kind-contract kind)) (field-kind-contract kind)))

(define (no-check _who _detail _record)
  (void))

(begin-for-syntax
  (define (field-words noun field)
    (string-join (cons noun (regexp-split #rx"-" (regexp-replace #rx"[?]$" field ""))) " "))

  (define (accessor-id record field)
    (format-id record "~a-~a" record field))

  (define-syntax-class (row record noun)
    #:description "[field kind option ...]"
    (pattern [field:id kind:expr
              (~alt (~optional (~seq #:key key:id) #:defaults ([key #'field]))
                    (~optional (~or* (~and #:optional (~bind [mode #''optional]))
                                     (~seq #:default value:expr (~bind [mode #''default])))
                               #:defaults ([mode #''required]))
                    (~optional (~and #:unprovided (~bind [(exported 1) '()]))
                               #:defaults ([(exported 1) (list (accessor-id record #'field))])))
              ...]
             #:with accessor (accessor-id record #'field)
             #:with kind-id (generate-temporary #'field)
             #:with record? (format-id record "~a?" record)
             #:with (export ...) #'([exported (-> record? (row-contract kind-id mode))] ...)
             #:with what (datum->syntax #'field
                                        (field-words noun (symbol->string (syntax-e #'field)))))))

(define-syntax-parse-rule (define-node-record name:id
                            #:noun noun:str
                            #:printer printer:expr
                            (~optional (~seq #:check check:expr) #:defaults ([check #'no-check]))
                            (~var row (row #'name (syntax-e #'noun))) ...)
  #:with name-struct (format-id #'name "~a-struct" #'name)
  #:with new-name (format-id #'name "new-~a" #'name)
  #:with name? (format-id #'name "~a?" #'name)
  #:with name-raw (format-id #'name "~a-raw" #'name)
  #:with name-fields (format-id #'name "~a-fields" #'name)
  #:with jsexpr->name (format-id #'name "jsexpr->~a" #'name)
  #:with name->jsexpr (format-id #'name "~a->jsexpr" #'name)
  #:with name->list (format-id #'name "~a->list" #'name)
  (begin
    (define row.kind-id row.kind) ...
    (struct name (row.field ... raw)
      #:name name-struct
      #:constructor-name new-name
      #:transparent
      #:property prop:custom-write printer
      #:property prop:custom-print-quotable 'never)
    (define/who (jsexpr->name who v [detail ""])
      (cond
        [(hash? v)
         (define record
           (new-name (read-row who v 'row.key row.kind-id row.what detail row.mode
                               (~? row.value #f))
                     ...
                     v))
         (check who detail record)
         record]
        [else (raise-node-malformed who (string-append noun detail) v)]))
    (define (name->jsexpr record)
      (name-raw record))
    (define (name->list record . more)
      (list* (row.accessor record) ... (name-raw record) more))
    (define name-fields
      (list (field-row 'row.field 'row.key row.kind-id row.mode (pair? '(row.exported ...))) ...))
    (provide (contract-out
              [name? (-> any/c boolean?)]
              row.export ... ...
              [jsexpr->name (-> any/c name?)]
              [name->jsexpr (-> name? hash?)]))))
