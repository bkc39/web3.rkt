#lang racket/base

(require (for-syntax racket/base
                     (only-in racket/string string-join)
                     (only-in racket/syntax format-id)
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
                  raise-node-malformed)
         "who.rkt")

(provide access-list-field
         address-field
         bit-field
         boolean-field
         byte-field
         data-field
         define-node-record
         field-kind
         field-kind-contract
         field-kind-name
         field-kind-reader
         field-kind?
         fixed-data-field
         hash-abbreviation
         hash-field
         list-field
         quantity-field
         record-field
         uint64-field)

;; reader : (who what v) -> value, raising through raise-node-malformed.
(struct field-kind (name reader contract))

(define (hash-abbreviation hash)
  (string-append (substring hash 0 10) "…"))

(define (natural-field name bits contract)
  (field-kind name (lambda (who what v) (node-natural who what v bits)) contract))

(define (read-boolean who what v)
  (if (boolean? v) v (raise-node-malformed who what v)))

(define quantity-field (field-kind 'quantity node-quantity->number uint256/c))
(define uint64-field (natural-field 'uint64 64 uint64/c))
(define byte-field (natural-field 'byte 8 byte?))
(define bit-field (natural-field 'bit 1 (or/c 0 1)))
(define boolean-field (field-kind 'boolean read-boolean boolean?))
(define address-field (field-kind 'address node-address->string checksum-address?))
(define hash-field (field-kind 'hash node-hash->string hash-string/c))
(define data-field (field-kind 'data node-data->bytes bytes?))

(define (fixed-data-field size)
  (field-kind (list 'fixed-data size)
              (lambda (who what v)
                (define bs (node-data->bytes who what v))
                (if (= (bytes-length bs) size) bs (raise-node-malformed who what v)))
              (flat-named-contract (string->symbol (format "bytes~a/c" size))
                                   (lambda (v) (and (bytes? v) (= (bytes-length v) size))))))

(define (list-field kind)
  (define read-element (field-kind-reader kind))
  (field-kind (list 'list (field-kind-name kind))
              (lambda (who what v)
                (if (list? v)
                    (for/list ([element (in-list v)]
                               [index (in-naturals)])
                      (read-element who (format "~a ~a" what index) element))
                    (raise-node-malformed who what v)))
              (listof (field-kind-contract kind))))

(define (record-field reader/who record?)
  (field-kind (list 'record (object-name record?))
              (lambda (who what v) (reader/who who v (string-append " in " what)))
              record?))

(define read-storage-keys (field-kind-reader (list-field (fixed-data-field 32))))

(define (read-access-entry who what v)
  (match v
    [(hash-table ('address address) ('storageKeys keys) _ ...)
     (cons (node-address->string who (string-append what " address") address)
           (read-storage-keys who (string-append what " storage keys") keys))]
    [_ (raise-node-malformed who what v)]))

(define access-list-field
  (field-kind 'access-list
              (field-kind-reader (list-field (field-kind 'access-entry read-access-entry any/c)))
              access-list/c))

(define (read-row who v key kind what optional?)
  (define raw (hash-ref v key 'null))
  (if (and optional? (eq? raw 'null)) #f ((field-kind-reader kind) who what raw)))

(define (read-defaulted who v key kind what default)
  (if (hash-has-key? v key) ((field-kind-reader kind) who what (hash-ref v key)) default))

(define (row-contract kind optional?)
  (if optional? (or/c #f (field-kind-contract kind)) (field-kind-contract kind)))

(define (no-check _who _detail _record)
  (void))

(begin-for-syntax
  (define-syntax-class row
    #:description "[field json-key kind option ...]"
    (pattern [field:id json-key:id kind:expr
              (~alt (~optional (~or* (~and #:optional (~bind [mode #''optional] [optional? #'#t]))
                                     (~seq #:default default:expr
                                           (~bind [mode #''default] [optional? #'#f])))
                               #:defaults ([mode #''required] [optional? #'#f]))
                    (~optional (~and #:unprovided (~bind [provided? #'#f]))
                               #:defaults ([provided? #'#t])))
              ...]))

  (define (field-words noun field)
    (string-join (cons noun (regexp-split #rx"-" (regexp-replace #rx"[?]$" field ""))) " ")))

(define-syntax-parse-rule (define-node-record name:id
                            #:noun noun:str
                            #:printer printer:expr
                            (~optional (~seq #:check check:expr) #:defaults ([check #'no-check]))
                            row:row ...)
  #:with name-struct (format-id #'name "~a-struct" #'name)
  #:with new-name (format-id #'name "new-~a" #'name)
  #:with name? (format-id #'name "~a?" #'name)
  #:with name-raw (format-id #'name "~a-raw" #'name)
  #:with name-fields (format-id #'name "~a-fields" #'name)
  #:with jsexpr->name (format-id #'name "jsexpr->~a" #'name)
  #:with name->jsexpr (format-id #'name "~a->jsexpr" #'name)
  #:with name->list (format-id #'name "~a->list" #'name)
  #:with (accessor ...) (for/list ([field (in-list (syntax->list #'(row.field ...)))])
                          (format-id #'name "~a-~a" #'name field))
  #:with (kind-id ...) (generate-temporaries #'(row ...))
  #:with (what ...) (for/list ([field (in-list (syntax->list #'(row.field ...)))])
                      (datum->syntax #'noun
                                     (field-words (syntax-e #'noun)
                                                  (symbol->string (syntax-e field)))))
  #:with ((provided-accessor provided-kind provided-optional?) ...)
  (for/list ([accessor (in-list (syntax->list #'(accessor ...)))]
             [kind (in-list (syntax->list #'(kind-id ...)))]
             [optional? (in-list (syntax->list #'(row.optional? ...)))]
             [provided? (in-list (syntax->list #'(row.provided? ...)))]
             #:when (syntax-e provided?))
    (list accessor kind optional?))
  (begin
    (define kind-id row.kind) ...
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
           (new-name (~? (read-defaulted who v 'row.json-key kind-id (string-append what detail)
                                         row.default)
                         (read-row who v 'row.json-key kind-id (string-append what detail)
                                   row.optional?))
                     ...
                     v))
         (check who detail record)
         record]
        [else (raise-node-malformed who (string-append noun detail) v)]))
    (define (name->jsexpr record)
      (name-raw record))
    (define (name->list record)
      (list (accessor record) ... (name-raw record)))
    (define name-fields
      (list (list 'row.field 'row.json-key kind-id row.mode row.provided?) ...))
    (provide (contract-out
              [name? (-> any/c boolean?)]
              [provided-accessor (-> name? (row-contract provided-kind provided-optional?))] ...
              [jsexpr->name (-> any/c name?)]
              [name->jsexpr (-> name? hash?)]))))
