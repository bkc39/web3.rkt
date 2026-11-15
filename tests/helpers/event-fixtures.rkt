#lang racket/base

;; Events, logs and topics shared by the event tests.

(require racket/match
         web3
         (only-in "../abi-gen.rkt" random-bytes)
         (only-in "../util.rkt" checksummed filled-hash word)
         (only-in (submod web3/abi/event-type private) spec->abi-event/who)
         (only-in web3/private/hex bytes->hex))

(provide (struct-out known-topic)
         approval
         bare
         counted
         decoded-arguments
         deposit
         flags
         four
         hashed-type?
         known-topics
         lists
         log-jsexpr
         nested
         note
         paired
         random-hash
         tagged
         transfer
         transfer-721
         transfer-topic
         topic-word
         topics+data)

(define transfer "Transfer(address indexed from, address indexed to, uint256 value)")
(define transfer-721 "Transfer(address indexed from, address indexed to, uint256 indexed tokenId)")
(define approval "Approval(address indexed owner, address indexed spender, uint256 value)")
(define deposit "Deposit(address indexed from, uint256 value, string memo)")
(define tagged "Tagged(string indexed tag, bytes indexed blob, address indexed sender, string text)")
(define lists "Lists(uint256[] indexed ids, string[] indexed names, uint8[2] indexed pair)")
(define nested "Nested(uint256[][] indexed grid, (uint256,string)[] indexed pairs)")
(define paired "Paired((uint256,string) indexed pair, (uint256,string) plain)")
(define flags "Flags(bool indexed flag, int8 indexed delta, bytes3 indexed code)")
(define bare "Bare(uint256, string)")
(define counted "Counted(uint256 indexed i)")
(define note "Note(address indexed sender, uint256 value) anonymous")
(define four "Four(uint256 indexed a, uint256 indexed b, uint256 indexed c, uint256 indexed d) anonymous")

(define transfer-topic "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef")

;; From cast sig-event and solc --hashes.
(struct known-topic (event topic) #:prefab)

(define known-topics
  `(#s(known-topic ,transfer ,transfer-topic)
    #s(known-topic ,transfer-721 ,transfer-topic)
    #s(known-topic ,approval "0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925")
    #s(known-topic ,deposit "0x643e927b32d5bfd08eccd2fcbd97057ad413850f857a2359639114e8e8dd3d7b")
    #s(known-topic ,tagged "0x56674d9d23bd3ef0d98e964e72b0cd1c72ea99340fbb1850c3cd4e9996d3b73a")
    #s(known-topic ,lists "0x58f467372b4f54586d5c6e4a656fbfad514cc4c491b6570c229b2cc8502b99f1")
    #s(known-topic ,nested "0x7d18048971f2ec5a741a98089e81f036a07f92da1a4321c2dc35c6ce55f89155")
    #s(known-topic ,paired "0xec4f9132e752f135bc5fbb7e34044b5354ead3c3ca29ec16ce7be28d53fdc414")
    #s(known-topic ,flags "0xb2e3cf2c1dd3a68f5412a6eba0b8dbfce1d0e75c8fe697a8a410e1b19d17b37b")
    #s(known-topic ,bare "0x8cccb0c5b28f620a5e57ab28294c9328b43da0c2b64b1c4c4680b4f8a3d42d3c")
    #s(known-topic ,counted "0x15df0a6785153dfd625c8af51397704c7ddaff0690e5076243b5e52f4a0d5409")
    #s(known-topic ,note #f)
    #s(known-topic ,four #f)))

(define (topic-word v)
  (bytes->hex (word (if (string? v) (string->number (substring v 2) 16) v))))

(define (random-hash prng)
  (bytes->hex (random-bytes prng 32)))

(define (topics+data e vs)
  (match-define (struct* abi-event ([parameters ps] [anonymous? anonymous?]))
    (spec->abi-event/who 'topics+data e))
  (define indexed-topics
    (for/list ([p (in-list ps)]
               [v (in-list vs)]
               #:when (abi-event-parameter-indexed? p))
      (abi-value->topic (abi-event-parameter-type p) v)))
  (define-values (types plain)
    (for/lists (types plain)
               ([p (in-list ps)]
                [v (in-list vs)]
                #:unless (abi-event-parameter-indexed? p))
      (values (abi-event-parameter-type p) v)))
  (values (if anonymous? indexed-topics (cons (event-topic e) indexed-topics))
          (abi-encode types plain)))

(define (hashed-type? t)
  (or (abi-string? t) (abi-bytes? t) (abi-array? t) (abi-fixed-array? t) (abi-tuple? t)))

(define (decoded-arguments e vs)
  (for/list ([p (in-list (abi-event-parameters (spec->abi-event/who 'decoded-arguments e)))]
             [v (in-list vs)])
    (match-define (struct* abi-event-parameter ([type t] [indexed? indexed?])) p)
    (if (and indexed? (hashed-type? t))
        (indexed-hash (abi-value->topic t v))
        (car (checksummed (list t) (list v))))))

;; A node's log object, as anvil writes it.
(define (log-jsexpr #:address [address "0x5fbdb2315678afecb367f032d93f642f64180aa3"]
                    #:topics topics
                    #:data [data #""]
                    #:block [block 2]
                    #:index [index 0]
                    #:removed [removed #f])
  (hasheq 'address address
          'topics topics
          'data (bytes->hex data)
          'blockHash (filled-hash #\a)
          'blockNumber (format "0x~a" (number->string block 16))
          'blockTimestamp "0x6553f100"
          'transactionHash (filled-hash #\b)
          'transactionIndex "0x0"
          'logIndex (format "0x~a" (number->string index 16))
          'removed removed))
