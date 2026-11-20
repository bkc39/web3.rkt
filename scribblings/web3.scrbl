#lang scribble/manual
@require[@for-label[json
                    libsecp256k1
                    racket/base
                    racket/contract
                    racket/generic
                    web3
                    web3/abi
                    web3/account
                    web3/abi/description
                    web3/abi/event-type
                    web3/abi/type
                    web3/abi/vectors
                    web3/address
                    web3/anvil
                    web3/contract
                    web3/abi/event
                    web3/eth
                    web3/eth/log
                    web3/event
                    web3/rlp
                    web3/rpc]
          scribble/example]

@(define abi-eval (make-base-eval '(require web3/abi web3/address web3/rpc)))
@(define contract-eval (make-base-eval '(require web3)))
@(define event-eval (make-base-eval '(require web3)))
@(define account-eval (make-base-eval '(require web3 (only-in keccak keccak256))))
@(define rlp-eval (make-base-eval '(require web3 (only-in keccak keccak256))))

@title{web3: Ethereum JSON-RPC for Racket}
@author{bkc}

@defmodule[web3]

An Ethereum JSON-RPC client: typed wrappers for the standard
@tt{web3_*}, @tt{net_*} and @tt{eth_*} methods, the contract ABI, and
support for running local
@hyperlink["https://book.getfoundry.sh/anvil/"]{anvil} nodes.

@racket[(require web3)] provides everything in @racketmodname[web3/rpc],
@racketmodname[web3/eth], @racketmodname[web3/abi],
@racketmodname[web3/address], @racketmodname[web3/account], @racketmodname[web3/rlp],
@racketmodname[web3/contract],
@racketmodname[web3/event] and @racketmodname[web3/anvil].

@racketblock[
(require web3)

(code:comment "a throwaway local chain, as the current provider")
(with-anvil ()
  (define me (car (eth-accounts)))
  (eth-get-balance me "latest")
  (anvil-set-code "0x00000000000000000000000000000000000c0de0"
                  "0x602a60005260206000f3")
  (eth-call (hasheq 'to "0x00000000000000000000000000000000000c0de0"
                    'data "0x")
            "latest"))

(code:comment "any node")
(with-provider (url->http-provider "https://eth.llamarpc.com")
  (eth-block-number))
]

@section{Conventions}

Each endpoint is named after its JSON-RPC method in kebab-case, so
@tt{eth_getBalance} becomes @racket[eth-get-balance].  Every endpoint
takes an optional @racket[#:provider] argument that defaults to
@racket[(current-provider)].

Results come back decoded where the encoding is unambiguous:

@itemlist[
 @item{quantities (balances, gas, block numbers, counts) are exact
       nonnegative integers;}
 @item{data, hashes and addresses are @racket["0x..."] strings;}
 @item{transactions, receipts and blocks are @racket[jsexpr?]
       hash tables with symbol keys, as read by @racketmodname[json];}
 @item{logs from @racket[get-logs] and @racket[receipt-logs] are
       @racket[log] values; @racket[eth-get-logs] returns the node's JSON;}
 @item{a missing object (an unknown transaction, a block that does not
       exist yet) is @racket['null].}]

Addresses and transaction objects are passed as the node expects them:
addresses as @racket["0x..."] strings, and transaction or call objects
as hash tables such as
@racket[(hasheq 'from sender 'to contract 'data "0x...")].

A JSON-RPC error response raises @racket[exn:fail:web3:rpc]; contract
reverts carry their payload in @racket[exn:fail:web3:rpc-data], which
@racket[rpc-exn->revert] decodes.  A result that does not decode
as documented raises @racket[exn:fail:web3].

@section[#:tag "guide"]{Guide}

@subsection[#:tag "guide-accounts"]{Accounts and Signatures}

An account that signs locally is a secp256k1 secret key.  @racket[generate-secret-key] makes a
new one; @racket[hex-string->secret-key] reads one written as 64 hexadecimal digits, the form
wallets and @tt{anvil} print.  The account's address comes from the key.

@examples[#:eval account-eval
          (define key
            (hex-string->secret-key
             "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))
          key
          (secret-key->address key)]

@racket[sign-digest] signs a 32-byte digest, usually a Keccak-256 hash, and does not hash it
again.  The same key and digest always give the same signature.  @racket[recover-address] gives
the address that signed a digest; for another digest it gives another address.

@examples[#:eval account-eval
          (define digest (keccak256 #"hello"))
          (define sig (sign-digest key digest))
          (recover-address sig digest)
          (equal? (recover-address sig (keccak256 #"hello!")) (secret-key->address key))]

Nodes, wallets and contracts pass a signature as 65 bytes.

@examples[#:eval account-eval
          (bytes->hex-string (signature->bytes sig))
          (equal? (bytes->signature (signature->bytes sig)) sig)]

See @filepath{examples/09-accounts.rkt}.

@subsection[#:tag "guide-rlp"]{Recursive Length Prefix}

Transactions, and the addresses of contracts created by @tt{CREATE}, are hashed and sent as RLP:
an item is a byte string or a list of items.  @racket[rlp-encode] writes an item and
@racket[rlp-decode] reads one back.

@examples[#:eval rlp-eval
          (bytes->hex-string (rlp-encode (list #"cat" #"dog")))
          (rlp-decode (hex-string->bytes "0xc88363617483646f67"))]

RLP has no integers.  A number is written as a scalar, its big-endian bytes without leading
zeros, so zero is the empty byte string.

@examples[#:eval rlp-eval
          (bytes->hex-string (rlp-encode (list (natural->rlp-scalar 0) (natural->rlp-scalar 1024))))
          (map rlp-scalar->natural (rlp-decode (hex-string->bytes "0xc480820400")))]

A contract's address is the last 20 bytes of the Keccak-256 hash of the list of its creator's
address and nonce, which is what @racket[create-address] computes.

@examples[#:eval rlp-eval
          (define sender "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")
          (define preimage (rlp-encode (list (hex-string->bytes sender) (natural->rlp-scalar 1))))
          (address->checksum-string (bytes->hex-string (subbytes (keccak256 preimage) 12)))
          (create-address sender 1)]

Each item has exactly one encoding, and @racket[rlp-decode] accepts nothing else: not a byte
written with a longer prefix than it needs, nor bytes after the item.

@examples[#:eval rlp-eval
          (eval:error (rlp-decode (bytes #x81 #x05)))
          (eval:error (rlp-decode (bytes #xc0 #x00)))]

See @filepath{examples/10-rlp.rkt}.

@section{Providers and Calls}

@defmodule[web3/rpc]

A provider delivers JSON-RPC requests to a node: over HTTP
(@racket[url->http-provider]), from a table (@racket[mock-provider]),
or any other way a structure implementing @racket[gen:provider] chooses.

@defproc[(provider? [v any/c]) boolean?]{
Recognizes providers: instances of structures implementing
@racket[gen:provider].}

@defproc[(provider-request [provider provider?]
                           [method string?]
                           [params (listof jsexpr?)])
         jsexpr?]{
Sends one request and returns its @tt{result}.  Raises
@racket[exn:fail:web3:rpc] for an error response and
@racket[exn:fail:web3] for anything else @racket[provider] cannot
deliver, including a result that is not a @racket[jsexpr?].  Most code
calls @racket[rpc-call] or an endpoint instead.}

@defproc[(provider-close [provider provider?]) void?]{
Releases what @racket[provider] holds open.  Providers that hold
nothing do nothing.}

@defparam[current-provider provider (or/c #f provider?)
          #:value #f]{
The provider used by endpoints and @racket[rpc-call] when no
@racket[#:provider] argument is given.}

@defform[(with-provider provider-expr body ...+)
         #:contracts ([provider-expr (or/c #f provider?)])]{
Evaluates the @racket[body]s with @racket[current-provider] set to the
value of @racket[provider-expr].}

@defproc[(rpc-call [method string?]
                   [params (listof jsexpr?)]
                   [#:provider provider (or/c #f provider?)
                    (current-provider)])
         jsexpr?]{

Sends a JSON-RPC request for @racket[method] with positional
@racket[params] through @racket[provider-request] and returns the
response's @tt{result}.  Raises @racket[exn:fail:web3] when
@racket[provider] is @racket[#f].

@racketblock[
(rpc-call "eth_getBalance" (list address "latest"))
]}

@defform[#:literals (->)
         (define-jsonrpc-endpoint rpc-name
           (-> arg-spec ... result-contract-expr)
           maybe-transform)
         #:grammar ([arg-spec contract-expr
                              [contract-expr ->param-expr]]
                    [maybe-transform (code:line)
                                     (code:line #:result-transform transform-expr)])]{

Defines a function for the JSON-RPC method @racket[rpc-name], named
after it in kebab-case (@racket[eth_getBalance] defines
@racket[eth-get-balance]).  The function takes one positional argument
per @racket[arg-spec] and an optional @racket[#:provider].

Each argument is checked against its @racket[contract-expr] and, when
the spec gives a @racket[->param-expr], converted by it into the JSON
parameter.  The response's @tt{result} goes through
@racket[transform-expr] (default @racket[values]) and is checked against
@racket[result-contract-expr], a flat contract.  A result that
@racket[transform-expr] raises on or that fails the contract raises
@racket[exn:fail:web3] with the message
@racket["method: unexpected result value"].

@racketblock[
(define-jsonrpc-endpoint eth_getBalance
  (-> string? [block-param/c block-param->string] exact-nonnegative-integer?)
  #:result-transform hex-string->number)
]}

@subsection{HTTP Providers}

@defproc[(url->http-provider [url string?]) http-provider?]{

Returns a provider that sends JSON-RPC requests to @racket[url] over
HTTP or HTTPS.  The path and query of @racket[url] are kept, so URLs
that carry an API key in their path work as given.

The provider can be shared between threads.  An interrupted or failed
request never leaks its reply into a later one.  A lost connection, or
one closed by @racket[provider-close], is reopened on the next request.}

@defproc[(http-provider? [v any/c]) boolean?]{
Recognizes providers made by @racket[url->http-provider].}

@defproc[(http-provider-url [provider http-provider?]) string?]{
The URL that @racket[provider] was made from.}

@subsection{Writing a Provider}

@defthing[gen:provider any/c]{
A generic interface with the methods @racket[provider-request], which
every implementation defines, and @racket[provider-close], which
defaults to doing nothing.

@racketblock[
(struct logging (inner)
  #:methods gen:provider
  [(define/generic inner-request provider-request)
   (define/generic inner-close provider-close)
   (define (provider-request p method params)
     (define start (current-inexact-milliseconds))
     (begin0 (inner-request (logging-inner p) method params)
       (printf ";; ~a ~s took ~a ms\n"
               method
               params
               (real->decimal-string (- (current-inexact-milliseconds) start) 1))))
   (define (provider-close p)
     (inner-close (logging-inner p)))])

(with-provider (logging (url->http-provider "http://127.0.0.1:8545"))
  (eth-block-number))
]}

@subsection{Mock Providers}

@defproc[(mock-provider [handler (or/c (-> string? list? jsexpr?)
                                       (hash/c string?
                                               (or/c (-> list? jsexpr?)
                                                     jsexpr?)))])
         mock-provider?]{
A provider that answers from @racket[handler] instead of a node.  A
procedure is called with each request's method and params.  A hash
maps method names to a result, or to a procedure of the params; a
method it lacks raises @racket[exn:fail:web3:rpc] with code
@racket[-32601].  Handlers answer with an error by calling
@racket[raise-rpc-error].  A mock can be shared between threads.

@racketblock[
(define p (mock-provider (hash "eth_chainId" "0x1"
                               "eth_getBalance" "0xff")))
(eth-get-balance "0xaa" 5 #:provider p)
(code:comment "=> 255")
(mock-provider-calls p)
(code:comment "=> '((\"eth_getBalance\" \"0xaa\" \"0x5\"))")
]}

@defproc[(mock-provider? [v any/c]) boolean?]{
Recognizes providers made by @racket[mock-provider].}

@defproc[(mock-provider-calls [provider mock-provider?])
         (listof (cons/c string? list?))]{
The requests @racket[provider] has received, oldest first, each as
@racket[(cons method params)].}

@subsection{Errors}

@defstruct*[(exn:fail:web3 exn:fail) ()]{
Raised for failures specific to this library, such as calling an
endpoint with no provider or timing out while waiting for a receipt.}

@defstruct*[(exn:fail:web3:rpc exn:fail:web3)
            ([method string?]
             [code (or/c #f exact-integer?)]
             [rpc-message string?]
             [data (or/c #f jsexpr?)])]{

Raised when a node answers with a JSON-RPC error.  @racket[code],
@racket[rpc-message] and @racket[data] are the error's fields.  Nodes
put a reverting call's return data in @racket[data] as a hex string.

@racketblock[
(with-handlers ([exn:fail:web3:rpc?
                 (lambda (e) (abi-revert->string (or (rpc-exn->revert e) (raise e))))])
  (eth-call (hasheq 'to contract 'data "0x") "latest"))
]}

@defproc[(raise-rpc-error [method string?]
                          [code (or/c #f exact-integer?)]
                          [message string?]
                          [data (or/c #f jsexpr?) #f])
         any]{
Raises @racket[exn:fail:web3:rpc] with the given fields and the message
@racket["method: message"], as a provider does for an error response.}

@subsection{Hex Strings}

@defproc[(hex-string? [v any/c]) boolean?]{
Recognizes strings of the form @racket["0x"] followed by zero or more
hexadecimal digits.}

@defproc[(hex-string->number [s string?]) exact-nonnegative-integer?]{
Reads a JSON-RPC quantity.  @racket["0x"] alone reads as @racket[0].
A string without the @racket["0x"] prefix is read as decimal.  Raises
@racket[exn:fail:contract] for a string that is neither.}

@defproc[(number->hex-string [n exact-nonnegative-integer?]) string?]{
Writes @racket[n] as a JSON-RPC quantity, such as @racket["0xbeef"].}

@defproc[(bytes->hex-string [bs bytes?]) string?]{
Writes @racket[bs] as @racket["0x"] followed by two digits per byte.}

@defproc[(hex-string->bytes [s string?]) bytes?]{
Reads data written as hexadecimal, with or without the @racket["0x"]
prefix.  Raises an error if the number of digits is odd.}

@section{Ethereum Endpoints}

@defmodule[web3/eth]

@defthing[block-param/c flat-contract?
          #:value (or/c exact-nonnegative-integer?
                        "latest" "earliest" "pending" "safe" "finalized")]{
A block to query: a block number or one of the named block tags.}

@defproc[(block-param->string [block block-param/c]) string?]{
Writes a block number as a hex quantity and passes tags through.}

@subsection{Node and Chain}

@deftogether[(
@defproc[(web3-client-version [#:provider provider (or/c #f provider?) (current-provider)])
         string?]
@defproc[(net-version [#:provider provider (or/c #f provider?) (current-provider)])
         string?]
@defproc[(net-listening [#:provider provider (or/c #f provider?) (current-provider)])
         boolean?]
@defproc[(net-peer-count [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?]
@defproc[(eth-chain-id [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?]
@defproc[(eth-syncing [#:provider provider (or/c #f provider?) (current-provider)])
         (or/c boolean? hash?)]
@defproc[(eth-gas-price [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?]
@defproc[(eth-accounts [#:provider provider (or/c #f provider?) (current-provider)])
         (listof string?)]
@defproc[(eth-block-number [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?])]{

@tt{web3_clientVersion}, @tt{net_version}, @tt{net_listening},
@tt{net_peerCount}, @tt{eth_chainId}, @tt{eth_syncing},
@tt{eth_gasPrice} (in wei), @tt{eth_accounts} and
@tt{eth_blockNumber}.  @racket[net-version] returns the network id as
the node reports it, a decimal string.}

@deftogether[(
@defproc[(eth-protocol-version [#:provider provider (or/c #f provider?) (current-provider)])
         string?]
@defproc[(eth-coinbase [#:provider provider (or/c #f provider?) (current-provider)])
         string?]
@defproc[(eth-mining [#:provider provider (or/c #f provider?) (current-provider)])
         boolean?]
@defproc[(eth-hashrate [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?])]{

Legacy proof-of-work era methods.  Many clients no longer serve them
since the Merge and answer with an error.}

@defproc[(web3-sha3 [data hex-string?]
                    [#:provider provider (or/c #f provider?) (current-provider)])
         hex-string?]{
Keccak-256 of @racket[data], computed by the node.  To hash locally, use
@racketidfont{keccak256} from the @tt{keccak} package.}

@subsection{State}

@defproc[(eth-get-balance [address string?]
                          [block block-param/c]
                          [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?]{
The balance of @racket[address] in wei.}

@defproc[(eth-get-transaction-count [address string?]
                                    [block block-param/c]
                                    [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?]{
The number of transactions sent from @racket[address]: its next nonce.}

@defproc[(eth-get-storage-at [address string?]
                             [slot exact-nonnegative-integer?]
                             [block block-param/c]
                             [#:provider provider (or/c #f provider?) (current-provider)])
         hex-string?]{
The 32-byte word at storage @racket[slot] of @racket[address].}

@defproc[(eth-get-code [address string?]
                       [block block-param/c]
                       [#:provider provider (or/c #f provider?) (current-provider)])
         hex-string?]{
The code deployed at @racket[address], or @racket["0x"] for an account
without code.}

@subsection{Blocks}

@defproc[(eth-get-block-by-number [block block-param/c]
                                  [full-transactions? boolean?]
                                  [#:provider provider (or/c #f provider?) (current-provider)])
         (or/c 'null hash?)]{
The block, with full transaction objects when
@racket[full-transactions?] is true and transaction hashes otherwise.}

@deftogether[(
@defproc[(eth-get-block-transaction-count-by-hash
          [block-hash string?]
          [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?]
@defproc[(eth-get-block-transaction-count-by-number
          [block block-param/c]
          [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?])]{
The number of transactions in a block; @racket[0] for an unknown block.}

@subsection{Calls and Transactions}

@defproc[(eth-call [tx (and/c hash? jsexpr?)]
                   [block block-param/c]
                   [#:provider provider (or/c #f provider?) (current-provider)])
         hex-string?]{
Executes the call @racket[tx] against the state at @racket[block]
without creating a transaction, and returns its return data.  A revert
raises @racket[exn:fail:web3:rpc] with the revert payload in its
@racket[data] field.}

@defproc[(eth-estimate-gas [tx (and/c hash? jsexpr?)]
                           [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?]{
An estimate of the gas @racket[tx] would use.}

@defproc[(eth-send-transaction [tx (and/c hash? jsexpr?)]
                               [#:provider provider (or/c #f provider?) (current-provider)])
         hex-string?]{
Asks the node to sign and send @racket[tx] from an account it manages,
such as one of anvil's development accounts, and returns the
transaction hash.}

@defproc[(eth-send-raw-transaction [signed-tx hex-string?]
                                   [#:provider provider (or/c #f provider?) (current-provider)])
         hex-string?]{
Submits an already signed, serialized transaction and returns its hash.}

@deftogether[(
@defproc[(eth-get-transaction-by-hash
          [tx-hash hex-string?]
          [#:provider provider (or/c #f provider?) (current-provider)])
         (or/c 'null hash?)]
@defproc[(eth-get-transaction-receipt
          [tx-hash hex-string?]
          [#:provider provider (or/c #f provider?) (current-provider)])
         (or/c 'null hash?)])]{
A transaction, or its receipt once it has been mined.}

@defproc[(eth-get-logs [filter (and/c hash? jsexpr?)]
                       [#:provider provider (or/c #f provider?) (current-provider)])
         list?]{
The logs matching @racket[filter], a hash table with keys such as
@racket['address], @racket['topics], @racket['fromBlock] and
@racket['toBlock].}

@defproc[(wait-for-receipt [tx-hash hex-string?]
                           [#:timeout timeout real? 30.0]
                           [#:interval interval real? 0.002]
                           [#:provider provider (or/c #f provider?) (current-provider)])
         hash?]{
Polls for the receipt of @racket[tx-hash], starting every
@racket[interval] seconds and backing off to every quarter second.
Raises @racket[exn:fail:web3] if no receipt appears within
@racket[timeout] seconds.}

@defproc[(send-transaction/receipt [tx (and/c hash? jsexpr?)]
                                   [#:provider provider (or/c #f provider?) (current-provider)])
         hash?]{
Sends @racket[tx] with @racket[eth-send-transaction] and returns its
receipt once mined.}

@defproc[(receipt-succeeded? [receipt hash?]) boolean?]{
Whether the receipt's status is success, @racket["0x1"], rather than
failure, @racket["0x0"].  Any other status, or none, raises
@racket[exn:fail:web3].}

@section{Addresses}

@defmodule[web3/address]

@defproc[(address->checksum-string [address (abi-value/c 'address)]) string?]{
The @hyperlink["https://eips.ethereum.org/EIPS/eip-55"]{EIP-55} form of @racket[address].
The case of @racket[address] is ignored.

@examples[#:eval abi-eval
          (address->checksum-string "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed")]}

@defproc[(checksum-address? [v any/c]) boolean?]{
Whether @racket[v] is an address string in its EIP-55 form.

@examples[#:eval abi-eval
          (checksum-address? "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed")
          (checksum-address? "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed")]}

@section[#:tag "accounts"]{Accounts and Signatures}

@defmodule[web3/account]

Secret keys, their addresses, and signatures of 32-byte digests, computed locally with
@racketmodname[libsecp256k1], which reads keys from bytes and integers.  No procedure here shows
an argument in an error.

Re-exports @racket[secret-key?], @racket[generate-secret-key], @racket[secret-key->public-key],
@racket[public-key?] and @racket[secp256k1-order] from @racketmodname[libsecp256k1].

@subsection{Keys and Addresses}

@defproc[(hex-string->secret-key [s (and/c string? #px"^(?:0[xX])?[0-9a-fA-F]{64}$")])
         secret-key?]{
Reads a secret key written as 64 hexadecimal digits, with or without @racket["0x"].
Surrounding whitespace is not removed.  Raises @racket[exn:fail:web3:key] when the digits are
zero or not less than @racket[secp256k1-order].

@examples[#:eval account-eval
          (secret-key->address
           (hex-string->secret-key
            "0000000000000000000000000000000000000000000000000000000000000001"))
          (eval:error (hex-string->secret-key (make-string 64 #\0)))]}

@defproc[(secret-key->address [key secret-key?]) checksum-address?]{
The address of the account @racket[key] signs for, in its EIP-55 form.}

@defproc[(public-key->address [key public-key?]) checksum-address?]{
The address of the account whose public key is @racket[key]: the last 20 bytes of the
Keccak-256 hash of the key's 64-byte uncompressed point.

@examples[#:eval account-eval
          (public-key->address (secret-key->public-key key))]}

@subsection{Signatures}

@defstruct*[signature ([r (integer-in 1 (sub1 secp256k1-order))]
                       [s (integer-in 1 (sub1 secp256k1-order))]
                       [y-parity (or/c 0 1)])
            #:transparent]{
An ECDSA signature as Ethereum uses it.  @racket[y-parity] is the parity of the @italic{y}
coordinate of the curve point whose @italic{x} is @racket[r]; it picks out the signer.}

@defproc[(sign-digest [key secret-key?]
                      [digest digest/c])
         signature?]{
Signs @racket[digest] as given, without hashing it.  The nonce is derived from the key and the
digest (RFC 6979), so the same arguments always give the same signature, and @racket[s] is at
most half of @racket[secp256k1-order].}

@defproc[(recover-address [sig signature?]
                          [digest digest/c])
         (or/c #f checksum-address?)]{
The address whose key signed @racket[digest] to give @racket[sig], as the @tt{ecrecover}
precompile computes it, or @racket[#f] when there is none.  Almost every signature recovers
some address, so compare the result with the address expected.  A high @racket[s] is accepted,
as by @tt{ecrecover}; see @racket[signature-low-s?].

@examples[#:eval account-eval
          (define high (signature (signature-r sig)
                                  (- secp256k1-order (signature-s sig))
                                  (- 1 (signature-y-parity sig))))
          (signature-low-s? high)
          (recover-address high digest)]}

@defproc[(signature-low-s? [sig signature?]) boolean?]{
Whether @racket[sig]'s @racket[s] is at most half of @racket[secp256k1-order].  Transactions
require it (EIP-2).}

@defproc[(signature-v [sig signature?]) (or/c 27 28)]{
27 plus @racket[sig]'s y-parity: the @italic{v} of @tt{ecrecover} and of
@racket[signature->bytes].}

@defproc[(signature->bytes [sig signature?])
         (and/c bytes? (property/c bytes-length (=/c 65)))]{
@racket[r] and @racket[s] as 32 big-endian bytes each, then @racket[(signature-v sig)].}

@defproc[(bytes->signature [bs (and/c bytes? (property/c bytes-length (=/c 65)))])
         signature?]{
Reads a signature written as by @racket[signature->bytes].  The last byte may also be the
y-parity itself, 0 or 1.  Raises @racket[exn:fail:web3:signature] for any other last byte, or
when @racket[r] or @racket[s] is zero or not less than @racket[secp256k1-order].

@examples[#:eval account-eval
          (define bs (signature->bytes sig))
          (equal? (bytes->signature (bytes-append (subbytes bs 0 64) (bytes 0))) sig)
          (eval:error (bytes->signature (bytes-append (subbytes bs 0 64) (bytes 37))))]}

@subsection[#:tag "account-errors"]{Errors}

@defstruct*[(exn:fail:web3:key exn:fail:web3) ()]{
Raised for a secret key that is zero or not less than @racket[secp256k1-order].}

@defstruct*[(exn:fail:web3:signature exn:fail:web3) ()]{
Raised for bytes that do not encode a signature, and by @racket[sign-digest] for a signature
whose recovery id Ethereum cannot encode.}

@section[#:tag "rlp"]{Recursive Length Prefix}

@defmodule[web3/rlp]

The @hyperlink["https://ethereum.org/en/developers/docs/data-structures-and-encoding/rlp/"]{RLP}
serialization.  An item is a byte string or a list of items; integers are written as scalars.

@defproc[(rlp-item? [v any/c]) boolean?]{
Whether @racket[v] is a byte string or a list of items.

@examples[#:eval rlp-eval
          (rlp-item? (list #"" (list #"a")))
          (rlp-item? (list "a"))]}

@defproc[(rlp-encode [item rlp-item?]) bytes?]{
The encoding of @racket[item].  A single byte below @racket[#x80] is its own encoding.

@examples[#:eval rlp-eval
          (bytes->hex-string (rlp-encode #"\x7f"))
          (bytes->hex-string (rlp-encode #"\x80"))
          (bytes->hex-string (rlp-encode (list #"" '())))]}

@defproc[(rlp-decode [bs bytes?]) rlp-item?]{
The item @racket[bs] encodes.  @racket[bs] is untrusted: unless it is exactly one canonical
encoding, @racket[rlp-decode] raises @racket[exn:fail:web3:rlp] with one of these reasons:

@itemlist[
 @item{@racket['short]: the input, or the list the item is in, ends before the item does;}
 @item{@racket['single-byte]: a byte below @racket[#x80] written with a prefix;}
 @item{@racket['size]: a length below 56 written in the long form;}
 @item{@racket['leading-zero]: a length written with a leading zero byte;}
 @item{@racket['trailing]: bytes after the item.}]

The reason is the first fault in reading order.  Byte strings in the result are fresh and
mutable.  Nesting is not limited.

@examples[#:eval rlp-eval
          (rlp-decode (bytes #xc3 #x80 #xc1 #x01))
          (eval:error (rlp-decode (bytes #xb8 #x01 #xff)))]}

@defproc[(natural->rlp-scalar [n exact-nonnegative-integer?]) bytes?]{
@racket[n] as big-endian bytes without leading zeros; zero is the empty byte string.

@examples[#:eval rlp-eval
          (natural->rlp-scalar 0)
          (natural->rlp-scalar 1024)]}

@defproc[(rlp-scalar->natural [item rlp-item?]) exact-nonnegative-integer?]{
The integer the scalar @racket[item] writes.  Raises @racket[exn:fail:web3:rlp] with reason
@racket['leading-zero] when @racket[item] starts with a zero byte, and @racket['list] when it is
a list.

@examples[#:eval rlp-eval
          (rlp-scalar->natural #"\4\0")
          (eval:error (rlp-scalar->natural #"\0\4"))]}

@defstruct*[(exn:fail:web3:rlp exn:fail:web3)
            ([offset (or/c #f exact-nonnegative-integer?)]
             [reason (or/c 'short 'single-byte 'size 'leading-zero 'trailing 'list)])]{
Bytes that are not one canonical encoding, or an item that is not a scalar.  @racket[offset]
indexes the bytes passed to @racket[rlp-decode]: where the item at fault starts, or the first
trailing byte.  It is @racket[#f] for @racket[rlp-scalar->natural].}

@section{Contract ABI}

@defmodule[web3/abi]

Encodes and decodes values for the
@hyperlink["https://docs.soliditylang.org/en/latest/abi-spec.html"]{contract ABI}
and decodes reverts.  Also provides everything in
@racketmodname[web3/abi/type], @racketmodname[web3/abi/event-type] and
@racketmodname[web3/abi/description].

@subsection{ABI Types}

@defmodule[web3/abi/type]

Types are values of the structs below.  Wherever a type is expected, a
canonical string such as @racket["(address,uint256)[2]"] or an
s-expression such as @racket['(array (tuple address uint256) 2)] works
too.  This module depends on nothing but @racketmodname[racket/base] and
its contract library, so macros can use it at phase 1.

The reader treats @litchar{[]} as parentheses, so the s-expression form
cannot spell @tt{T[]} as a symbol: write @racket[(array T)].

@racketgrammar*[#:literals (array tuple uint uint8 uint256 int int8 int256 address bool
                            bytes bytes1 bytes32 string)
 [type uint8 ... uint256 uint int8 ... int256 int address bool bytes1 ... bytes32 bytes string
       (array type) (array type length) (tuple type ...+)]]

@deftogether[(@defstruct*[abi-uint ([bits (integer-in 8 256)]) #:transparent]
              @defstruct*[abi-int ([bits (integer-in 8 256)]) #:transparent])]{
@tt{uint}@racket[bits] and @tt{int}@racket[bits]; @racket[bits] is a multiple of 8.}

@deftogether[(@defstruct*[abi-address () #:transparent]
              @defstruct*[abi-bool () #:transparent]
              @defstruct*[abi-bytes () #:transparent]
              @defstruct*[abi-string () #:transparent])]{
@tt{address}, @tt{bool}, @tt{bytes} and @tt{string}.}

@defstruct*[abi-fixed-bytes ([size (integer-in 1 32)]) #:transparent]{
@tt{bytes}@racket[size].}

@deftogether[(@defstruct*[abi-array ([element abi-type-spec/c]) #:transparent]
              @defstruct*[abi-fixed-array ([element abi-type-spec/c]
                                           [length (integer-in 1 (sub1 (expt 2 64)))])
                          #:transparent]
              @defstruct*[abi-tuple ([components (non-empty-listof abi-type-spec/c)])
                          #:transparent])]{
@racket[element]@tt{[]}, @racket[element]@tt{[}@racket[length]@tt{]} and
the tuple of @racket[components].  The constructors accept any type form
and convert it with @racket[abi-type], so the fields hold type structs.

@examples[#:eval abi-eval
          (abi-array "uint")
          (equal? (abi-array 'uint256) (abi-type "uint256[]"))]}

@defproc[(abi-type? [v any/c]) boolean?]{
Recognizes the type structs.}

@defthing[abi-type-spec/c flat-contract?]{
A type struct, string or s-expression.  Only the rough shape is checked;
a string or s-expression that does not parse raises
@racket[exn:fail:web3:abi:type].}

@defproc[(abi-type [spec abi-type-spec/c]) abi-type?]{
The type @racket[spec] denotes.

@examples[#:eval abi-eval
          (abi-type "(address,uint)[2]")
          (abi-type '(array (tuple address uint) 2))
          (eval:error (abi-type "uint7"))]}

@deftogether[(@defproc[(string->abi-type [s string?]) abi-type?]
              @defproc[(sexp->abi-type [v (or/c symbol? pair?)]) abi-type?])]{
Parse one form.  Spaces are allowed next to parentheses and commas, but
not before @litchar{[};
@tt{uint} and @tt{int} mean @tt{uint256} and @tt{int256};
@tt{tuple(...)} means @tt{(...)}.}

@deftogether[(@defproc[(abi-type->string [t abi-type-spec/c]) string?]
              @defproc[(abi-type->sexp [t abi-type-spec/c]) (or/c symbol? pair?)])]{
The canonical forms, as used in selectors.

@examples[#:eval abi-eval
          (abi-type->string "tuple(uint, int)[]")
          (abi-type->sexp "uint8[2][]")]}

@defproc[(abi-type-dynamic? [t abi-type-spec/c]) boolean?]{
Whether @racket[t] is encoded by offset: @tt{bytes}, @tt{string},
@tt{T[]}, and arrays and tuples containing them.}

@defproc[(abi-type-static-size [t abi-type-spec/c]) (or/c #f exact-positive-integer?)]{
Bytes in the encoding of a static type, or @racket[#f] for a dynamic one.

@examples[#:eval abi-eval
          (abi-type-static-size "(uint256,bool)[3]")
          (abi-type-static-size "string")]}

@defproc[(abi-value? [t abi-type-spec/c] [v any/c]) boolean?]{
Whether @racket[v] is a value of @racket[t] (see the table in
@secref["abi-values"]).}

@defproc[(abi-value/c [t abi-type-spec/c]) flat-contract?]{
@racket[abi-value?] as a contract named after @racket[t].}

@defstruct*[abi-signature ([name (and/c string? #px"^[A-Za-z_$][A-Za-z0-9_$]*$")]
                           [inputs (listof abi-type-spec/c)]
                           [outputs (or/c #f (listof abi-type-spec/c))])
            #:transparent]{
A function: @racket[name] is a Solidity identifier and @racket[outputs]
is @racket[#f] when not given.  The constructor converts the types with
@racket[abi-type].}

@defthing[abi-signature-spec/c flat-contract?]{
A signature struct or a string read by @racket[string->abi-signature].}

@defproc[(string->abi-signature [s string?]) abi-signature?]{
Reads @racket["name(T,...)"] or, with outputs,
@racket["name(T,...)(U,...)"].}

@defproc[(abi-signature->string [sig abi-signature?]
                                [#:outputs? outputs? boolean? #f])
         string?]{
The canonical signature; without outputs it is the selector preimage.

@examples[#:eval abi-eval
          (abi-signature->string (string->abi-signature "balanceOf(address)(uint)"))]}

@subsubsection[#:tag "abi-events"]{Events}

@defmodule[web3/abi/event-type]

Like @racketmodname[web3/abi/type], usable at phase 1.

@defstruct*[abi-event-parameter ([name (or/c #f (and/c string? #px"^[A-Za-z_$][A-Za-z0-9_$]*$"))]
                                 [type abi-type-spec/c]
                                 [indexed? boolean?])
            #:transparent]{
One parameter of an event.  @racket[name] is @racket[#f] when unnamed,
and is not a keyword or a type name.  The constructor converts
@racket[type] with @racket[abi-type].}

@defstruct*[abi-event ([name (and/c string? #px"^[A-Za-z_$][A-Za-z0-9_$]*$")]
                       [parameters (listof abi-event-parameter?)]
                       [anonymous? boolean?])
            #:transparent]{
An event: at most three indexed parameters, four when
@racket[anonymous?], with distinct names.  Wherever an event is
expected, a string or s-expression such as the ones below works too.

@racketgrammar*[#:literals (event)
 [event (event name ([param-name type maybe-indexed] ...) maybe-anonymous)]
 [param-name identifier #f]
 [maybe-indexed code:blank #:indexed]
 [maybe-anonymous code:blank #:anonymous]]

@examples[#:eval event-eval
          (string->abi-event "event Transfer(address indexed from, address indexed to, uint256 value)")
          (abi-event->string '(event Deposit ([from address #:indexed] [value uint256] [memo string])))]}

@defthing[abi-event-spec/c flat-contract?]{
An @racket[abi-event], string or s-expression.}

@deftogether[(@defproc[(string->abi-event [s string?]) abi-event?]
              @defproc[(sexp->abi-event [v pair?]) abi-event?])]{
Parse one form.  The string is Solidity's declaration without the
semicolon; @tt{event} is optional, and names of tuple components are
allowed and dropped.}

@deftogether[(@defproc[(abi-event->string [e abi-event-spec/c]) string?]
              @defproc[(abi-event->sexp [e abi-event-spec/c]) pair?])]{
The canonical forms.}

@defproc[(abi-event->signature [e abi-event-spec/c]) abi-signature?]{
The signature whose hash is the event's topic.

@examples[#:eval event-eval
          (abi-signature->string
           (abi-event->signature "Deposit(address indexed from, uint256 value, string memo)"))]}

@subsection[#:tag "abi-values"]{Values}

@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
 (list (list @bold{type} @bold{value})
       (list @tt{uintN} @elem{an exact integer in [0, 2@superscript{N})})
       (list @tt{intN} @elem{an exact integer in [-2@superscript{N-1}, 2@superscript{N-1})})
       (list @tt{address} @elem{a string: @tt{0x} and 40 hex digits in any case; the checksum is not checked})
       (list @tt{bool} @elem{@racket[#t] or @racket[#f]})
       (list @tt{bytesN} @elem{a byte string of exactly N bytes})
       (list @tt{bytes} @elem{a byte string})
       (list @tt{string} @elem{a string, written as UTF-8})
       (list @tt{T[]} @elem{a list of @tt{T} values})
       (list @tt{T[k]} @elem{a list of exactly k @tt{T} values})
       (list @tt{(T1,...,Tn)} @elem{a list of n values, in order}))]

Integers must be exact, and vectors are not lists.

@subsection{Encoding}

@defproc[(abi-encode [types (listof abi-type-spec/c)] [vs list?]) bytes?]{
The standard encoding of @racket[vs] as values of @racket[types].  Raises
@racket[exn:fail:web3:abi:value] when a value does not fit its type or
the lengths differ.

@examples[#:eval abi-eval
          (bytes->hex-string (abi-encode '(int8 bytes3) (list -1 #"abc")))
          (eval:error (abi-encode '(uint8) '(256)))]}

@defproc[(abi-encode-packed [types (listof abi-type-spec/c)] [vs list?]) bytes?]{
Solidity's @tt{abi.encodePacked}: elementary values in their own width,
@tt{bytes} and @tt{string} unpadded, array elements padded to 32 bytes.
Tuples, nested arrays and arrays of dynamic types raise
@racket[exn:fail:web3:abi:type].

@examples[#:eval abi-eval
          (bytes->hex-string (abi-encode-packed '(uint16 bytes1 string) (list 1 #"A" "hi")))]}

@defproc[(encode-call [signature abi-signature-spec/c] [v any/c] ...) bytes?]{
Call data: the selector of @racket[signature] and the encoding of the
@racket[v]s as its inputs.  Outputs in @racket[signature] are ignored.

@examples[#:eval abi-eval
          (bytes->hex-string (encode-call "add(uint256,uint256)" 2 40))]}

@defproc[(function-selector [signature (or/c string? bytes? abi-signature?)]) bytes?]{
The first four bytes of the Keccak-256 hash of the canonical signature.
A string must parse as a signature, or @racket[exn:fail:web3:abi:type] is
raised; a byte string is hashed as given.

@examples[#:eval abi-eval
          (bytes->hex-string (function-selector "transfer(address, uint)"))
          (eval:error (function-selector "foo"))]}

@defproc[(word->bytes [n (integer-in 0 (sub1 (expt 2 256)))]) bytes?]{
@racket[n] as a 32-byte big-endian word.}

@defproc[(bytes->words [bs bytes?]) (listof exact-nonnegative-integer?)]{
Reads @racket[bs] as consecutive 32-byte big-endian words.  A trailing
chunk shorter than 32 bytes is read as a number on its own.}

@subsection{Decoding}

Decoders treat their input as untrusted.  They return the values in
@secref["abi-values"], with addresses checksummed, and raise
@racket[exn:fail:web3:abi:decode] for bytes that are not an encoding:
data shorter than its head, an offset or length past the end, tails that
overlap or point backwards, dirty high bits, a @tt{bool} other than 0 or
1, and invalid UTF-8.  Trailing bytes, gaps between tails and the padding
after @tt{bytes} and @tt{string} are not checked.

@defproc[(abi-decode [types (listof abi-type-spec/c)] [data bytes?]) list?]{
The values @racket[data] encodes as @racket[types].

@examples[#:eval abi-eval
          (abi-decode '(uint8 string) (abi-encode '(uint8 string) '(7 "hi")))
          (abi-decode '(address) (abi-encode '(address) '("0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed")))
          (eval:error (abi-decode '(uint8) (word->bytes 256)))]}

@defproc[(decode-result [signature abi-signature-spec/c] [data bytes?]) list?]{
@racket[data] decoded as the outputs of @racket[signature]; for the hex
string @racket[eth-call] returns, pass @racket[(hex-string->bytes result)].  Raises @racket[exn:fail:web3:abi:type] when
@racket[signature] has no outputs.

@examples[#:eval abi-eval
          (decode-result "balanceOf(address)(uint256)" (word->bytes 1000))]}

@defproc[(decode-call [signature abi-signature-spec/c] [data bytes?]) list?]{
The arguments in call data for @racket[signature]; the inverse of
@racket[encode-call].  A different selector raises
@racket[exn:fail:web3:abi:decode] with reason @racket['selector].

@examples[#:eval abi-eval
          (decode-call "transfer(address,uint256)"
                       (encode-call "transfer(address,uint256)"
                                    "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed" 1000))]}

@subsection{Reverts}

A revert's data is a selector and arguments, like call data.
@tt{Error(string)} and @tt{Panic(uint256)} are always recognised; custom
errors are passed as signatures.

@defstruct*[abi-revert ([signature (or/c #f abi-signature?)]
                        [arguments list?]
                        [data bytes?])
            #:transparent]{
A decoded revert.  When @racket[signature] is an @racket[abi-signature],
@racket[arguments] must be values of its inputs, one for each, or the
constructor raises a contract violation.  @racket[signature] is
@racket[#f], and @racket[arguments] is @racket['()], when the selector is
not recognised or @racket[data] is shorter than four bytes.}

@deftogether[(@defproc[(abi-revert-name [r abi-revert?]) (or/c #f string?)]
              @defproc[(abi-revert-selector [r abi-revert?]) (or/c #f bytes?)])]{
The name of the error, and the first four bytes of its data.}

@defproc[(error-selector [signature abi-signature-spec/c]) bytes?]{
The selector of a custom error.

@examples[#:eval abi-eval
          (bytes->hex-string (error-selector "InsufficientBalance(uint256,uint256)"))]}

@defproc[(decode-revert [data bytes?]
                        [#:errors errors (listof abi-signature-spec/c) '()])
         abi-revert?]{
Decodes revert @racket[data] against the built-in errors and
@racket[errors].  An unrecognised selector is not an error.  Two
signatures with one selector raise @racket[exn:fail:web3:abi:type].

@examples[#:eval abi-eval
          (define data (encode-call "InsufficientBalance(uint256,uint256)" 10 20))
          (abi-revert-arguments
           (decode-revert data #:errors '("InsufficientBalance(uint256,uint256)")))
          (abi-revert->string (decode-revert data))]}

@defproc[(abi-revert->string [r abi-revert?]) string?]{
A readable form of @racket[r].

@examples[#:eval abi-eval
          (abi-revert->string (decode-revert (encode-call "Panic(uint256)" #x11)))]}

@defproc[(panic-description [code exact-nonnegative-integer?]) (or/c #f string?)]{
What a Solidity panic code means, or @racket[#f].

@examples[#:eval abi-eval
          (panic-description #x32)]}

@defproc[(rpc-exn->revert [e exn:fail:web3:rpc?]
                          [#:errors errors (listof abi-signature-spec/c) '()])
         (or/c #f abi-revert?)]{
The revert carried by @racket[e]'s @racket[data]: a hex string,
@racket["Reverted 0x..."], or an object whose @tt{data} is one of those.
No data under the message @tt{execution reverted} is an empty revert.
Gives @racket[#f] when @racket[e] carries no revert.  A payload that does
not decode is returned unrecognised.

@examples[#:eval abi-eval
          (with-handlers ([exn:fail:web3:rpc?
                           (lambda (e) (abi-revert->string (or (rpc-exn->revert e) (raise e))))])
            (raise-rpc-error "eth_call" 3 "execution reverted"
                             (bytes->hex-string (encode-call "Error(string)" "oops!"))))]}

@defproc[(rpc-exn->revert-exn [e exn:fail:web3:rpc?]
                              [#:errors errors (listof abi-signature-spec/c) '()])
         (or/c #f exn:fail:web3:rpc:revert?)]{
@racket[e] as an @racket[exn:fail:web3:rpc:revert], for re-raising, or
@racket[#f].}

@defproc[(exn->abi-revert [e exn?]) (or/c #f abi-revert?)]{
The revert an @racket[exn:fail:web3:rpc:revert] or
@racket[exn:fail:web3:transaction] carries, or @racket[#f].  Where
@racket[rpc-exn->revert] and @racket[rpc-exn->revert-exn] decode an RPC
error's data, this reads the revert decoded when @racket[e] was raised.

@examples[#:eval abi-eval
          (abi-revert->string
           (exn->abi-revert
            (rpc-exn->revert-exn
             (with-handlers ([exn:fail:web3:rpc? values])
               (raise-rpc-error "eth_call" 3 "execution reverted"
                                (bytes->hex-string (encode-call "Error(string)" "oops!")))))))
          (exn->abi-revert (exn:fail "f: no" (current-continuation-marks)))]}

@defstruct*[(exn:fail:web3:rpc:revert exn:fail:web3:rpc) ([revert abi-revert?])]{
A call that reverted, with its decoded @racket[revert].}

@defproc[(decode-revert-reason [data bytes?]) (or/c string? #f)]{
@racket[(abi-revert->string (decode-revert data))] for @tt{Error}
payloads and @racket["Panic(0x11)"] for @tt{Panic} payloads; @racket[#f]
for anything else, including malformed data.}

@subsection[#:tag "abi-errors"]{Errors}

@declare-exporting[web3/abi/type web3/abi]

@defstruct*[(exn:fail:web3:abi exn:fail:web3) ()]{
Raised for ABI failures.}

@defstruct*[(exn:fail:web3:abi:type exn:fail:web3:abi)
            ([input any/c] [position (or/c #f exact-nonnegative-integer?)])]{
A type that does not parse or cannot be used here.  @racket[position]
indexes a string @racket[input].}

@defstruct*[(exn:fail:web3:abi:value exn:fail:web3:abi)
            ([type (or/c abi-type? (listof abi-type?))]
             [value any/c]
             [path (listof exact-nonnegative-integer?)])]{
A value that does not fit @racket[type]; @racket[path] is the argument
index followed by element and component indices.}

@defstruct*[(exn:fail:web3:abi:decode exn:fail:web3:abi)
            ([type (or/c abi-type? (listof abi-type?))]
             [offset exact-nonnegative-integer?]
             [reason (or/c 'short 'offset 'overlap 'length 'non-canonical 'utf-8 'selector 'topic)])]{
Bytes that do not decode.  @racket[offset] indexes the data passed in;
@racket[type] is the type being read there.}

@subsection{Conformance Vectors}

@defmodule[web3/abi/vectors]

@defstruct*[abi-vector ([mode (or/c 'encode 'packed 'call 'decode)]
                        [name string?]
                        [signature string?]
                        [values list?]
                        [hex string?])]{
One case: @racket[hex] is what @racket[abi-encode],
@racket[abi-encode-packed] or @racket[encode-call] produces for
@racket[values] and the inputs of @racket[signature].}

@defthing[abi-encode-vectors (listof abi-vector?)]{
The cases, checked against Foundry's @tt{cast}.}

@defthing[abi-decode-vectors (listof abi-vector?)]{
Inputs in mode @racket['decode]: @racket[hex] decodes to @racket[values]
but need not be canonical.}

@defstruct*[abi-reject-vector ([name string?]
                               [signature string?]
                               [hex string?]
                               [reason symbol?]
                               [offset exact-nonnegative-integer?]
                               [type (or/c string? (listof string?))])]{
Bytes that do not decode as the inputs of @racket[signature]: decoding
raises @racket[exn:fail:web3:abi:decode] with @racket[reason] at
@racket[offset], and the type there is @racket[type], or the list of
input types when the whole head is short.
@racket['overlap] and @racket['utf-8] rows are this library's rules;
Solidity's decoder accepts them.}

@defthing[abi-reject-vectors (listof abi-reject-vector?)]{
The rejected inputs.}

@subsection{ABI Descriptions}

@defmodule[web3/abi/description]

A contract's interface: its functions, events, errors, constructor,
fallback and receive, as @deftech{items}.  An item is written as a struct,
a Solidity declaration string, an s-expression or a JSON ABI object, and
every form converts to every other.  Like @racketmodname[web3/abi/type],
this module can be used at phase 1.

@racketgrammar*[#:literals (function error constructor fallback receive event)
 [item (function name (param ...) clause ...)
       (error name (param ...))
       (constructor (param ...) maybe-payable)
       (fallback maybe-payable)
       (receive)
       event]
 [clause (code:line #:returns (param ...)) #:view #:pure #:payable]
 [param [param-name type]]
 [param-name identifier #f]
 [maybe-payable code:blank #:payable]]

@defproc[(abi-parameter [name (or/c #f abi-parameter-name/c)] [type abi-type-spec/c]) abi-parameter?]{
A named or unnamed parameter.  @racket[name] is an identifier that is not a
keyword or a type.}

@deftogether[(@defproc[(abi-parameter? [v any/c]) boolean?]
              @defproc[(abi-parameter-name [p abi-parameter?]) (or/c #f string?)]
              @defproc[(abi-parameter-type [p abi-parameter?]) abi-type?])]{}

@defproc[(abi-function [name abi-identifier/c]
                       [inputs (listof (or/c abi-parameter? abi-type-spec/c))]
                       [outputs (listof (or/c abi-parameter? abi-type-spec/c))]
                       [state-mutability (or/c 'pure 'view 'nonpayable 'payable) 'nonpayable])
         abi-function?]{
A function.  An @racket[abi-function] is an @racket[abi-signature] with its
outputs, so it works wherever a signature does: @racket[encode-call],
@racket[decode-result], @racket[contract-call].  A type in place of a
parameter is an unnamed parameter.  Two parameters with one name raise
@racket[exn:fail:web3:abi:type].

@examples[#:eval abi-eval
 (define balance-of
   (abi-function "balanceOf" (list (abi-parameter "owner" 'address)) '(uint256) 'view))
 balance-of
 (abi-signature->string balance-of #:outputs? #t)
 (bytes->hex-string (function-selector balance-of))]}

@deftogether[(@defproc[(abi-function? [v any/c]) boolean?]
              @defproc[(abi-function-parameters [f abi-function?]) (listof abi-parameter?)]
              @defproc[(abi-function-returns [f abi-function?]) (listof abi-parameter?)]
              @defproc[(abi-function-state-mutability [f abi-function?])
                       (or/c 'pure 'view 'nonpayable 'payable)])]{}

@defproc[(abi-error [name abi-identifier/c] [inputs (listof (or/c abi-parameter? abi-type-spec/c))])
         abi-error?]{
A custom error; also an @racket[abi-signature], for @racket[decode-revert]'s
@racket[#:errors].}

@deftogether[(@defproc[(abi-error? [v any/c]) boolean?]
              @defproc[(abi-error-parameters [e abi-error?]) (listof abi-parameter?)])]{}

@defproc[(abi-constructor [inputs (listof (or/c abi-parameter? abi-type-spec/c))]
                          [state-mutability (or/c 'nonpayable 'payable) 'nonpayable])
         abi-constructor?]{
A constructor; also the @racket["constructor(...)"] signature
@racket[deploy-contract] takes.}

@deftogether[(@defproc[(abi-constructor? [v any/c]) boolean?]
              @defproc[(abi-constructor-parameters [c abi-constructor?]) (listof abi-parameter?)]
              @defproc[(abi-constructor-state-mutability [c abi-constructor?])
                       (or/c 'nonpayable 'payable)]
              @defproc[(abi-fallback [state-mutability (or/c 'nonpayable 'payable) 'nonpayable])
                       abi-fallback?]
              @defproc[(abi-fallback? [v any/c]) boolean?]
              @defproc[(abi-fallback-state-mutability [f abi-fallback?]) (or/c 'nonpayable 'payable)]
              @defproc[(abi-receive) abi-receive?]
              @defproc[(abi-receive? [v any/c]) boolean?])]{
The constructor, fallback and receive items.}

@defproc[(abi-item? [v any/c]) boolean?]{Recognizes items, @racket[abi-event] included.}

@defthing[abi-item-spec/c flat-contract?]{An item, a string or an s-expression:
@racket[(or/c abi-item? string? pair?)].}

@defproc[(abi-item [spec abi-item-spec/c]) abi-item?]{The item @racket[spec] denotes.}

@deftogether[(@defproc[(string->abi-item [s string?]) abi-item?]
              @defproc[(sexp->abi-item [v pair?]) abi-item?])]{
Parse one item.  The string is a Solidity declaration without its
semicolon, starting with @tt{function}, @tt{event}, @tt{error},
@tt{constructor}, @tt{fallback} or @tt{receive}; @tt{external},
@tt{public}, data locations and names of tuple components are allowed and
dropped.  Anything else raises @racket[exn:fail:web3:abi:type], with the
position of the offending token for a string.

@examples[#:eval abi-eval
 (string->abi-item "function balanceOf(address owner) external view returns (uint256)")
 (eval:error (string->abi-item "function balanceOf(address owner) veiw returns (uint256)"))]}

@deftogether[(@defproc[(abi-item->string [i abi-item-spec/c]) string?]
              @defproc[(abi-item->sexp [i abi-item-spec/c]) pair?])]{
The canonical forms.

@examples[#:eval abi-eval
 (abi-item->string '(function transfer ([to address] [amount uint256]) #:returns ([#f bool])))
 (abi-item->sexp "constructor(string name_, uint256 supply) payable")]}

@deftogether[(@defproc[(jsexpr->abi-item [v any/c]) abi-item?]
              @defproc[(abi-item->jsexpr [i abi-item-spec/c]) hash?])]{
An item of a Solidity JSON ABI.  @tt{internalType} and the names of tuple
components are not kept; the legacy @tt{constant} and @tt{payable} keys
are read.  Anything else raises @racket[exn:fail:web3:abi:type].

@examples[#:eval abi-eval
 (abi-item->jsexpr "error InsufficientBalance(uint256 have, uint256 want)")]}

@defproc[(contract-abi [items (listof abi-item-spec/c)]
                       [#:bytecode bytecode
                                   (or/c #f bytes? (and/c string? #px"^0x(?:[0-9a-fA-F]{2})*$"))
                                   #f])
         contract-abi?]{
A contract's description: its items, in order, and the init code that
deploys it, if known; empty bytecode is none.  Errors with one signature,
and events with one signature, anonymity and indexed positions, are
repeats, as solc lists one declared by two libraries under any parameter
names; the first is kept.  Two functions with one signature, two events
with one signature, anonymity and number of indexed parameters but
different indexed positions, or two constructors, fallbacks or receives,
raise @racket[exn:fail:web3:abi:type].

@examples[#:eval abi-eval
 (define erc20
   (contract-abi
    (list "function balanceOf(address owner) view returns (uint256)"
          "function transfer(address to, uint256 amount) returns (bool)"
          "event Transfer(address indexed from, address indexed to, uint256 value)")))
 erc20
 (bytes->hex-string (encode-call (contract-abi-function erc20 'transfer)
                                 "0x70997970C51812dc3A010C7d01b50e0d17dc79C8" 250))]}

@deftogether[(@defproc[(contract-abi? [v any/c]) boolean?]
              @defproc[(contract-abi-items [d contract-abi?]) (listof abi-item?)]
              @defproc[(contract-abi-bytecode [d contract-abi?]) (or/c #f bytes?)]
              @defproc[(contract-abi-functions [d contract-abi?]) (listof abi-function?)]
              @defproc[(contract-abi-events [d contract-abi?]) (listof abi-event?)]
              @defproc[(contract-abi-errors [d contract-abi?]) (listof abi-error?)]
              @defproc[(contract-abi-constructor [d contract-abi?]) (or/c #f abi-constructor?)]
              @defproc[(contract-abi-fallback [d contract-abi?]) (or/c #f abi-fallback?)]
              @defproc[(contract-abi-receive [d contract-abi?]) (or/c #f abi-receive?)])]{
The items of each kind, in order.}

@defproc[(contract-abi-function [d contract-abi?]
                                [key (or/c symbol? string? abi-signature?)]
                                [#:arity arity exact-nonnegative-integer?])
         abi-function?]{
The function named @racket[key], with the signature @racket[key], or
@racket[key] itself.  An overloaded name needs @racket[arity], the number
of inputs, to pick one.  A key that names no function, or more than one,
raises @racket[exn:fail:web3:abi].

@examples[#:eval abi-eval
 (define nft
   (contract-abi
    (list "function safeTransferFrom(address from, address to, uint256 id)"
          "function safeTransferFrom(address from, address to, uint256 id, bytes data)")))
 (abi-item->string (contract-abi-function nft 'safeTransferFrom #:arity 4))
 (abi-item->string (contract-abi-function nft "safeTransferFrom(address,address,uint256)"))
 (eval:error (contract-abi-function nft 'safeTransferFrom))]}

@deftogether[(@defproc[(contract-abi-event [d contract-abi?]
                                            [key (or/c symbol? string? abi-event?)])
                        abi-event?]
              @defproc[(contract-abi-error [d contract-abi?]
                                            [key (or/c symbol? string? abi-signature?)])
                        abi-error?])]{
The event or error named @racket[key], with that signature, or
@racket[key] itself; anything else raises @racket[exn:fail:web3:abi].}

@defproc[(jsexpr->contract-abi [v any/c]) contract-abi?]{
A description from a JSON ABI (a list of items) or a compiled artifact with
an @tt{abi}, as forge, solc, Hardhat and Truffle write them; the bytecode
is read from the artifact's @tt{bytecode}, @tt{bytecode.object},
@tt{evm.bytecode.object} or @tt{bin}, the first present and not
@tt{null}.  Repeated and duplicate items are as for
@racket[contract-abi].  Bytecode with unlinked library references, a
bytecode key holding the wrong kind of value, and anything else raise
@racket[exn:fail:web3:abi:type].}

@defproc[(contract-abi->jsexpr [d contract-abi?]) (listof hash?)]{
The JSON ABI of @racket[d].

@examples[#:eval abi-eval
 (contract-abi->jsexpr (contract-abi '((function count () #:view #:returns ([#f uint256])))))]}

@section{Contracts}

@defmodule[web3/contract]

Deploying contracts and calling their functions from accounts the node
manages.  Signatures are written as for @racket[encode-call]; a call's
outputs follow its inputs, as in @racket["balanceOf(address)(uint256)"].
Arguments and results are as in @secref["abi-values"].  A revert raises
@racket[exn:fail:web3:rpc:revert], decoded against @racket[errors] and the
built-in @tt{Error} and @tt{Panic}.

@defparam[current-account account (or/c #f (abi-value/c 'address)) #:value #f]{
The sender when no @racket[#:from] is given.  Sending with no sender raises
@racket[exn:fail:web3]; a call with none has no @tt{from}.}

@defproc[(contract-call [target (or/c (abi-value/c 'address) contract-instance?)]
                        [function (or/c string? symbol? abi-signature?)]
                        [arg any/c] ...
                        [#:from from (or/c #f (abi-value/c 'address)) (current-account)]
                        [#:value value exact-nonnegative-integer? 0]
                        [#:gas gas (or/c #f exact-positive-integer?) #f]
                        [#:block block block-param/c "latest"]
                        [#:errors errors (listof (or/c string? abi-signature?)) '()]
                        [#:provider provider (or/c #f provider?) (current-provider)])
         any]{
Calls @racket[function] at @racket[target] with @racket[eth-call] and
returns its outputs as multiple values: one value per output, none for
@tt{()}.  @racket[function] must give its outputs.  Nothing is sent.  With
a @racket[contract-instance], @racket[function] may be a name, resolved by
its number of arguments, and the instance's ABI errors, provider and sender
are the defaults.  With an address, @racket[function] must be a signature,
with its parentheses; a name is a contract violation.  An instance looks
up names and signature strings in its ABI, and one it does not hold raises
@racket[exn:fail:web3:abi]; an @racket[abi-signature] or
@racket[abi-function] value is used as given.  A @racket[value] above 0
for an @racket[abi-function], or a function looked up, that is not
@tt{payable} raises @racket[exn:fail:web3:abi].

@examples[#:eval contract-eval
          (define token "0x5FbDB2315678afecb367f032d93F642f64180aa3")
          (define node
            (mock-provider
             (hash "eth_call" (bytes->hex-string (abi-encode '(string uint8) '("Token" 18))))))
          (contract-call token "meta()(string,uint8)" #:provider node)
          (define refusing
            (mock-provider
             (lambda (method params)
               (raise-rpc-error method 3 "execution reverted"
                                (bytes->hex-string
                                 (encode-call "InsufficientBalance(uint256,uint256)" 0 5))))))
          (eval:error
           (contract-call token "transfer(address,uint256)(bool)" token 5
                          #:errors '("InsufficientBalance(uint256,uint256)")
                          #:provider refusing))]}

@defproc[(contract-send [target (or/c (abi-value/c 'address) contract-instance?)]
                        [function (or/c string? symbol? abi-signature?)]
                        [arg any/c] ...
                        [#:from from (or/c #f (abi-value/c 'address)) (current-account)]
                        [#:value value exact-nonnegative-integer? 0]
                        [#:gas gas (or/c #f exact-positive-integer?) #f]
                        [#:errors errors (listof (or/c string? abi-signature?)) '()]
                        [#:provider provider (or/c #f provider?) (current-provider)])
         (and/c hash? jsexpr?)]{
Sends a transaction calling @racket[function] from @racket[from], an
account the node manages, and returns its receipt once mined.  Outputs in
@racket[function] are ignored.  With a @racket[contract-instance],
@racket[function] may be a name, resolved by its number of arguments, and
the instance's ABI errors, provider and sender are the defaults; names,
signatures and @racket[value] are checked as for @racket[contract-call].
Without @racket[gas] the gas is estimated first, so a transaction that would revert raises
@racket[exn:fail:web3:rpc:revert] and is not sent.  A transaction mined with
status 0 raises @racket[exn:fail:web3:transaction].  A malformed or zero
estimate raises @racket[exn:fail:web3] and nothing is sent.

Once the transaction is sent, the receipt is awaited for up to 30 seconds.
When waiting fails, or the receipt cannot be read, the error's message
names the transaction's hash: the transaction was sent and its outcome is
unknown, so sending it again may send it twice.  The error is copied as
the most specific of @racket[exn:fail:web3:rpc:revert],
@racket[exn:fail:web3:rpc] and @racket[exn:fail:web3] it is an instance
of, with that type's fields: a further subtype, such as one a provider
defines, is not kept, and any other error becomes an
@racket[exn:fail:web3].

@racketblock[
(define receipt (contract-send token "transfer(address,uint256)" bob 250 #:from alice))
(receipt-succeeded? receipt)
]}

@deftogether[(
@defproc[(deploy-contract [init-code (or/c bytes? (and/c string? #px"^0x(?:[0-9a-fA-F]{2})*$"))]
                          [constructor (or/c string? abi-signature?) "constructor()"]
                          [arg any/c] ...
                          [#:from from (or/c #f (abi-value/c 'address)) (current-account)]
                          [#:value value exact-nonnegative-integer? 0]
                          [#:gas gas (or/c #f exact-positive-integer?) #f]
                          [#:errors errors (listof (or/c string? abi-signature?)) '()]
                          [#:provider provider (or/c #f provider?) (current-provider)])
         checksum-address?]
@defproc[(deploy-contract/receipt
          [init-code (or/c bytes? (and/c string? #px"^0x(?:[0-9a-fA-F]{2})*$"))]
          [constructor (or/c string? abi-signature?) "constructor()"]
          [arg any/c] ...
          [#:from from (or/c #f (abi-value/c 'address)) (current-account)]
          [#:value value exact-nonnegative-integer? 0]
          [#:gas gas (or/c #f exact-positive-integer?) #f]
          [#:errors errors (listof (or/c string? abi-signature?)) '()]
          [#:provider provider (or/c #f provider?) (current-provider)])
         (values checksum-address? (and/c hash? jsexpr?))])]{
Deploys @racket[init-code] with the @racket[arg]s encoded as the inputs of
@racket[constructor], a signature named @tt{constructor}, and returns the
new contract's address, and with @racket[deploy-contract/receipt] its
receipt.  Gas and failures are as for @racket[contract-send].

@racketblock[
(define token
  (deploy-contract token-code "constructor(string,string,uint256)" "Token" "TOK" 1000000))
]}

@defproc[(create-address [sender (abi-value/c 'address)]
                         [nonce (integer-in 0 (- (expt 2 64) 2))])
         checksum-address?]{
The address of the contract @racket[sender] creates with @racket[nonce].  That is the last 20
bytes of the Keccak-256 hash of the RLP list of @racket[sender]'s bytes and
@racket[(natural->rlp-scalar nonce)]; see @secref["guide-rlp"].

@examples[#:eval contract-eval
          (create-address "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266" 0)]}

@defproc[(create2-address [deployer (abi-value/c 'address)]
                          [salt (or/c (and/c bytes? (property/c bytes-length (=/c 32)))
                                      (integer-in 0 (sub1 (expt 2 256))))]
                          [init-code (or/c bytes? (and/c string? #px"^0x(?:[0-9a-fA-F]{2})*$"))])
         checksum-address?]{
The address @tt{CREATE2} gives @racket[init-code] deployed by
@racket[deployer] with @racket[salt], 32 bytes or an integer.

@examples[#:eval contract-eval
          (create2-address "0x0000000000000000000000000000000000000000" 0 #"\0")]}

@defstruct*[(exn:fail:web3:transaction exn:fail:web3)
            ([receipt (and/c hash? jsexpr?)] [revert (or/c #f abi-revert?)])]{
A transaction that was mined and failed.  @racket[revert] is what replaying
it with @racket[eth-call] at its block reverted with, or @racket[#f] when the
replay did not revert or could not be made.  The replay runs after the whole
block, so it may not reproduce the failure when other transactions in the
block changed the state it depends on.}

@subsection{Contract Instances}

@defproc[(contract-instance [abi contract-abi?] [address (abi-value/c 'address)]
                            [#:provider provider (or/c #f provider?) #f]
                            [#:from from (or/c #f (abi-value/c 'address)) #f])
         contract-instance?]{
A contract at @racket[address] described by @racket[abi]:
@racket[contract-call], @racket[contract-send],
@racket[contract-estimate-gas] and @racket[contract-logs] take it with a
function or event name.  Its errors decode every revert; @racket[provider]
and @racket[from], when given, replace @racket[current-provider] and
@racket[current-account].

@examples[#:eval contract-eval
 (define token-abi
   (contract-abi (list "function balanceOf(address owner) view returns (uint256)"
                       "function transfer(address to, uint256 amount) returns (bool)"
                       "error InsufficientBalance(uint256 have, uint256 want)")))
 (define node (mock-provider (hash "eth_call" (bytes->hex-string (abi-encode '(uint256) '(1000))))))
 (define gold
   (contract-instance token-abi "0x5fbdb2315678afecb367f032d93f642f64180aa3" #:provider node))
 gold
 (contract-call gold 'balanceOf "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")
 (mock-provider-calls node)
 (define refusing
   (mock-provider
    (lambda (method params)
      (raise-rpc-error method 3 "execution reverted"
                       (bytes->hex-string (encode-call "InsufficientBalance(uint256,uint256)" 0 5))))))
 (eval:error (contract-send gold 'transfer "0x70997970C51812dc3A010C7d01b50e0d17dc79C8" 5
                            #:from "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"
                            #:provider refusing))]}

@deftogether[(@defproc[(contract-instance? [v any/c]) boolean?]
              @defproc[(contract-instance-abi [i contract-instance?]) contract-abi?]
              @defproc[(contract-instance-address [i contract-instance?]) checksum-address?]
              @defproc[(contract-instance-provider [i contract-instance?]) (or/c #f provider?)]
              @defproc[(contract-instance-from [i contract-instance?])
                       (or/c #f (abi-value/c 'address))])]{}

@defproc[(contract-estimate-gas [target (or/c (abi-value/c 'address) contract-instance?)]
                                [function (or/c string? symbol? abi-signature?)]
                                [arg any/c] ...
                                [#:from from (or/c #f (abi-value/c 'address)) (current-account)]
                                [#:value value exact-nonnegative-integer? 0]
                                [#:errors errors (listof (or/c string? abi-signature?)) '()]
                                [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?]{
The gas a @racket[contract-send] of the same arguments would be given.  A
call that would revert raises @racket[exn:fail:web3:rpc:revert].}

@defproc[(contract-logs [target (or/c (abi-value/c 'address) contract-instance?)]
                        [event (or/c #f abi-event-spec/c symbol?) #f]
                        [#:where where hash? (hasheq)]
                        [#:from-block from block-param/c "earliest"]
                        [#:to-block to block-param/c "latest"]
                        [#:chunk chunk (or/c #f exact-positive-integer?) #f]
                        [#:provider provider (or/c #f provider?) (current-provider)])
         (listof log?)]{
The logs @racket[target] emitted, as @racket[get-logs] gives them:
@racket[event]'s, filtered by @racket[where], or all of them, decoded with
an instance's events.  A name is allowed only with an instance, and
@racket[where] only with an @racket[event]; anything else is a contract
violation.  A name the instance's ABI does not hold, and a @racket[where]
that names no indexed parameter of @racket[event], raise
@racket[exn:fail:web3:abi].}

@defproc[(deploy-contract-instance [abi (and/c contract-abi? contract-abi-bytecode)]
                                   [arg any/c] ...
                                   [#:from from (or/c #f (abi-value/c 'address)) (current-account)]
                                   [#:value value exact-nonnegative-integer? 0]
                                   [#:gas gas (or/c #f exact-positive-integer?) #f]
                                   [#:errors errors (listof (or/c string? abi-signature?)) '()]
                                   [#:provider provider (or/c #f provider?) (current-provider)])
         contract-instance?]{
Deploys @racket[abi]'s bytecode with the @racket[arg]s for its
constructor, as @racket[deploy-contract] does, and returns the new contract
as an instance.  A @racket[value] above 0 for a constructor that is not
payable is a contract violation.}

@deftogether[(@defproc[(read-contract-abi [in input-port? (current-input-port)]) contract-abi?]
              @defproc[(file->contract-abi [path path-string?]) contract-abi?])]{
@racket[jsexpr->contract-abi] of the JSON in @racket[in] or the file
@racket[path].  Text that is not one JSON value raises
@racket[exn:fail:web3:abi:type]; a file that cannot be read raises
@racket[exn:fail:filesystem].}

@subsection[#:tag "binding-contracts"]{Binding Contracts}

@defform[#:literals (function error constructor fallback receive event)
         (define-contract id clause ...+)
         #:grammar ([clause (code:line #:file path-string)
                            (code:line item ...+)
                            (code:line #:bytecode bytecode-expr)
                            (code:line #:rename ([key rename-id] ...))]
                    [item string-literal sexp-item]
                    [key string-literal])
         #:contracts ([bytecode-expr
                       (or/c #f bytes? (and/c string? #px"^0x(?:[0-9a-fA-F]{2})*$"))])]{
Binds @racket[id] to a @racket[contract-abi], read from the JSON ABI or
artifact @racket[path-string] (relative to the enclosing file) when the
module is compiled, or written as @tech{items}.  It also binds, for each
function, a procedure named @racket[id] followed by the function's name in
the table below; for each event, that name followed by
@racketidfont{-event}, bound to the @racket[abi-event]; and, when the
bytecode is known, @racket[id]@racketidfont{-deploy}, which deploys it
with the constructor's inputs and returns the address.  There is one
source, @racket[#:file] or the items, which come together, and at most one
@racket[#:bytecode] and one @racket[#:rename], before or after it.  With
@racket[#:bytecode], @racket[id]@racketidfont{-deploy} is bound even when
@racket[bytecode-expr] gives none, and then raises @racket[exn:fail:web3].

A function procedure takes the contract's address and the function's
inputs: a @tt{view} or @tt{pure} function is a @racket[contract-call],
any other a @racket[contract-send], with their keywords (@racket[#:value]
only for a @tt{payable} function) and the ABI's errors.  Overloads with
different numbers of inputs share one procedure; @racket[#:rename] gives a
function (@racket["name"]), an overload (@racket["name(T,...)"]) or an
event (@racket["event Name"], or @racket["event Name(T,...)"] when the name
is overloaded) another name.  Arity and keywords are checked where a
procedure is applied, and mistakes in the items are syntax errors at the
item.  Two events with one signature, which a @racket[contract-abi] allows
when they index different numbers of parameters, cannot be bound.

@tabular[#:sep @hspace[2]
         (list (list @bold{Solidity} @bold{bound, with @racket[id] @racketidfont{token}})
               (list @tt{balanceOf} @racketidfont{token-balance-of})
               (list @tt{tokenURI} @racketidfont{token-token-uri})
               (list @tt{DOMAIN_SEPARATOR} @racketidfont{token-domain-separator})
               (list @tt{onERC721Received} @racketidfont{token-on-erc721-received})
               (list @tt{IPFSHash} @racketidfont{token-ipfs-hash})
               (list @tt{v2Swap} @racketidfont{token-v2-swap})
               (list @tt{__ERC20_init} @racketidfont{token-erc20-init})
               (list @tt{balanceOf$} @racketidfont{token-balance-of$})
               (list @elem{event @tt{Transfer}} @racketidfont{token-transfer-event})
               (list "the bytecode" @racketidfont{token-deploy}))]

Words split at underscores, before an uppercase letter that follows a
lowercase letter or a digit, and before the last letter of an uppercase run
followed by a lowercase letter; they are lowercased and joined with
@litchar{-}.  Two bindings with one name are a syntax error naming both,
and a name with no words, such as @tt{_}, needs @racket[#:rename].

@examples[#:eval contract-eval
 (define-contract erc20
   "function balanceOf(address owner) view returns (uint256)"
   "function transfer(address to, uint256 amount) returns (bool)"
   "error InsufficientBalance(uint256 have, uint256 want)")
 (define token "0x5FbDB2315678afecb367f032d93F642f64180aa3")
 (define node (mock-provider (hash "eth_call" (bytes->hex-string (abi-encode '(uint256) '(1000))))))
 (erc20-balance-of token "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266" #:provider node)
 (eval:error (erc20-transfer token token 5 #:from token #:provider refusing))
 (eval:error (erc20-balance-of token))]

@racketblock[
(define-contract token #:file "contracts/out/Token.sol/Token.json")
(define gold (token-deploy "Gold" "GLD" 1000000))
(token-balance-of gold alice)
]}

@defform[(contract-bindings-out id ...)]{
In @racket[provide]: each @racket[id] and every name its
@racket[define-contract] bound.  @racket[(all-defined-out)] provides them
too.}

@section{Events}

@defmodule[web3/event]

Reading the logs contracts emit.  Also provides everything in
@racketmodname[web3/abi/event] and @racketmodname[web3/eth/log].

@subsection{Logs}

@defmodule[web3/eth/log]

@defproc[(log? [v any/c]) boolean?]{
Recognizes logs.  A log prints with its address, position and
abbreviated topics.}

@deftogether[(@defproc[(log-address [l log?]) string?]
              @defproc[(log-topics [l log?]) (listof string?)]
              @defproc[(log-data [l log?]) bytes?]
              @defproc[(log-block-number [l log?]) (or/c #f exact-nonnegative-integer?)]
              @defproc[(log-block-hash [l log?]) (or/c #f string?)]
              @defproc[(log-block-timestamp [l log?]) (or/c #f exact-nonnegative-integer?)]
              @defproc[(log-transaction-hash [l log?]) (or/c #f string?)]
              @defproc[(log-transaction-index [l log?]) (or/c #f exact-nonnegative-integer?)]
              @defproc[(log-index [l log?]) (or/c #f exact-nonnegative-integer?)]
              @defproc[(log-removed? [l log?]) boolean?])]{
The fields of a log: a checksummed address, up to four lowercase
topics, the data, and its place in the chain, @racket[#f] while pending.
@racket[log-removed?] is true for a log undone by a reorg.}

@defproc[(jsexpr->log [v any/c]) log?]{
A log from the node's JSON.  Raises @racket[exn:fail:web3] for anything
else.  Unknown fields are kept in @racket[log->jsexpr].

@examples[#:eval event-eval
          (define transfer-json
            (hasheq 'address "0x5fbdb2315678afecb367f032d93f642f64180aa3"
                    'topics (list "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"
                                  "0x000000000000000000000000f39fd6e51aad88f6f4ce6ab8827279cfffb92266"
                                  "0x00000000000000000000000070997970c51812dc3a010c7d01b50e0d17dc79c8")
                    'data "0x00000000000000000000000000000000000000000000000000000000000000fa"
                    'blockNumber "0x2" 'logIndex "0x0" 'transactionIndex "0x0" 'removed #f
                    'blockHash (string-append "0x" (make-string 64 #\a))
                    'transactionHash (string-append "0x" (make-string 64 #\b))))
          (jsexpr->log transfer-json)]}

@defproc[(log->jsexpr [l log?]) hash?]{
The node's object @racket[l] was read from.}

@subsection{Topics and Decoding}

@defmodule[web3/abi/event]

@defproc[(event-topic [e abi-event-spec/c]) (or/c #f (and/c string? #px"^0x[0-9a-fA-F]{64}$"))]{
Topic 0 of @racket[e]'s logs, or @racket[#f] for an anonymous event,
which has none.

@examples[#:eval event-eval
          (event-topic "Transfer(address indexed from, address indexed to, uint256 value)")]}

@defproc[(abi-value->topic [t abi-type-spec/c] [v any/c]) (and/c string? #px"^0x[0-9a-fA-F]{64}$")]{
The topic of @racket[v] as an indexed parameter of type @racket[t]: its
word for a value type, otherwise the Keccak-256 hash of its in-place
encoding, so @tt{string}, @tt{bytes}, arrays and structs cannot be
recovered from a log.

@examples[#:eval event-eval
          (abi-value->topic 'string "hello")]}

@defproc[(event-topics [e abi-event-spec/c] [where hash? (hasheq)])
         (and/c (listof (or/c #f
                                       (and/c string? #px"^0x[0-9a-fA-F]{64}$")
                                       (non-empty-listof (and/c string? #px"^0x[0-9a-fA-F]{64}$"))))
                       (property/c length (<=/c 4)))]{
Topic positions matching @racket[e] with the indexed parameters in
@racket[where], keyed by name or position; @racket[#f] matches anything
and a list matches any of its topics.  A key that is not an indexed
parameter of @racket[e] is a contract violation.

@examples[#:eval event-eval
          (event-topics "Transfer(address indexed from, address indexed to, uint256 value)"
                        (hasheq 'to "0x70997970C51812dc3A010C7d01b50e0d17dc79C8"))]}

@defproc[(any-of [v any/c] ...+) any-of?]{
Any of the @racket[v]s, as a value in @racket[where].}

@deftogether[(@defproc[(any-of? [v any/c]) boolean?]
              @defproc[(any-of-values [a any-of?]) (non-empty-listof any/c)])]{
Recognizes the result of @racket[any-of], and gives its values.}

@defstruct*[indexed-hash ([topic (and/c string? #px"^0x[0-9a-fA-F]{64}$")]) #:transparent]{
The value of an indexed parameter whose type is hashed.  The constructor
lowercases @racket[topic].}

@defproc[(decode-log [e abi-event-spec/c] [l log?]) event-log?]{
Decodes @racket[l] as @racket[e].  Raises
@racket[exn:fail:web3:abi:decode] with reason @racket['topic] when the
topics do not match; offsets count bytes in the topics followed by the
data.

@examples[#:eval event-eval
          (define sent
            (decode-log "Transfer(address indexed from, address indexed to, uint256 value)"
                        (jsexpr->log transfer-json)))
          sent
          (event-log-ref sent 'value)]}

@defproc[(decode-logs [events (listof abi-event-spec/c)] [logs (listof log?)]) (listof log?)]{
Each log decoded as the one event in @racket[events] with its topic 0
and number of topics, or returned unchanged when there is none or it
does not decode.  Two events that cannot be told apart raise
@racket[exn:fail:web3:abi:type].}

@defproc[(event-log? [v any/c]) boolean?]{
A decoded log; also a @racket[log?].}

@deftogether[(@defproc[(event-log-event [el event-log?]) abi-event?]
              @defproc[(event-log-name [el event-log?]) string?]
              @defproc[(event-log-arguments [el event-log?]) list?]
              @defproc[(event-log-ref [el event-log?]
                                      [key (or/c symbol? string? exact-nonnegative-integer?)])
                       any/c])]{
The event, its name, the arguments in order, and one argument by name or
position; a key that names no parameter is a contract violation.}

@subsection{Filters and Fetching}

@defproc[(log-filter [#:address address (or/c #f
                                              (abi-value/c 'address)
                                              (non-empty-listof (abi-value/c 'address)))
                                #f]
                     [#:from-block from block-param/c "earliest"]
                     [#:to-block to block-param/c "latest"]
                     [#:block-hash block-hash (or/c #f (and/c string? #px"^0x[0-9a-fA-F]{64}$")) #f]
                     [#:topics topics (and/c (listof (or/c #f
                                       (and/c string? #px"^0x[0-9a-fA-F]{64}$")
                                       (non-empty-listof (and/c string? #px"^0x[0-9a-fA-F]{64}$"))))
                       (property/c length (<=/c 4))) '()]
                     [#:event event (or/c #f abi-event-spec/c) #f]
                     [#:where where hash? (hasheq)])
         log-filter?]{
Logs from @racket[address] in a block range or one block, with
@racket[topics] given raw or as @racket[event] with indexed arguments as
for @racket[event-topics].  @racket[block-hash] excludes the range,
@racket[topics] excludes @racket[event], and @racket[where] must be
empty without @racket[event].

@examples[#:eval event-eval
          (log-filter->jsexpr
           (log-filter #:address "0x5FbDB2315678afecb367f032d93F642f64180aa3" #:from-block 10
                       #:event "Transfer(address indexed from, address indexed to, uint256 value)"
                       #:where (hasheq 'from (any-of "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266"
                                                     "0x70997970C51812dc3A010C7d01b50e0d17dc79C8"))))]}

@defproc[(log-filter? [v any/c]) boolean?]{
Recognizes filters.}

@defproc[(log-filter->jsexpr [f log-filter?]) (and/c hash? jsexpr?)]{
The filter as @racket[eth-get-logs] takes it.}

@defproc[(get-logs [f log-filter?]
                   [#:events events (listof abi-event-spec/c) (list event)]
                   [#:chunk chunk (or/c #f exact-positive-integer?) #f]
                   [#:provider provider (or/c #f provider?) (current-provider)])
         (listof log?)]{
The logs matching @racket[f], in chain order, decoded with
@racket[decode-logs]; @racket[events] defaults to the filter's event.  A
range is fetched in requests of @racket[chunk] blocks; a request the
node refuses as too large is halved and retried.  Splitting a range reads
its tags as block numbers, @racket["pending"] as the latest block.

@examples[#:eval event-eval
          (define node (mock-provider (hash "eth_getLogs" (list transfer-json))))
          (get-logs (log-filter #:event "Transfer(address indexed from, address indexed to, uint256 value)")
                    #:provider node)
          (mock-provider-calls node)]}

@defproc[(receipt-logs [receipt (and/c hash? jsexpr?)]
                       [#:events events (listof abi-event-spec/c) '()])
         (listof log?)]{
The logs of @racket[receipt], decoded with @racket[decode-logs].}

@section{Local Nodes with anvil}

@defmodule[web3/anvil]

@subsection{Running anvil}

@defparam[current-anvil-path path (or/c #f path-string?)]{
The anvil executable used by @racket[start-anvil].  Defaults to the
@envvar{WEB3_ANVIL} environment variable when it is set and non-empty,
and otherwise to @tt{anvil} found on @envvar{PATH}, or @racket[#f].}

@defstruct*[anvil ([process subprocess?]
                   [port (integer-in 1 65535)]
                   [log-file path?]
                   [provider http-provider?])
            #:omit-constructor]{
A running anvil started by @racket[start-anvil], with a provider
connected to it.  Its output goes to @racket[log-file].}

@defproc[(start-anvil [#:path path (or/c #f path-string?) (current-anvil-path)]
                      [#:port port (integer-in 1 65535) _free-port]
                      [#:hardfork hardfork (or/c #f string?) #f]
                      [#:args args (listof string?) '()]
                      [#:timeout timeout real? 20.0])
         anvil?]{

Starts anvil listening on @tt{127.0.0.1}, by default on a port the
operating system reports as free, and returns once it answers
RPC requests.  @racket[hardfork] and @racket[args] are passed to anvil
on its command line.  Raises @racket[exn:fail:web3] if no anvil
executable is configured, if anvil exits during startup (the message
includes its output) or if it does not answer within @racket[timeout]
seconds.

The process is registered with the current custodian, so shutting the
custodian down stops it, and it is also stopped when Racket exits.}

@defproc[(stop-anvil [node anvil?]) void?]{
Stops @racket[node] if it is running and waits for it to exit.}

@defproc[(anvil-running? [node anvil?]) boolean?]{
Whether @racket[node]'s process is still running.}

@defform[(with-anvil (start-anvil-arg ...) body ...+)]{
Starts anvil by applying @racket[start-anvil] to the
@racket[start-anvil-arg]s, evaluates the @racket[body]s with the new
node's provider as @racket[current-provider], and stops the node when
the @racket[body]s return or raise.

@racketblock[
(with-anvil (#:hardfork "prague")
  (eth-accounts))
]}

@subsection{anvil Methods}

These wrap anvil's own methods and the hardhat-style @tt{evm_*}
methods.  They need a node that implements them, such as one started
with @racket[start-anvil].

@defproc[(anvil-set-code [address string?]
                         [code hex-string?]
                         [#:provider provider (or/c #f provider?) (current-provider)])
         void?]{
Replaces the code at @racket[address].}

@defproc[(anvil-set-balance [address string?]
                            [wei exact-nonnegative-integer?]
                            [#:provider provider (or/c #f provider?) (current-provider)])
         void?]{
Sets the balance of @racket[address].}

@defproc[(anvil-set-storage-at [address string?]
                               [slot exact-nonnegative-integer?]
                               [value hex-string?]
                               [#:provider provider (or/c #f provider?) (current-provider)])
         void?]{
Sets storage @racket[slot] of @racket[address] to the 32-byte
@racket[value].}

@defproc[(anvil-mine [#:provider provider (or/c #f provider?) (current-provider)])
         void?]{
Mines a block.}

@deftogether[(
@defproc[(anvil-impersonate-account
          [address string?]
          [#:provider provider (or/c #f provider?) (current-provider)])
         void?]
@defproc[(anvil-stop-impersonating-account
          [address string?]
          [#:provider provider (or/c #f provider?) (current-provider)])
         void?])]{
Lets @racket[eth-send-transaction] send from @racket[address] without
its key, or stops doing so.}

@deftogether[(
@defproc[(evm-snapshot [#:provider provider (or/c #f provider?) (current-provider)])
         hex-string?]
@defproc[(evm-revert [snapshot hex-string?]
                     [#:provider provider (or/c #f provider?) (current-provider)])
         boolean?])]{
Saves the chain state and returns its id, or restores the state saved
as @racket[snapshot].}

@subsection{Tracing}

@deftogether[(
@defproc[(debug-trace-call [tx (and/c hash? jsexpr?)]
                           [block block-param/c]
                           [options (and/c hash? jsexpr?)]
                           [#:provider provider (or/c #f provider?) (current-provider)])
         hash?]
@defproc[(debug-trace-transaction [tx-hash hex-string?]
                                  [options (and/c hash? jsexpr?)]
                                  [#:provider provider (or/c #f provider?) (current-provider)])
         hash?])]{

Struct-log traces of a call or a mined transaction, with one entry per
executed opcode in @racket['structLogs].  @racket[options] follows
geth's tracer configuration, such as
@racket[(hasheq 'enableMemory #t)].}
