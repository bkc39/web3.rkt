#lang scribble/manual
@require[@for-label[json
                    racket/base
                    racket/contract
                    racket/generic
                    web3
                    web3/abi
                    web3/abi/type
                    web3/abi/vectors
                    web3/address
                    web3/anvil
                    web3/eth
                    web3/rpc]
          scribble/example]

@(define abi-eval (make-base-eval '(require web3/abi web3/address web3/rpc)))

@title{web3: Ethereum JSON-RPC for Racket}
@author{bkc}

@defmodule[web3]

An Ethereum JSON-RPC client: typed wrappers for the standard
@tt{web3_*}, @tt{net_*} and @tt{eth_*} methods, the contract ABI, and
support for running local
@hyperlink["https://book.getfoundry.sh/anvil/"]{anvil} nodes.

@racket[(require web3)] provides everything in @racketmodname[web3/rpc],
@racketmodname[web3/eth], @racketmodname[web3/abi],
@racketmodname[web3/address] and @racketmodname[web3/anvil].

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
 @item{transactions, receipts, blocks and logs are @racket[jsexpr?]
       hash tables with symbol keys, as read by @racketmodname[json];}
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
Whether the receipt's status is success, @racket["0x1"].}

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

@section{Contract ABI}

@defmodule[web3/abi]

Encodes and decodes values for the
@hyperlink["https://docs.soliditylang.org/en/latest/abi-spec.html"]{contract ABI}
and decodes reverts.  Also provides everything in
@racketmodname[web3/abi/type].

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
             [reason (or/c 'short 'offset 'overlap 'length 'non-canonical 'utf-8 'selector)])]{
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
