#lang scribble/manual
@require[@for-label[json
                    racket/base
                    racket/contract
                    web3
                    web3/abi
                    web3/anvil
                    web3/eth
                    web3/rpc]]

@title{web3: Ethereum JSON-RPC for Racket}
@author{bkc}

@defmodule[web3]

An Ethereum JSON-RPC client: typed wrappers for the standard
@tt{web3_*}, @tt{net_*} and @tt{eth_*} methods, a first slice of the
contract ABI, and support for running local
@hyperlink["https://book.getfoundry.sh/anvil/"]{anvil} nodes.

@racket[(require web3)] provides everything in @racketmodname[web3/rpc],
@racketmodname[web3/eth], @racketmodname[web3/abi] and
@racketmodname[web3/anvil].

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
@racket[decode-revert-reason] can read.

@section{Providers and Calls}

@defmodule[web3/rpc]

@defproc[(url->http-provider [url string?]) http-provider?]{

Returns a provider that sends JSON-RPC requests to @racket[url] over
HTTP or HTTPS.  The path and query of @racket[url] are kept, so URLs
that carry an API key in their path work as given.

The provider holds one connection, reopened on demand, and serializes
requests on it, so it can be shared between threads.}

@defproc[(http-provider? [v any/c]) boolean?]{
Recognizes providers made by @racket[url->http-provider].}

@defproc[(http-provider-url [provider http-provider?]) string?]{
The URL that @racket[provider] was made from.}

@defparam[current-provider provider (or/c #f http-provider?)
          #:value #f]{
The provider used by endpoints and @racket[rpc-call] when no
@racket[#:provider] argument is given.}

@defform[(with-provider provider-expr body ...+)]{
Evaluates the @racket[body]s with @racket[current-provider] set to the
value of @racket[provider-expr].}

@defproc[(rpc-call [method string?]
                   [params (listof jsexpr?)]
                   [#:provider provider (or/c #f http-provider?)
                    (current-provider)])
         jsexpr?]{

Sends a JSON-RPC request for @racket[method] with positional
@racket[params] and returns the response's @tt{result}.  Raises
@racket[exn:fail:web3:rpc] when the node returns an error response, and
@racket[exn:fail:web3] when there is no provider or the response is not
a JSON object.

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
@racket[result-contract-expr].

@racketblock[
(define-jsonrpc-endpoint eth_getBalance
  (-> string? [block-param/c block-param->string] exact-nonnegative-integer?)
  #:result-transform hex-string->number)
]}

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
                 (lambda (e)
                   (decode-revert-reason
                    (hex-string->bytes (exn:fail:web3:rpc-data e))))])
  (eth-call (hasheq 'to contract 'data "0x") "latest"))
]}

@subsection{Hex Strings}

@defproc[(hex-string? [v any/c]) boolean?]{
Recognizes strings of the form @racket["0x"] followed by zero or more
hexadecimal digits.}

@defproc[(hex-string->number [s string?]) exact-nonnegative-integer?]{
Reads a JSON-RPC quantity.  @racket["0x"] alone reads as @racket[0].
A string without the @racket["0x"] prefix is read as decimal.}

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
@defproc[(web3-client-version [#:provider provider (or/c #f http-provider?) (current-provider)])
         string?]
@defproc[(net-version [#:provider provider (or/c #f http-provider?) (current-provider)])
         string?]
@defproc[(net-listening [#:provider provider (or/c #f http-provider?) (current-provider)])
         boolean?]
@defproc[(net-peer-count [#:provider provider (or/c #f http-provider?) (current-provider)])
         exact-nonnegative-integer?]
@defproc[(eth-chain-id [#:provider provider (or/c #f http-provider?) (current-provider)])
         exact-nonnegative-integer?]
@defproc[(eth-syncing [#:provider provider (or/c #f http-provider?) (current-provider)])
         (or/c boolean? hash?)]
@defproc[(eth-gas-price [#:provider provider (or/c #f http-provider?) (current-provider)])
         exact-nonnegative-integer?]
@defproc[(eth-accounts [#:provider provider (or/c #f http-provider?) (current-provider)])
         (listof string?)]
@defproc[(eth-block-number [#:provider provider (or/c #f http-provider?) (current-provider)])
         exact-nonnegative-integer?])]{

@tt{web3_clientVersion}, @tt{net_version}, @tt{net_listening},
@tt{net_peerCount}, @tt{eth_chainId}, @tt{eth_syncing},
@tt{eth_gasPrice} (in wei), @tt{eth_accounts} and
@tt{eth_blockNumber}.  @racket[net-version] returns the network id as
the node reports it, a decimal string.}

@deftogether[(
@defproc[(eth-protocol-version [#:provider provider (or/c #f http-provider?) (current-provider)])
         string?]
@defproc[(eth-coinbase [#:provider provider (or/c #f http-provider?) (current-provider)])
         string?]
@defproc[(eth-mining [#:provider provider (or/c #f http-provider?) (current-provider)])
         boolean?]
@defproc[(eth-hashrate [#:provider provider (or/c #f http-provider?) (current-provider)])
         exact-nonnegative-integer?])]{

Legacy proof-of-work era methods.  Many clients no longer serve them
since the Merge and answer with an error.}

@defproc[(web3-sha3 [data hex-string?]
                    [#:provider provider (or/c #f http-provider?) (current-provider)])
         hex-string?]{
Keccak-256 of @racket[data], computed by the node.  To hash locally, use
@racketidfont{keccak256} from the @tt{keccak} package.}

@subsection{State}

@defproc[(eth-get-balance [address string?]
                          [block block-param/c]
                          [#:provider provider (or/c #f http-provider?) (current-provider)])
         exact-nonnegative-integer?]{
The balance of @racket[address] in wei.}

@defproc[(eth-get-transaction-count [address string?]
                                    [block block-param/c]
                                    [#:provider provider (or/c #f http-provider?) (current-provider)])
         exact-nonnegative-integer?]{
The number of transactions sent from @racket[address]: its next nonce.}

@defproc[(eth-get-storage-at [address string?]
                             [slot exact-nonnegative-integer?]
                             [block block-param/c]
                             [#:provider provider (or/c #f http-provider?) (current-provider)])
         hex-string?]{
The 32-byte word at storage @racket[slot] of @racket[address].}

@defproc[(eth-get-code [address string?]
                       [block block-param/c]
                       [#:provider provider (or/c #f http-provider?) (current-provider)])
         hex-string?]{
The code deployed at @racket[address], or @racket["0x"] for an account
without code.}

@subsection{Blocks}

@defproc[(eth-get-block-by-number [block block-param/c]
                                  [full-transactions? boolean?]
                                  [#:provider provider (or/c #f http-provider?) (current-provider)])
         (or/c 'null hash?)]{
The block, with full transaction objects when
@racket[full-transactions?] is true and transaction hashes otherwise.}

@deftogether[(
@defproc[(eth-get-block-transaction-count-by-hash
          [block-hash string?]
          [#:provider provider (or/c #f http-provider?) (current-provider)])
         exact-nonnegative-integer?]
@defproc[(eth-get-block-transaction-count-by-number
          [block block-param/c]
          [#:provider provider (or/c #f http-provider?) (current-provider)])
         exact-nonnegative-integer?])]{
The number of transactions in a block; @racket[0] for an unknown block.}

@subsection{Calls and Transactions}

@defproc[(eth-call [tx (and/c hash? jsexpr?)]
                   [block block-param/c]
                   [#:provider provider (or/c #f http-provider?) (current-provider)])
         hex-string?]{
Executes the call @racket[tx] against the state at @racket[block]
without creating a transaction, and returns its return data.  A revert
raises @racket[exn:fail:web3:rpc] with the revert payload in its
@racket[data] field.}

@defproc[(eth-estimate-gas [tx (and/c hash? jsexpr?)]
                           [#:provider provider (or/c #f http-provider?) (current-provider)])
         exact-nonnegative-integer?]{
An estimate of the gas @racket[tx] would use.}

@defproc[(eth-send-transaction [tx (and/c hash? jsexpr?)]
                               [#:provider provider (or/c #f http-provider?) (current-provider)])
         hex-string?]{
Asks the node to sign and send @racket[tx] from an account it manages,
such as one of anvil's development accounts, and returns the
transaction hash.}

@defproc[(eth-send-raw-transaction [signed-tx hex-string?]
                                   [#:provider provider (or/c #f http-provider?) (current-provider)])
         hex-string?]{
Submits an already signed, serialized transaction and returns its hash.}

@deftogether[(
@defproc[(eth-get-transaction-by-hash
          [tx-hash hex-string?]
          [#:provider provider (or/c #f http-provider?) (current-provider)])
         (or/c 'null hash?)]
@defproc[(eth-get-transaction-receipt
          [tx-hash hex-string?]
          [#:provider provider (or/c #f http-provider?) (current-provider)])
         (or/c 'null hash?)])]{
A transaction, or its receipt once it has been mined.}

@defproc[(eth-get-logs [filter (and/c hash? jsexpr?)]
                       [#:provider provider (or/c #f http-provider?) (current-provider)])
         list?]{
The logs matching @racket[filter], a hash table with keys such as
@racket['address], @racket['topics], @racket['fromBlock] and
@racket['toBlock].}

@defproc[(wait-for-receipt [tx-hash hex-string?]
                           [#:timeout timeout real? 30.0]
                           [#:interval interval real? 0.002]
                           [#:provider provider (or/c #f http-provider?) (current-provider)])
         hash?]{
Polls for the receipt of @racket[tx-hash], starting every
@racket[interval] seconds and backing off to every quarter second.
Raises @racket[exn:fail:web3] if no receipt appears within
@racket[timeout] seconds.}

@defproc[(send-transaction/receipt [tx (and/c hash? jsexpr?)]
                                   [#:provider provider (or/c #f http-provider?) (current-provider)])
         hash?]{
Sends @racket[tx] with @racket[eth-send-transaction] and returns its
receipt once mined.}

@defproc[(receipt-succeeded? [receipt hash?]) boolean?]{
Whether the receipt's status is success, @racket["0x1"].}

@section{Contract ABI}

@defmodule[web3/abi]

The parts of the
@hyperlink["https://docs.soliditylang.org/en/latest/abi-spec.html"]{contract
ABI} needed to call functions whose parameters are all static 32-byte
types (@tt{uint256}, @tt{address}, @tt{bool}, ...) and to read revert
reasons.

@defproc[(function-selector [signature (or/c string? bytes?)]) bytes?]{
The first four bytes of the Keccak-256 hash of a canonical signature.

@racketblock[
(bytes->hex-string (function-selector "transfer(address,uint256)"))
(code:comment "=> \"0xa9059cbb\"")
]}

@defproc[(word->bytes [n exact-nonnegative-integer?]) bytes?]{
@racket[n] as a 32-byte big-endian word.  Raises an error if @racket[n]
does not fit in 256 bits.}

@defproc[(bytes->words [bs bytes?]) (listof exact-nonnegative-integer?)]{
Reads @racket[bs] as consecutive 32-byte big-endian words.  A trailing
chunk shorter than 32 bytes is read as a number on its own.}

@defproc[(encode-call [signature (or/c string? bytes?)]
                      [word exact-nonnegative-integer?] ...)
         bytes?]{
Call data for @racket[signature]: its selector followed by each
@racket[word] as a 32-byte word.  Addresses are passed as their numeric
value.

@racketblock[
(bytes->hex-string (encode-call "balanceOf(address)" #xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045))
]}

@defproc[(decode-revert-reason [data bytes?]) (or/c string? #f)]{
A readable form of a revert payload encoded as @tt{Error(string)} or
@tt{Panic(uint256)}, such as @racket["Error(\"oops!\")"] or
@racket["Panic(0x11)"], or @racket[#f] for any other payload.}

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
                         [#:provider provider (or/c #f http-provider?) (current-provider)])
         any/c]{
Replaces the code at @racket[address].}

@defproc[(anvil-set-balance [address string?]
                            [wei exact-nonnegative-integer?]
                            [#:provider provider (or/c #f http-provider?) (current-provider)])
         any/c]{
Sets the balance of @racket[address].}

@defproc[(anvil-set-storage-at [address string?]
                               [slot exact-nonnegative-integer?]
                               [value hex-string?]
                               [#:provider provider (or/c #f http-provider?) (current-provider)])
         any/c]{
Sets storage @racket[slot] of @racket[address] to the 32-byte
@racket[value].}

@defproc[(anvil-mine [#:provider provider (or/c #f http-provider?) (current-provider)])
         any/c]{
Mines a block.}

@deftogether[(
@defproc[(anvil-impersonate-account
          [address string?]
          [#:provider provider (or/c #f http-provider?) (current-provider)])
         any/c]
@defproc[(anvil-stop-impersonating-account
          [address string?]
          [#:provider provider (or/c #f http-provider?) (current-provider)])
         any/c])]{
Lets @racket[eth-send-transaction] send from @racket[address] without
its key, or stops doing so.}

@deftogether[(
@defproc[(evm-snapshot [#:provider provider (or/c #f http-provider?) (current-provider)])
         hex-string?]
@defproc[(evm-revert [snapshot hex-string?]
                     [#:provider provider (or/c #f http-provider?) (current-provider)])
         boolean?])]{
Saves the chain state and returns its id, or restores the state saved
as @racket[snapshot].}

@subsection{Tracing}

@deftogether[(
@defproc[(debug-trace-call [tx (and/c hash? jsexpr?)]
                           [block block-param/c]
                           [options (and/c hash? jsexpr?)]
                           [#:provider provider (or/c #f http-provider?) (current-provider)])
         hash?]
@defproc[(debug-trace-transaction [tx-hash hex-string?]
                                  [options (and/c hash? jsexpr?)]
                                  [#:provider provider (or/c #f http-provider?) (current-provider)])
         hash?])]{

Struct-log traces of a call or a mined transaction, with one entry per
executed opcode in @racket['structLogs].  @racket[options] follows
geth's tracer configuration, such as
@racket[(hasheq 'enableMemory #t)].}
