#lang scribble/manual
@require[@for-label[json
                    libsecp256k1
                    openssl
                    racket/base
                    racket/contract
                    racket/generic
                    racket/match
                    racket/runtime-path
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
                    web3/eth/block
                    web3/eth/log
                    web3/eth/receipt
                    web3/eth/transaction-info
                    web3/event
                    web3/rlp
                    web3/rpc
                    web3/send
                    web3/signer
                    web3/transaction
                    web3/units]
          (for-syntax racket/base)
          scribble/example
          syntax/parse/define]

@(define abi-eval (make-base-eval '(require web3/abi web3/address web3/rpc)))
@(define contract-eval (make-base-eval '(require web3)))
@(define event-eval (make-base-eval '(require web3)))
@(define account-eval (make-base-eval '(require web3 (only-in keccak keccak256))))
@(define rlp-eval (make-base-eval '(require web3 (only-in keccak keccak256))))
@(define units-eval (make-base-eval '(require web3)))
@(define transaction-eval (make-base-eval '(require web3)))
@(define signer-eval (make-base-eval '(require web3)))
@(define send-eval (make-base-eval '(require web3)))
@(define eth-eval (make-base-eval '(require web3)))
@(define provider-eval (make-base-eval '(require openssl web3)))
@(define provider-guide-eval (make-base-eval '(require racket/generic racket/match web3)))
@(define abi-guide-eval (make-base-eval '(require web3)))
@(define event-guide-eval (make-base-eval '(require web3)))
@(define results-eval
   (make-base-eval
    '(require web3)
    '(define log-json
       (hasheq 'address "0x5fbdb2315678afecb367f032d93f642f64180aa3"
               'topics
               '("0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"
                 "0x000000000000000000000000f39fd6e51aad88f6f4ce6ab8827279cfffb92266"
                 "0x00000000000000000000000070997970c51812dc3a010c7d01b50e0d17dc79c8")
               'data "0x00000000000000000000000000000000000000000000000000000000000000fa"
               'blockNumber "0x2"
               'blockHash "0x78e3cb19177d24232982259d6717f8ea45cfae5f369be21280af7b8e535146b5"
               'transactionHash "0xf6530f5992a7b2b0ff8b8014dc54ad20e0f16ae89343cfb98640c6a7d94cf5d4"
               'transactionIndex "0x0"
               'logIndex "0x0"
               'removed #f))
    '(define receipt-json
       (hasheq 'transactionHash "0xf6530f5992a7b2b0ff8b8014dc54ad20e0f16ae89343cfb98640c6a7d94cf5d4"
               'transactionIndex "0x0"
               'blockHash "0x78e3cb19177d24232982259d6717f8ea45cfae5f369be21280af7b8e535146b5"
               'blockNumber "0x2"
               'from "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266"
               'to "0x5fbdb2315678afecb367f032d93f642f64180aa3"
               'type "0x2"
               'status "0x1"
               'cumulativeGasUsed "0xcbf6"
               'gasUsed "0xcbf6"
               'effectiveGasPrice "0x705f9991"
               'contractAddress 'null
               'logs (list log-json)
               'logsBloom (string-append "0x" (make-string 512 #\0))))
    '(define transaction-info-json
       (hasheq 'hash "0x4a02a85a61dbc92fec29ffa220781b27b2c6bc91527b22a04af286791e41c28a"
               'blockHash "0x56268c1258891dd669aacbe1c854f83204063dd979b3207267179897f8e45d7c"
               'blockNumber "0x1"
               'transactionIndex "0x0"
               'from "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266"
               'to "0x70997970c51812dc3a010c7d01b50e0d17dc79c8"
               'type "0x0"
               'chainId "0x7a69"
               'nonce "0x0"
               'value "0x1"
               'input "0x"
               'gas "0x5208"
               'gasPrice "0x6fc23ac00"
               'v "0xf4f6"
               'r "0xbbfa8af4971395f4254befd93c53c020fb096fc451369a7dc6496814972c5948"
               's "0x17e6be4dd1cdb05aa9f72da7d69aa2af9d5c619ad2d29b538f7cf16e40a21411"))
    '(define withdrawal-json
       (hasheq 'index "0x7"
               'validatorIndex "0x2a"
               'address "0x70997970c51812dc3a010c7d01b50e0d17dc79c8"
               'amount "0x3b9aca00"))
    '(define block-json
       (hasheq 'hash "0x56268c1258891dd669aacbe1c854f83204063dd979b3207267179897f8e45d7c"
               'parentHash "0xcf3edcafefb348d27cb299d18f73cbcc4e93a912168b6291369a3fc8e0580e5b"
               'stateRoot "0xb1b8d88a4b0a2acaaa60e14245dc7d794bcce5a13d918af30890c565db667608"
               'transactionsRoot
               "0x681c8e89df028e0c0d0fb8c3c3dc8a4185b9ea50aab92f381704fd7cfa5a1f52"
               'receiptsRoot "0xdfeff9dad83b6f7eb62afe83ba8adff1d0951e31f085553500598d2a77725114"
               'logsBloom (string-append "0x" (make-string 512 #\0))
               'number "0x1"
               'gasLimit "0x1c9c380"
               'gasUsed "0x5208"
               'timestamp "0x6b02118f"
               'baseFeePerGas "0x3b9aca00"
               'withdrawals (list withdrawal-json)
               'transactions
               '("0x4a02a85a61dbc92fec29ffa220781b27b2c6bc91527b22a04af286791e41c28a")))))

@(define hash-string-contract @racket[(and/c string? #px"^0x[0-9a-f]{64}$")])
@(define hash-argument-contract @racket[(and/c string? #px"^0x[0-9a-fA-F]{64}$")])

@(define-syntax-parse-rule (defproc/filling (name:id positional ...) result:expr body ...)
   (defproc (name positional ...
             [#:value value uint256/c 0]
             [#:data data bytes? #""]
             [#:nonce nonce (or/c #f uint64/c) #f]
             [#:gas-limit gas-limit (or/c #f uint64/c) #f]
             [#:chain-id chain-id (or/c #f uint64/c) #f]
             [#:type type (or/c #f 0 1 2) #f]
             [#:gas-price gas-price (or/c #f uint256/c) #f]
             [#:max-priority-fee-per-gas max-priority-fee-per-gas (or/c #f uint256/c) #f]
             [#:max-fee-per-gas max-fee-per-gas (or/c #f uint256/c) #f]
             [#:access-list access-list (or/c #f access-list/c) #f]
             [#:errors errors (listof (or/c string? abi-signature?)) '()]
             [#:provider provider (or/c #f provider?) (current-provider)])
     result
     body ...))

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
@racketmodname[web3/units], @racketmodname[web3/transaction],
@racketmodname[web3/signer], @racketmodname[web3/send], @racketmodname[web3/contract],
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
 @item{the addresses of @racket[eth-accounts] and @racket[eth-coinbase] are checksummed;}
 @item{receipts, blocks, transactions on chain, fee histories and logs are values with
       accessors, @racket[receipt?], @racket[block?], @racket[transaction-info?],
       @racket[fee-history?] and @racket[log?]: their addresses are checksummed, their hashes
       lowercase @racket["0x..."] strings and their data bytes, and the node's object is kept,
       not copied, in @racket[receipt->jsexpr], @racket[block->jsexpr] and the like, so it must
       not be mutated;}
 @item{logs from @racket[get-logs] and @racket[receipt-logs] are @racket[log?] values;
       @racket[eth-get-logs] returns the node's JSON;}
 @item{other results, such as proofs, access lists and filter changes, are @racket[jsexpr?]
       hash tables with symbol keys, as read by @racketmodname[json], whose data, hashes and
       addresses are @racket["0x..."] strings;}
 @item{a missing receipt, block or transaction (one that does not exist yet) is @racket[#f], and
       a missing raw transaction is @racket['null].}]

An address argument is an @racket[address?]: @racket["0x"] and 40 hex digits, all lowercase,
all uppercase or in their EIP-55 checksum, so a mixed-case address with a wrong checksum is a
contract violation.  A hash argument is @racket["0x"] and 64 hex digits in either case.
A refused address or hash raises @racket[exn:fail:contract:blame] whose message names the
procedure, the argument (and the field of an object), the value and why it is refused.  The
value is shown only when it is a string of @racket["0x"] and at most 40 hex digits; any other
value, which could be a secret key or the text one was read from, is described by its kind, such
as @tt{a secret key} or @tt{a string of 65 characters}:

@examples[#:eval contract-eval #:label #f
(eval:error (eth-get-balance "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD" "latest"))]

Addresses a node returns are respelled in their EIP-55 checksum, not checked against it.
Transaction, call and filter objects are passed to @tt{eth_*} endpoints as the node expects them,
as hash tables such as @racket[(hasheq 'from sender 'to contract 'data "0x...")]; their
@racket['from], @racket['to] and @racket['address] fields and the @racket['address] of each
@racket['accessList] entry are @racket[address?] values (a filter's @racket['address] may be a list
of them, and @racket['to] may be @racket['null]).
@racket[send-transaction] and the other procedures of @secref["sending"] take a transaction's
fields as keywords, quantities as exact integers and data as bytes.

A JSON-RPC error response raises @racket[exn:fail:web3:rpc]; contract
reverts carry their payload in @racket[exn:fail:web3:rpc-data], which
@racket[rpc-exn->revert] decodes.  A result that does not decode
as documented raises @racket[exn:fail:web3].

@section[#:tag "guide"]{Guide}

@subsection[#:tag "guide-providers"]{Providers and Calls}

A provider is a connection to a node: @racket[url->http-provider] makes one from the node's URL,
and @racket[with-anvil] starts a local node and makes its provider the @racket[current-provider]
while its body runs.  Every endpoint takes a @racket[#:provider] argument and otherwise uses the
current one.  An @tt{https} provider checks the node's certificate and host name unless given
another context.

@racket[define-jsonrpc-endpoint] defines an endpoint for a method this library lacks, with
contracts on its arguments and a conversion of its result.  An error response raises
@racket[exn:fail:web3:rpc], which holds the node's code, message and data.

@racketblock[
(define-jsonrpc-endpoint eth_getUncleCountByBlockNumber
  (-> [exact-nonnegative-integer? number->hex-string] exact-nonnegative-integer?)
  #:result-transform hex-string->number)

(define (print-rpc-error e)
  (printf "refused ~a: ~a\n" (exn:fail:web3:rpc-code e) (exn:fail:web3:rpc-rpc-message e)))]

Endpoints return decoded values, and @racket[rpc-call] sends any method and returns its result as
the node wrote it, here @racket["0x7a69"].  A method the node lacks is refused with code
@racket[-32601].

@racketblock[
(with-anvil ()
  (printf "chain id ~a\n" (eth-chain-id))
  (printf "block ~a\n" (eth-block-number))
  (printf "eth_chainId ~s\n" (rpc-call "eth_chainId" '()))
  (printf "uncles in block 0: ~a\n" (eth-get-uncle-count-by-block-number 0))
  (with-handlers ([exn:fail:web3:rpc? print-rpc-error])
    (rpc-call "eth_noSuchMethod" '())))]

See @filepath{examples/00-provider.rkt}.

@subsection[#:tag "guide-chain-state"]{Chain State}

Quantities are exact integers: balances in wei, nonces, gas and block numbers.  Receipts, blocks
and transactions are values with accessors such as @racket[receipt-block-number], as in
@secref["Conventions"]; @racket[eth-call] and the other endpoints take the node's JSON.

A node such as anvil holds keys for its accounts and signs for them.
@racket[send-transaction/receipt] sends a transaction from one of them, its fields given as
keywords, and returns its receipt once it is mined; @racket[send-transaction] returns its hash at
once.  The node gives each transaction a sender sends its next nonce.

@racketblock[
(define (print-accounts alice bob)
  (printf "alice nonce ~a, bob ~a ether\n"
          (eth-get-transaction-count alice "latest")
          (wei->string (eth-get-balance bob "latest"))))]

Here alice sends bob one ether: bob's balance goes from 10000 to 10001 ether, and alice's nonce
from 0 to 1.

@racketblock[
(with-anvil ()
  (match-define (list alice bob _ ...) (eth-accounts))
  (print-accounts alice bob)
  (define receipt (send-transaction/receipt alice bob #:value (ether->wei 1)))
  (printf "block ~a, gas used ~a, succeeded ~a\n"
          (receipt-block-number receipt)
          (receipt-gas-used receipt)
          (receipt-succeeded? receipt))
  (print-accounts alice bob))]

Blocks and transactions are read as in @secref["guide-blocks"], and code and storage the same
way; @secref["Ethereum_Endpoints"] lists the endpoints.  A transaction signed here, not by the node, is sent as in
@secref["guide-sending"].

See @filepath{examples/01-chain-state.rkt}.

@subsection[#:tag "guide-anvil"]{Local Nodes}

@racket[with-anvil] runs its body against a fresh anvil and stops it afterwards; its accounts
start with 10000 ether each.  anvil's own methods set up state directly: the code and balance of
any address, its storage, sends from an address whose key nobody holds, and snapshots of the
whole chain to go back to.

Here the code @racket[store-calldata-code] stores its call data in slot 0.

@racketblock[
(define contract "0x00000000000000000000000000000000000c0de0")
(define stranger "0x000000000000000000000000000000000000beef")
(define store-calldata-code "0x5f355f5500")

(define (stored-value)
  (hex-string->number (eth-get-storage-at contract 0 "latest")))]

The stranger, funded and impersonated, sends 7 to the contract, which stores it; reverting to the
snapshot taken before undoes the send, and slot 0 holds 0 again.

@racketblock[
(with-anvil ()
  (anvil-set-code contract store-calldata-code)
  (anvil-set-balance stranger (ether->wei 1))
  (define snapshot (evm-snapshot))
  (anvil-impersonate-account stranger)
  (define receipt (send-transaction/receipt stranger contract #:data (word->bytes 7)))
  (anvil-stop-impersonating-account stranger)
  (printf "sent from ~a, stored ~a\n" (receipt-from receipt) (stored-value))
  (evm-revert snapshot)
  (printf "reverted, stored ~a\n" (stored-value)))]

@secref["Local_Nodes_with_anvil"] has the rest of anvil's methods.

See @filepath{examples/02-anvil.rkt}.

@subsection[#:tag "guide-custom-providers"]{Writing a Provider}

Anything implementing @racket[gen:provider] is a provider, so one can wrap another to log, cache
or retry its requests.  @racket[mock-provider] answers from a table instead of a node, which tests
code that calls a node without one.  An entry is a result, or a procedure of the request's params;
@racket[raise-rpc-error] answers with an error, as a node does.

@examples[#:eval provider-guide-eval
          (struct logging (inner)
            #:methods gen:provider
            [(define/generic inner-request provider-request)
             (define/generic inner-close provider-close)
             (define (provider-request provider method params)
               (printf "request ~a\n" method)
               (inner-request (logging-inner provider) method params))
             (define (provider-close provider)
               (inner-close (logging-inner provider)))])]

The mock holds one balance and refuses every call.  A method its table lacks raises
@racket[exn:fail:web3:rpc] with code @racket[-32601].

@examples[#:eval provider-guide-eval
          (define alice "0x00000000000000000000000000000000000a11ce")
          (define token "0x00000000000000000000000000000000000c0de0")
          (define balances (hash alice "0xde0b6b3a7640000"))
          (define node
            (mock-provider
             (hash "eth_chainId" "0x7a69"
                   "eth_getBalance" (match-lambda
                                      [(list address _block) (hash-ref balances address "0x0")])
                   "eth_call" (match-lambda
                                [(list _call _block)
                                 (raise-rpc-error "eth_call" 3 "execution reverted" "0x")]))))]

Every request through the wrapper is logged, then answered by the mock, which records it.

@examples[#:eval provider-guide-eval
          (with-provider (logging node)
            (printf "chain id ~a\n" (eth-chain-id))
            (printf "balance ~a ether\n" (wei->string (eth-get-balance alice "latest")))
            (with-handlers ([exn:fail:web3:rpc?
                             (lambda (e) (printf "refused: ~a\n" (exn-message e)))])
              (eth-call (hasheq 'to token 'data "0x") "latest")))
          (printf "the mock answered ~a requests\n" (length (mock-provider-calls node)))]

See @filepath{examples/03-custom-providers.rkt}.

@subsection[#:tag "guide-abi"]{Encoding and Decoding}

A contract is called with call data: a four-byte selector, from the Keccak-256 hash of the
function's canonical signature, then the arguments in the contract ABI's encoding.  Types are
written as Solidity writes them, as s-expressions, or as @secref["ABI_Types"] structs.
@racket[encode-call] encodes a call.  A string or an array is written after the fixed-size words,
which hold its offset: for @tt{submit}, the string's offset, the array's, the string's length and
bytes, then the array's length and elements.

@examples[#:eval abi-guide-eval
          (define calls
            `(("transfer(address, uint)" "0x70997970C51812dc3A010C7d01b50e0d17dc79C8"
               ,(ether->wei 1))
              ("submit(string,uint256[])" "hello" (1 2))))
          (define (print-call signature . arguments)
            (define data (apply encode-call signature arguments))
            (printf "~a\n" (abi-signature->string (string->abi-signature signature)))
            (printf "  selector ~a\n" (bytes->hex-string (function-selector signature)))
            (for ([word (in-list (bytes->words (subbytes data 4)))])
              (printf "  ~a\n" (bytes->hex-string (word->bytes word)))))
          (for ([call (in-list calls)])
            (apply print-call call))]

See @filepath{examples/04-abi-encode.rkt}.

Decoding goes the other way.  @racket[decode-result] reads what @racket[eth-call] returns, given
the function's outputs, and @racket[decode-call] reads call data back into its arguments.  A
revert's data is decoded by @racket[decode-revert]: @tt{Error(string)}, @tt{Panic(uint256)}, and
the contract's own errors given in @racket[#:errors].  A call that reverts raises
@racket[exn:fail:web3:rpc] with that data, which @racket[rpc-exn->revert] decodes the same way.

@examples[#:eval abi-guide-eval
          (define token-info "info()(string,uint256)")
          (define transfer "transfer(address,uint256)")
          (define errors '("InsufficientBalance(uint256,uint256)"))
          (define result (abi-encode '(string uint256) '("Token" 1000)))
          (define call-data
            (encode-call transfer "0x70997970C51812dc3A010C7d01b50e0d17dc79C8" 250))
          (define reverts
            (list (encode-call "Error(string)" "not the owner")
                  (encode-call "Panic(uint256)" #x11)
                  (encode-call "InsufficientBalance(uint256,uint256)" 250 1000)))
          (printf "result ~s\n" (decode-result token-info result))
          (printf "call ~s\n" (decode-call transfer call-data))
          (for ([data (in-list reverts)])
            (printf "revert ~a\n" (abi-revert->string (decode-revert data #:errors errors))))]

What a node returns is untrusted: anything that is not an encoding raises
@racket[exn:fail:web3:abi:decode].

@examples[#:eval abi-guide-eval
          (with-handlers ([exn:fail:web3:abi:decode?
                           (lambda (e) (printf "refused: ~a\n" (exn-message e)))])
            (decode-call transfer (subbytes call-data 0 36)))]

See @filepath{examples/05-abi-decode.rkt}.

@subsection[#:tag "guide-contracts"]{Contracts}

@racket[deploy-contract] sends a contract's init code with its constructor's arguments and
returns the new contract's address.  @racket[contract-call] calls a function without sending a
transaction and returns its outputs, one value each; @racket[contract-send] sends a transaction
from @racket[current-account], or @racket[#:from], and returns its receipt once mined.  A function
is named by its signature, with its outputs for a call.  @racket[file->contract-abi] reads a
compiled artifact, which holds the init code.

A send is estimated first, so one that would revert raises @racket[exn:fail:web3:rpc:revert] and
is not sent; one mined and failed raises @racket[exn:fail:web3:transaction].  Either way
@racket[exn->abi-revert] gives the revert, decoded with the errors given as @racket[#:errors].

@racketblock[
(define-runtime-path token-artifact "contracts/out/Token.sol/Token.json")

(define init-code (contract-abi-bytecode (file->contract-abi token-artifact)))
(define transfer "transfer(address,uint256)")
(define errors '("InsufficientBalance(uint256,uint256)"))

(define (print-refusal e)
  (printf "refused: ~a\n" (abi-revert->string (exn->abi-revert e))))]

Alice deploys the token, which gives her all 1000 tokens, and sends bob 250.  Bob then tries to
send alice 1000, more than he holds: the estimate reverts with @tt{InsufficientBalance(250, 1000)}
and nothing is sent.

@racketblock[
(with-anvil ()
  (match-define (list alice bob _ ...) (eth-accounts))
  (parameterize ([current-account alice])
    (define token
      (deploy-contract init-code "constructor(string,string,uint256)" "Token" "TOK" 1000))
    (printf "~a at ~a\n" (contract-call token "name()(string)") token)
    (define receipt (contract-send token transfer bob 250))
    (printf "transfer succeeded ~a\n" (receipt-succeeded? receipt))
    (printf "balances ~s\n"
            (contract-call token "balancesOf(address[])(uint256[])" (list alice bob)))
    (with-handlers ([exn:fail:web3? print-refusal])
      (contract-send token transfer alice 1000 #:from bob #:errors errors))))]

A contract's ABI read once, with a procedure per function, is @secref["guide-bindings"].

See @filepath{examples/06-deploy-and-call.rkt}.

@subsection[#:tag "guide-events"]{Events}

A contract logs what it does as events.  An event is written as Solidity declares it; its topic
0, the hash of its signature, picks its logs out.

@racket[decode-log] reads a log as an event, whose arguments @racket[event-log-ref] gives by
name.  The token that logs here is deployed as in @secref["guide-contracts"].

@racketblock[
(define-runtime-path token-artifact "contracts/out/Token.sol/Token.json")

(define init-code (contract-abi-bytecode (file->contract-abi token-artifact)))
(define transfer "transfer(address,uint256)")]

@examples[#:eval event-guide-eval
          (define transfer-event
            (string->abi-event
             "event Transfer(address indexed from, address indexed to, uint256 value)"))
          (define (print-transfer entry)
            (printf "  ~a -> ~a ~a\n"
                    (event-log-ref entry 'from) (event-log-ref entry 'to)
                    (event-log-ref entry 'value)))]

@racket[receipt-logs] gives a receipt's logs, and @racket[get-logs] fetches the logs a
@racket[log-filter] matches: from an address, of an event, with given values of its indexed
arguments.  The token's deploy is a transfer from the zero address, so every transfer is three
here, and one of them is to bob.

@racketblock[
(with-anvil ()
  (match-define (list alice bob carol _ ...) (eth-accounts))
  (parameterize ([current-account alice])
    (define token
      (deploy-contract init-code "constructor(string,string,uint256)" "Token" "TOK" 1000))
    (define receipt (contract-send token transfer bob 250))
    (match-define (list raw) (receipt-logs receipt))
    (printf "receipt\n")
    (print-transfer (decode-log transfer-event raw))
    (contract-send token transfer carol 100)
    (printf "every transfer\n")
    (for ([entry (in-list (get-logs (log-filter #:address token #:event transfer-event)))])
      (print-transfer entry))
    (printf "to bob\n")
    (define to-bob (log-filter #:address token #:event transfer-event #:where (hasheq 'to bob)))
    (for ([entry (in-list (get-logs to-bob))])
      (print-transfer entry))))]

With @racket[#:chunk], @racket[get-logs] fetches a long range of blocks in requests of that many
blocks, and halves a request the node refuses as too large.

See @filepath{examples/07-events.rkt}.

@subsection[#:tag "guide-bindings"]{Contract Bindings}

@racket[define-contract] reads a contract's ABI when its module is compiled, from a compiled
artifact, from Solidity's declarations or from s-expressions, and binds a procedure for each
function, named as in @secref["binding-contracts"].  Arguments are checked against the ABI before
anything is sent.

A @tt{view} function's procedure calls it and returns its outputs; any other sends a transaction
and returns the receipt.  With bytecode, as in an artifact, @racketidfont{-deploy} deploys the
contract.  Errors decode by name with no @racket[#:errors], and each event is bound with an
@racketidfont{-event} suffix, which @racket[contract-logs] takes.

@racketblock[
(define-contract token #:file "contracts/out/Token.sol/Token.json")

(define (print-refusal e)
  (define revert (exn->abi-revert e))
  (printf "refused ~a ~s\n" (abi-revert-name revert) (abi-revert-arguments revert)))]

As in @secref["guide-contracts"], bob's transfer of 1000 is refused with
@tt{InsufficientBalance}, here by name.

@racketblock[
(with-anvil ()
  (match-define (list alice bob _ ...) (eth-accounts))
  (parameterize ([current-account alice])
    (define gold (token-deploy "Gold" "GLD" 1000))
    (token-transfer gold bob 250)
    (printf "~a: alice ~a, bob ~a\n"
            (token-symbol gold) (token-balance-of gold alice) (token-balance-of gold bob))
    (with-handlers ([exn:fail:web3? print-refusal])
      (token-transfer gold alice 1000 #:from bob))
    (for ([entry (in-list (contract-logs gold token-transfer-event #:where (hasheq 'to bob)))])
      (printf "transfer to bob ~a\n" (event-log-ref entry 'value)))))]

@racket[contract-instance] binds an ABI read at run time to an address instead.

See @filepath{examples/08-contract-bindings.rkt}.

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
@racket[rlp-decode] reads one back, accepting only its one encoding.

@examples[#:eval rlp-eval
          (bytes->hex-string (rlp-encode (list #"cat" #"dog")))
          (rlp-decode (hex-string->bytes "0xc88363617483646f67"))]

RLP has no integers.  A number is written as a scalar, its big-endian bytes without leading
zeros, so zero is the empty byte string.

@examples[#:eval rlp-eval
          (bytes->hex-string (rlp-encode (list (natural->rlp-scalar 0) (natural->rlp-scalar 1024))))]

A contract's address is the last 20 bytes of the Keccak-256 hash of the list of its creator's
address and nonce, which is what @racket[create-address] computes.

@examples[#:eval rlp-eval
          (define sender "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266")
          (define preimage (rlp-encode (list (hex-string->bytes sender) (natural->rlp-scalar 1))))
          (address->checksum-string (bytes->hex-string (subbytes (keccak256 preimage) 12)))
          (create-address sender 1)]

See @filepath{examples/10-rlp.rkt}.

@subsection[#:tag "guide-units"]{Units}

Amounts of ether are exact integers of wei: a gwei is 10@superscript{9} wei and an ether
10@superscript{18}.  @racket[ether->wei] and @racket[gwei->wei] read an amount written as a decimal
string or given as an exact number, and @racket[wei->string] writes one back.

@examples[#:eval units-eval
          (ether->wei "1.5")
          (gwei->wei 30)
          (wei->string (* 21000 (gwei->wei 30)))]

Nothing is rounded.  An amount finer than one wei is refused, and so is a flonum, which cannot
hold most decimal amounts exactly.

@examples[#:eval units-eval
          (ether->wei 3/2)
          (eval:error (ether->wei "0.0000000000000000001"))
          (eval:error (ether->wei 1.5))]

See @filepath{examples/11-units.rkt}.

@subsection[#:tag "guide-transactions"]{Transactions}

A transaction is built from its fields, signed with a secret key, and sent to a node as raw
bytes.  @racket[eip1559-transaction] builds the kind most chains expect.  Amounts are in wei.

@examples[#:eval transaction-eval
          (define key
            (hex-string->secret-key
             "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))
          (define recipient "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")
          (define tx
            (eip1559-transaction #:chain-id 31337
                                 #:nonce 0
                                 #:max-priority-fee-per-gas (gwei->wei 2)
                                 #:max-fee-per-gas (gwei->wei 30)
                                 #:gas-limit 21000
                                 #:to recipient
                                 #:value (ether->wei "1.5")))
          (define signed (sign-transaction key tx))
          (bytes->hex-string (signed-transaction->bytes signed))
          (signed-transaction-hash signed)]

@racket[eth-send-raw-transaction] takes the raw bytes as a hex string and returns that hash.

@racketblock[
(eth-send-raw-transaction (bytes->hex-string (signed-transaction->bytes signed)))]

@racket[bytes->signed-transaction] reads raw bytes back.  It accepts only the one encoding of a
valid signed transaction, and recovers the sender.

@examples[#:eval transaction-eval
          (define parsed (bytes->signed-transaction (signed-transaction->bytes signed)))
          (signed-transaction-sender parsed)
          (equal? parsed signed)
          (transaction-value (signed-transaction-transaction parsed))
          (eval:error (bytes->signed-transaction (bytes 3)))]

See @filepath{examples/12-transactions.rkt}.

@subsection[#:tag "guide-signers"]{Signers and Messages}

A signer signs for an address: with a secret key held here, with an account a node holds, or
in any way a structure implementing @racket[gen:signer] chooses.  Code that takes a signer works
the same with each.  A local signer prints as its address, never its key.

@examples[#:eval signer-eval
          (define key
            (hex-string->secret-key
             "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))
          (define signer (local-signer key))
          signer]

Wallets sign text as an EIP-191 personal message: the Keccak-256 hash of a prefix, the message's
length in bytes and the message, so that no signed message is also a transaction.  A string is
signed as its UTF-8 bytes.

@examples[#:eval signer-eval
          (bytes->hex-string (hash-message "hello"))
          (define sig (signer-sign-message signer "hello"))
          (bytes->hex-string (signature->bytes sig))
          (recover-message-address sig "hello")
          (message-signed-by? sig "hello" (signer-address signer))
          (message-signed-by? sig "hello!" (signer-address signer))]

A signer signs transactions too, with @racket[signer-sign-transaction].

@racket[node-signer] signs with an account the node holds, through @tt{eth_sign} and
@tt{eth_signTransaction}.  What it returns is checked as for any signer: a signature that does
not recover its address, or a transaction other than the one asked for, raises
@racket[exn:fail:web3:signer].

See @filepath{examples/13-signers.rkt}.

@subsection[#:tag "guide-sending"]{Sending Transactions}

@racket[fill-transaction] completes a transaction from what the caller gives.  The node supplies
the rest: its chain id, the sender's next nonce, a gas estimate, and fees.  A chain whose blocks
have a base fee gets a type 2 transaction; one without gets a legacy one.

@examples[#:eval send-eval
          (define key
            (hex-string->secret-key
             "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"))
          (define signer (local-signer key))
          (define recipient "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")
          (define answers
            (hash "eth_chainId" "0x7a69"
                  "eth_estimateGas" "0x5208"
                  "eth_getTransactionCount" "0x0"
                  "eth_getBlockByNumber" (hasheq 'number "0x1" 'baseFeePerGas "0x3b9aca00")
                  "eth_maxPriorityFeePerGas" "0x3b9aca00"
                  "eth_sendRawTransaction"
                  "0x353911be23e52e6b62f32b2323be63d06541c4ab17382cde9ff87b3174c3c9f6"))
          (define node (mock-provider answers))
          (define tx (fill-transaction signer recipient #:value (ether->wei "1.5") #:provider node))
          tx
          (transaction-max-fee-per-gas tx)]

A field given is kept, and the node is not asked for it.

@racket[send-signed-transaction] fills a transaction, signs it with the signer, and sends the raw
bytes with @tt{eth_sendRawTransaction}.  @racket[send-signed-transaction/receipt] also waits for
the receipt.

@examples[#:eval send-eval
          (send-signed-transaction signer recipient #:value (ether->wei "1.5") #:provider node)]

Contracts take a signer wherever they take a sender: as @racket[#:from], as
@racket[current-account], or in a @racket[contract-instance].  A send from a signer is filled,
signed here and sent raw.  A send from an address is signed by the node, as with
@racket[send-transaction], which is how impersonated accounts send.  Calls and estimates use the
signer's address and sign nothing.

@racketblock[
(define-contract counter #:file "contracts/out/Counter.sol/Counter.json")
(parameterize ([current-account signer])
  (define address (counter-deploy))
  (counter-increment address)
  (counter-count address))]

See @filepath{examples/14-sending.rkt}.

@subsection[#:tag "guide-blocks"]{Blocks and Transactions}

A block read with @racket[#:full-transactions? #t] holds its transactions as
@racket[transaction-info?] values, with accessors such as @racket[transaction-info-type] and
@racket[transaction-info-value]; without, it holds their hashes.  Quantities are exact integers in wei, as everywhere.

@racketblock[
(define (print-transaction tx)
  (printf "  type ~a nonce ~a, ~a ether from ~a\n"
          (transaction-info-type tx)
          (transaction-info-nonce tx)
          (wei->string (transaction-info-value tx))
          (transaction-info-from tx)))]

Started with @tt{--no-mining}, anvil holds what it is sent until @racket[anvil-mine], so alice's
legacy, EIP-2930 and EIP-1559 transfers land in one block.
@racket[transaction-info->signed-transaction] rebuilds the legacy one as it was signed and
recovers alice as its sender.  @racket[eth-fee-history] gives, for each block, the priority fee
paid at the 25th and 75th percentiles of gas: none in the empty block 0, 1 and 2 gwei in block 1.
Its base fees have one entry more than its blocks: the last is the next block's.

@racketblock[
(with-anvil (#:args '("--no-mining"))
  (match-define (list alice bob _ ...) (eth-accounts))
  (send-transaction alice bob #:value (ether->wei 1) #:type 0)
  (send-transaction alice bob #:value (ether->wei 2) #:type 1)
  (send-transaction alice bob #:value (ether->wei 3)
                    #:max-priority-fee-per-gas (gwei->wei 2) #:max-fee-per-gas (gwei->wei 5))
  (anvil-mine)
  (define block (eth-get-block-by-number "latest" #:full-transactions? #t))
  (define transactions (block-transactions block))
  (printf "block ~a, gas used ~a, base fee ~a gwei, ~a transactions\n"
          (block-number block)
          (block-gas-used block)
          (wei->string (block-base-fee-per-gas block) 'gwei)
          (length transactions))
  (for-each print-transaction transactions)
  (match-define (list legacy _ _) transactions)
  (printf "legacy transaction rebuilt, signed by ~a\n"
          (signed-transaction-sender (transaction-info->signed-transaction legacy)))
  (define history (eth-fee-history 2 "latest" '(25 75)))
  (for ([height (in-naturals (fee-history-oldest-block history))]
        [rewards (in-list (fee-history-reward history))])
    (match-define (list low high) rewards)
    (printf "fee history, block ~a: priority fees ~a and ~a gwei\n"
            height (wei->string low 'gwei) (wei->string high 'gwei)))
  (printf "next base fee ~a gwei\n"
          (wei->string (last (fee-history-base-fee-per-gas history)) 'gwei)))]

@secref["Transactions_on_Chain"] and @secref["eth-block"] list every field of a transaction, a
block and a fee history.

See @filepath{examples/15-blocks.rkt}.

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
                              [contract-expr ->param-expr]
                              (code:line keyword [contract-expr #:default default-expr])]
                    [maybe-transform (code:line)
                                     (code:line #:result-transform transform-expr)
                                     (code:line #:result-reader reader-expr)])]{

Defines a function for the JSON-RPC method @racket[rpc-name], named
after it in kebab-case (@racket[eth_getBalance] defines
@racket[eth-get-balance]).  The function takes one positional argument
per @racket[arg-spec] without a @racket[keyword], an optional keyword argument per
@racket[arg-spec] with one, @racket[default-expr] when the call leaves it out, and an optional
@racket[#:provider], which no @racket[arg-spec] may name.  Each argument is the JSON parameter at
its spec's position.  The name is bound as syntax that
stands for the function wherever it appears, so it cannot be @racket[set!].

Each argument is checked against its @racket[contract-expr], which blames the
module of the call, and, when the spec gives a @racket[->param-expr], converted by it into
the JSON parameter.  The response's @tt{result} goes through
@racket[transform-expr] (default @racket[values]) and is checked against
@racket[result-contract-expr], a flat contract.  A result that
@racket[transform-expr] raises on or that fails the contract raises
@racket[exn:fail:web3] with the message
@racket["method: unexpected result value"], unless @racket[transform-expr] raises
@racket[exn:fail:web3] itself, which is raised as it is.  A @racket[reader-expr] takes the
place of @racket[transform-expr] and is applied to the method's name, a string, the result and
the function's arguments before their conversion, in their specs' order.

@racketblock[
(define-jsonrpc-endpoint eth_getBalance
  (-> string? [block-param/c block-param->string] exact-nonnegative-integer?)
  #:result-transform hex-string->number)
(define-jsonrpc-endpoint eth_getBlockByNumber
  (-> [block-param/c block-param->string] #:full-transactions? [boolean? #:default #f]
      (or/c #f hash?))
  #:result-transform (lambda (v) (and (hash? v) v)))
]}

@subsection{HTTP Providers}

@defproc[(url->http-provider [url string?]
                             [#:ssl-context ssl-context ssl-client-context?
                              (ssl-secure-client-context)])
         http-provider?]{

Returns a provider that sends JSON-RPC requests to @racket[url] over
HTTP or HTTPS.  The path and query of @racket[url] are kept, so URLs
that carry an API key in their path work as given.

An @tt{https} URL's connections use @racket[ssl-context], by default
@racket[(ssl-secure-client-context)] as called when the provider is made: it checks the
certificate against @racket[(ssl-default-verify-sources)] and the host name.  A refused
handshake sends nothing and raises @racket[exn:fail:web3] naming the host and port.  A context
from @racket[ssl-make-client-context] trusts a private certificate authority, or, with nothing
loaded, any certificate.  An @tt{http} URL takes no @racket[ssl-context].

The provider can be shared between threads.  An interrupted or failed
request never leaks its reply into a later one.  A lost connection, or
one closed by @racket[provider-close], is reopened on the next request.

@racketblock[
(define private-ca (ssl-make-client-context 'auto))
(ssl-load-verify-source! private-ca "ca.pem")
(ssl-set-verify! private-ca #t)
(ssl-set-verify-hostname! private-ca #t)
(url->http-provider "https://node.internal:8545" #:ssl-context private-ca)]

@examples[#:eval provider-eval
          (url->http-provider "https://127.0.0.1:8545"
                              #:ssl-context (ssl-make-client-context 'auto))]}

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
(define alice "0x00000000000000000000000000000000000a11ce")
(eth-get-balance alice 5 #:provider p)
(code:comment "=> 255")
(mock-provider-calls p)
(code:comment "=> '((\"eth_getBalance\" \"0x00000000000000000000000000000000000a11ce\" \"0x5\"))")
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

@defproc[(hex-string->number [s hex-string?]) exact-nonnegative-integer?]{
Reads a JSON-RPC quantity: @racket["0x"] and hexadecimal digits in either case.
@racket["0x"] alone reads as @racket[0].  A string without the prefix is not a quantity, even
when all its digits are decimal.

@examples[#:eval contract-eval
          (hex-string->number "0x10")
          (eval:error (hex-string->number "10"))]}

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

@defthing[reward-percentiles/c flat-contract?]{
Percentiles for @racket[eth-fee-history]: a list of reals between 0 and 100, strictly increasing
once each non-integer is written as a flonum, as the node reads them.}

@defthing[storage-slot/c flat-contract?
          #:value (integer-in 0 (sub1 (expt 2 256)))]{
A storage slot: a 256-bit word.}

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
@defproc[(eth-max-priority-fee-per-gas [#:provider provider (or/c #f provider?)
                                                   (current-provider)])
         exact-nonnegative-integer?]
@defproc[(eth-blob-base-fee [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?]
@defproc[(eth-accounts [#:provider provider (or/c #f provider?) (current-provider)])
         (listof checksum-address?)]
@defproc[(eth-block-number [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?])]{

@tt{web3_clientVersion}, @tt{net_version}, @tt{net_listening},
@tt{net_peerCount}, @tt{eth_chainId}, @tt{eth_syncing},
@tt{eth_gasPrice} (in wei), @tt{eth_maxPriorityFeePerGas} (in wei),
@tt{eth_blobBaseFee} (in wei), @tt{eth_accounts} and
@tt{eth_blockNumber}.  @racket[net-version] returns the network id as
the node reports it, a decimal string.}

@defproc[(eth-fee-history [block-count exact-positive-integer?]
                          [newest-block block-param/c]
                          [reward-percentiles reward-percentiles/c]
                          [#:provider provider (or/c #f provider?) (current-provider)])
         fee-history?]{
Base fees and gas used ratios of the @racket[block-count] blocks ending at @racket[newest-block],
and the priority fee paid at each of @racket[reward-percentiles]; see @racket[fee-history?].  A
reward row without one fee per percentile raises @racket[exn:fail:web3].

@examples[#:eval eth-eval
          (define node
            (mock-provider (hash "eth_feeHistory"
                                 (hasheq 'oldestBlock "0x10" 'baseFeePerGas '("0x7" "0x7")
                                         'gasUsedRatio '(0.5) 'reward '(("0x1" "0x2"))))))
          (define history (eth-fee-history 1 16 '(1/2 25) #:provider node))
          history
          (fee-history-reward history)
          (mock-provider-calls node)]}

@deftogether[(
@defproc[(eth-protocol-version [#:provider provider (or/c #f provider?) (current-provider)])
         string?]
@defproc[(eth-coinbase [#:provider provider (or/c #f provider?) (current-provider)])
         checksum-address?]
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

@defproc[(eth-get-balance [address address?]
                          [block block-param/c]
                          [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?]{
The balance of @racket[address] in wei.}

@defproc[(eth-get-transaction-count [address address?]
                                    [block block-param/c]
                                    [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?]{
The number of transactions sent from @racket[address]: its next nonce.}

@defproc[(eth-get-storage-at [address address?]
                             [slot exact-nonnegative-integer?]
                             [block block-param/c]
                             [#:provider provider (or/c #f provider?) (current-provider)])
         hex-string?]{
The 32-byte word at storage @racket[slot] of @racket[address].}

@defproc[(eth-get-proof [address address?]
                        [slots (listof storage-slot/c)]
                        [block block-param/c]
                        [#:provider provider (or/c #f provider?) (current-provider)])
         hash?]{
The Merkle proof of @racket[address] and of its storage @racket[slots] at @racket[block], as the
node's JSON.  Slots are sent as 32-byte words.}

@defproc[(eth-get-code [address address?]
                       [block block-param/c]
                       [#:provider provider (or/c #f provider?) (current-provider)])
         hex-string?]{
The code deployed at @racket[address], or @racket["0x"] for an account
without code.}

@subsection{Blocks}

@deftogether[(
@defproc[(eth-get-block-by-number [block block-param/c]
                                  [#:full-transactions? full-transactions? boolean? #f]
                                  [#:provider provider (or/c #f provider?) (current-provider)])
         (or/c #f block?)]
@defproc[(eth-get-block-by-hash [block-hash #,hash-argument-contract]
                                [#:full-transactions? full-transactions? boolean? #f]
                                [#:provider provider (or/c #f provider?) (current-provider)])
         (or/c #f block?)])]{
The block, or @racket[#f] for an unknown one.  Its @racket[block-transactions] are
@racket[transaction-info?] values when @racket[full-transactions?] is true and hashes otherwise; a
node that answers with the other form, or with a block whose number or hash is not the one asked
for, raises @racket[exn:fail:web3].}

@defproc[(eth-get-block-receipts [block block-param/c]
                                 [#:provider provider (or/c #f provider?) (current-provider)])
         (or/c #f (listof receipt?))]{
The receipts of the block's transactions, in order, or @racket[#f] for an unknown block.  A
receipt the node gets wrong raises @racket[exn:fail:web3] naming its index and the field.}

@deftogether[(
@defproc[(eth-get-transaction-by-block-hash-and-index
          [block-hash #,hash-argument-contract]
          [index exact-nonnegative-integer?]
          [#:provider provider (or/c #f provider?) (current-provider)])
         (or/c #f transaction-info?)]
@defproc[(eth-get-transaction-by-block-number-and-index
          [block block-param/c]
          [index exact-nonnegative-integer?]
          [#:provider provider (or/c #f provider?) (current-provider)])
         (or/c #f transaction-info?)])]{
The transaction at @racket[index] in a block, or @racket[#f].  A transaction whose block hash,
block number or index is not the one asked for raises @racket[exn:fail:web3].}

@deftogether[(
@defproc[(eth-get-block-transaction-count-by-hash
          [block-hash #,hash-argument-contract]
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

@defproc[(eth-create-access-list [tx (and/c hash? jsexpr?)]
                                 [block block-param/c]
                                 [#:provider provider (or/c #f provider?) (current-provider)])
         hash?]{
The access list and gas used of @racket[tx] at @racket[block], as the node's JSON; a reverting
@racket[tx] gives an @racket['error] field, not an exception.}

@defproc[(eth-send-transaction [tx (and/c hash? jsexpr?)]
                               [#:provider provider (or/c #f provider?) (current-provider)])
         #,hash-string-contract]{
Asks the node to sign and send @racket[tx] from an account it manages,
such as one of anvil's development accounts, and returns the
transaction hash, in lowercase.  An answer that is not a hash raises @racket[exn:fail:web3].
@racket[send-transaction] sends from keywords.}

@defproc[(eth-send-raw-transaction [signed-tx hex-string?]
                                   [#:provider provider (or/c #f provider?) (current-provider)])
         hex-string?]{
Submits an already signed, serialized transaction and returns its hash, such as
@racket[(bytes->hex-string (signed-transaction->bytes stx))]; see @racket[sign-transaction].}

@defproc[(eth-sign [address address?] [data hex-string?]
                   [#:provider provider (or/c #f provider?) (current-provider)])
         hex-string?]{
The node's EIP-191 signature of @racket[data] with an account it holds, as 65 bytes; see
@racket[node-signer].}

@defproc[(eth-sign-transaction [tx (and/c hash? jsexpr?)]
                               [#:provider provider (or/c #f provider?) (current-provider)])
         hex-string?]{
The raw bytes of @racket[tx] signed by the account the node holds for its @racket['from]; a node
that answers with an object gives its @racket['raw].  See @racket[transaction->jsexpr].}

@defproc[(eth-get-transaction-by-hash
          [tx-hash #,hash-argument-contract]
          [#:provider provider (or/c #f provider?) (current-provider)])
         (or/c #f transaction-info?)]{
A transaction, or @racket[#f] for an unknown one.  A transaction whose hash is not
@racket[tx-hash], in any case, raises @racket[exn:fail:web3].}

@defproc[(eth-get-transaction-receipt
          [tx-hash #,hash-argument-contract]
          [#:provider provider (or/c #f provider?) (current-provider)])
         (or/c #f receipt?)]{
The receipt of a mined transaction, or @racket[#f] before it is mined.  A receipt the node gets
wrong raises @racket[exn:fail:web3] naming the method and the field, and so does a receipt of
another transaction than @racket[tx-hash].

@examples[#:eval results-eval
          (eth-get-transaction-receipt
           (hash-ref receipt-json 'transactionHash)
           #:provider (mock-provider (hash "eth_getTransactionReceipt" receipt-json)))
          (eval:error
           (eth-get-transaction-receipt
            (hash-ref receipt-json 'transactionHash)
            #:provider (mock-provider
                        (hash "eth_getTransactionReceipt"
                              (hash-set receipt-json 'gasUsed "zz")))))]}

@defproc[(eth-get-raw-transaction-by-hash
          [tx-hash #,hash-argument-contract]
          [#:provider provider (or/c #f provider?) (current-provider)])
         (or/c 'null hex-string?)]{
The signed, serialized transaction, an even number of hex digits as
@racket[eth-send-raw-transaction] takes it, or @racket['null] for an unknown one; see
@racket[bytes->signed-transaction].}

@defproc[(eth-get-logs [filter (and/c hash? jsexpr?)]
                       [#:provider provider (or/c #f provider?) (current-provider)])
         (listof hash?)]{
The logs matching @racket[filter], a hash table with keys such as
@racket['address], @racket['topics], @racket['fromBlock] and
@racket['toBlock].}

@defproc[(wait-for-receipt [tx-hash #,hash-argument-contract]
                           [#:timeout timeout real? 30.0]
                           [#:interval interval real? 0.002]
                           [#:provider provider (or/c #f provider?) (current-provider)])
         receipt?]{
Polls for the receipt of @racket[tx-hash], starting every
@racket[interval] seconds and backing off to every quarter second.
Raises @racket[exn:fail:web3] if no receipt appears within
@racket[timeout] seconds, or if the receipt is not @racket[tx-hash]'s.}

@subsection{Filters}

@defproc[(eth-new-filter [filter (and/c hash? jsexpr?)]
                         [#:provider provider (or/c #f provider?) (current-provider)])
         hex-string?]{
Installs a log filter on the node and returns its id, which the other filter endpoints take as
the node wrote it.  @racket[filter] is as for @racket[eth-get-logs], such as
@racket[(log-filter->jsexpr f)].

@examples[#:eval eth-eval
          (define node (mock-provider (hash "eth_newFilter" "0x1f")))
          (eth-new-filter
           (log-filter->jsexpr (log-filter #:address "0x5FbDB2315678afecb367f032d93F642f64180aa3"))
           #:provider node)
          (mock-provider-calls node)]}

@deftogether[(
@defproc[(eth-new-block-filter [#:provider provider (or/c #f provider?) (current-provider)])
         hex-string?]
@defproc[(eth-new-pending-transaction-filter
          [#:provider provider (or/c #f provider?) (current-provider)])
         hex-string?])]{
Install a filter for new block hashes, or for new pending transaction hashes.}

@defproc[(eth-get-filter-changes [filter-id hex-string?]
                                 [#:provider provider (or/c #f provider?) (current-provider)])
         (listof (or/c hex-string? hash?))]{
What the filter has matched since it was last polled: logs for a log filter, hashes otherwise.}

@defproc[(eth-get-filter-logs [filter-id hex-string?]
                              [#:provider provider (or/c #f provider?) (current-provider)])
         (listof hash?)]{
Every log a log filter matches.}

@defproc[(eth-uninstall-filter [filter-id hex-string?]
                               [#:provider provider (or/c #f provider?) (current-provider)])
         boolean?]{
Removes the filter; @racket[#f] if the node had none with that id.}

@subsection{Receipts}

@defmodule[web3/eth/receipt]

A receipt is the outcome of a mined transaction.  @racketmodname[web3/eth] provides everything
here.  In the examples, @racket[receipt-json] is a token transfer's receipt as anvil sent it.

@defproc[(receipt? [v any/c]) boolean?]{
Recognizes receipts.  A receipt prints with its transaction hash, place, status and gas used.}

@deftogether[(@defproc[(receipt-transaction-hash [r receipt?])
                       #,hash-string-contract]
              @defproc[(receipt-transaction-index [r receipt?]) uint64/c]
              @defproc[(receipt-block-hash [r receipt?]) #,hash-string-contract]
              @defproc[(receipt-block-number [r receipt?]) uint64/c]
              @defproc[(receipt-from [r receipt?]) checksum-address?]
              @defproc[(receipt-to [r receipt?]) (or/c #f checksum-address?)]
              @defproc[(receipt-type [r receipt?]) (or/c #f byte?)]
              @defproc[(receipt-status [r receipt?]) (or/c #f 0 1)]
              @defproc[(receipt-root [r receipt?]) (or/c #f #,hash-string-contract)]
              @defproc[(receipt-cumulative-gas-used [r receipt?]) uint64/c]
              @defproc[(receipt-gas-used [r receipt?]) uint64/c]
              @defproc[(receipt-effective-gas-price [r receipt?]) (or/c #f uint256/c)]
              @defproc[(receipt-contract-address [r receipt?]) (or/c #f checksum-address?)]
              @defproc[(receipt-logs-bloom [r receipt?])
                       (and/c bytes? (property/c bytes-length (=/c 256)))]
              @defproc[(receipt-blob-gas-used [r receipt?]) (or/c #f uint64/c)]
              @defproc[(receipt-blob-gas-price [r receipt?]) (or/c #f uint256/c)])]{
The fields of a receipt, each the node's field of the same name in camelCase.  @racket[#f] is a
field the node leaves out or sends as @tt{null}: @racket[receipt-to] for a contract creation,
@racket[receipt-contract-address] for anything else, the blob fields for other transactions,
@racket[receipt-type] from nodes before Berlin, @racket[receipt-effective-gas-price] from nodes
before London, @racket[receipt-root] after Byzantium and @racket[receipt-status] before it.  The
logs are @racket[receipt-logs].}

@defproc[(receipt-succeeded? [r receipt?]) boolean?]{
Whether @racket[(receipt-status r)] is @racket[1].  A receipt with only a root raises
@racket[exn:fail:web3].}

@defproc[(jsexpr->receipt [v any/c]) receipt?]{
A receipt from the node's JSON.  Raises @racket[exn:fail:web3], naming the field, for anything
else.  Unknown fields are kept in @racket[receipt->jsexpr].

@examples[#:eval results-eval
          (define receipt (jsexpr->receipt receipt-json))
          receipt
          (receipt-gas-used receipt)
          (receipt-succeeded? receipt)
          (eval:error (jsexpr->receipt (hash-set receipt-json 'gasUsed "zz")))]}

@defproc[(receipt->jsexpr [r receipt?]) hash?]{
The node's object @racket[r] was read from.}

@subsection{Transactions on Chain}

@defmodule[web3/eth/transaction-info]

A transaction as a node describes it: its fields, its signature and where it was mined.
@racketmodname[web3/eth] provides everything here.  In the examples,
@racket[transaction-info-json] is a mined legacy transaction as anvil sent it.

@defproc[(transaction-info? [v any/c]) boolean?]{
Recognizes transactions read from a node.  One prints with its hash, type, nonce and place.}

@deftogether[(@defproc[(transaction-info-hash [t transaction-info?]) #,hash-string-contract]
              @defproc[(transaction-info-block-hash [t transaction-info?])
                       (or/c #f #,hash-string-contract)]
              @defproc[(transaction-info-block-number [t transaction-info?]) (or/c #f uint64/c)]
              @defproc[(transaction-info-block-timestamp [t transaction-info?])
                       (or/c #f uint64/c)]
              @defproc[(transaction-info-transaction-index [t transaction-info?])
                       (or/c #f uint64/c)]
              @defproc[(transaction-info-from [t transaction-info?]) checksum-address?]
              @defproc[(transaction-info-type [t transaction-info?]) (or/c #f byte?)]
              @defproc[(transaction-info-chain-id [t transaction-info?]) (or/c #f uint64/c)]
              @defproc[(transaction-info-nonce [t transaction-info?]) uint64/c]
              @defproc[(transaction-info-to [t transaction-info?]) (or/c #f checksum-address?)]
              @defproc[(transaction-info-value [t transaction-info?]) uint256/c]
              @defproc[(transaction-info-data [t transaction-info?]) bytes?]
              @defproc[(transaction-info-gas-limit [t transaction-info?]) uint64/c]
              @defproc[(transaction-info-gas-price [t transaction-info?]) (or/c #f uint256/c)]
              @defproc[(transaction-info-max-priority-fee-per-gas [t transaction-info?])
                       (or/c #f uint256/c)]
              @defproc[(transaction-info-max-fee-per-gas [t transaction-info?])
                       (or/c #f uint256/c)]
              @defproc[(transaction-info-max-fee-per-blob-gas [t transaction-info?])
                       (or/c #f uint256/c)]
              @defproc[(transaction-info-access-list [t transaction-info?])
                       (or/c #f access-list/c)]
              @defproc[(transaction-info-blob-versioned-hashes [t transaction-info?])
                       (or/c #f (listof #,hash-string-contract))]
              @defproc[(transaction-info-v [t transaction-info?]) (or/c #f uint256/c)]
              @defproc[(transaction-info-y-parity [t transaction-info?]) (or/c #f 0 1)]
              @defproc[(transaction-info-r [t transaction-info?]) (or/c #f uint256/c)]
              @defproc[(transaction-info-s [t transaction-info?]) (or/c #f uint256/c)])]{
The fields of a transaction, each the node's field of the same name in camelCase, except
@racket[transaction-info-data], which reads @tt{input}, and @racket[transaction-info-gas-limit],
which reads @tt{gas}.  @racket[#f] is a field the node leaves out or sends as @tt{null}: the
block fields while the transaction is pending, @racket[transaction-info-to] for a contract
creation, the fields other types of transaction lack, and the type and signature from nodes that
send none.}

@defproc[(transaction-info->signed-transaction [t transaction-info?]) signed-transaction?]{
The signed transaction @racket[t] describes, for types 0, 1 and 2.  A transaction without a type
is legacy unless it has a field only typed transactions have.  A legacy transaction's chain id
comes from @tt{v}, and must agree with @tt{chainId} when the node sends both.  Raises
@racket[exn:fail:web3] for another type, for a field the type needs that is missing or out of
range, and when the transaction's hash is not @racket[(transaction-info-hash t)] or its
signature's sender is not @racket[(transaction-info-from t)]; a high @racket[s] or a signature
that recovers no address raises @racket[exn:fail:web3:signature].

@examples[#:eval results-eval
          (define stx
            (transaction-info->signed-transaction (jsexpr->transaction-info transaction-info-json)))
          stx
          (signed-transaction-sender stx)
          (eval:error
           (transaction-info->signed-transaction
            (jsexpr->transaction-info (hash-set transaction-info-json 'type "0x3"))))]}

@defproc[(jsexpr->transaction-info [v any/c]) transaction-info?]{
A transaction from the node's JSON.  Raises @racket[exn:fail:web3], naming the field, for
anything else.  Unknown fields are kept in @racket[transaction-info->jsexpr].

@examples[#:eval results-eval
          (define info (jsexpr->transaction-info transaction-info-json))
          info
          (transaction-info-gas-limit info)
          (eval:error (jsexpr->transaction-info (hash-set transaction-info-json 'gas "zz")))]}

@defproc[(transaction-info->jsexpr [t transaction-info?]) hash?]{
The node's object @racket[t] was read from.}

@subsection[#:tag "eth-block"]{Blocks}

@defmodule[web3/eth/block]

Blocks, the withdrawals they hold, and fee histories.  @racketmodname[web3/eth] provides
everything here.  In the examples, @racket[block-json] is a block as anvil sent it.

@defproc[(block? [v any/c]) boolean?]{
Recognizes blocks.  A block prints with its number, hash and number of transactions.}

@deftogether[(@defproc[(block-hash [b block?]) (or/c #f #,hash-string-contract)]
              @defproc[(block-parent-hash [b block?]) #,hash-string-contract]
              @defproc[(block-sha3-uncles [b block?]) (or/c #f #,hash-string-contract)]
              @defproc[(block-miner [b block?]) (or/c #f checksum-address?)]
              @defproc[(block-state-root [b block?]) #,hash-string-contract]
              @defproc[(block-transactions-root [b block?]) #,hash-string-contract]
              @defproc[(block-receipts-root [b block?]) #,hash-string-contract]
              @defproc[(block-logs-bloom [b block?])
                       (or/c #f (and/c bytes? (property/c bytes-length (=/c 256))))]
              @defproc[(block-difficulty [b block?]) (or/c #f uint256/c)]
              @defproc[(block-number [b block?]) uint64/c]
              @defproc[(block-gas-limit [b block?]) uint64/c]
              @defproc[(block-gas-used [b block?]) uint64/c]
              @defproc[(block-timestamp [b block?]) uint64/c]
              @defproc[(block-extra-data [b block?]) (or/c #f bytes?)]
              @defproc[(block-mix-hash [b block?]) (or/c #f #,hash-string-contract)]
              @defproc[(block-nonce [b block?])
                       (or/c #f (and/c bytes? (property/c bytes-length (=/c 8))))]
              @defproc[(block-size [b block?]) (or/c #f uint64/c)]
              @defproc[(block-base-fee-per-gas [b block?]) (or/c #f uint256/c)]
              @defproc[(block-withdrawals-root [b block?]) (or/c #f #,hash-string-contract)]
              @defproc[(block-withdrawals [b block?]) (or/c #f (listof withdrawal?))]
              @defproc[(block-blob-gas-used [b block?]) (or/c #f uint64/c)]
              @defproc[(block-excess-blob-gas [b block?]) (or/c #f uint64/c)]
              @defproc[(block-parent-beacon-block-root [b block?])
                       (or/c #f #,hash-string-contract)]
              @defproc[(block-requests-hash [b block?]) (or/c #f #,hash-string-contract)]
              @defproc[(block-uncles [b block?]) (or/c #f (listof #,hash-string-contract))]
              @defproc[(block-transactions [b block?])
                       (or/c (listof #,hash-string-contract) (listof transaction-info?))])]{
The fields of a block, each the node's field of the same name in camelCase.  @racket[#f] is a
field the node leaves out or sends as @tt{null}: the hash, miner, nonce and logs bloom of a pending
block, the fields of forks after the block's, and fields some nodes do not send.  A block without a
number is refused.  The transactions are hashes or @racket[transaction-info?] values, never both.}

@defproc[(block-transaction-hashes [b block?]) (listof #,hash-string-contract)]{
The hashes of the block's transactions, in either form.}

@defproc[(jsexpr->block [v any/c]) block?]{
A block from the node's JSON.  Raises @racket[exn:fail:web3], naming the field, for anything
else.  Unknown fields are kept in @racket[block->jsexpr].

@examples[#:eval results-eval
          (define block (jsexpr->block block-json))
          block
          (block-base-fee-per-gas block)
          (block-transaction-hashes block)
          (eval:error (jsexpr->block (hash-set block-json 'transactions (list 42))))]}

@defproc[(block->jsexpr [b block?]) hash?]{
The node's object @racket[b] was read from.}

@defproc[(withdrawal? [v any/c]) boolean?]{
Recognizes a validator's withdrawal from the beacon chain.  One prints with its index, validator,
address and amount.}

@deftogether[(@defproc[(withdrawal-index [w withdrawal?]) uint64/c]
              @defproc[(withdrawal-validator-index [w withdrawal?]) uint64/c]
              @defproc[(withdrawal-address [w withdrawal?]) checksum-address?]
              @defproc[(withdrawal-amount [w withdrawal?]) uint256/c])]{
The fields of a withdrawal, each the node's field of the same name in camelCase.  The amount is in
gwei.}

@defproc[(jsexpr->withdrawal [v any/c]) withdrawal?]{
A withdrawal from the node's JSON.  Raises @racket[exn:fail:web3], naming the field, for
anything else.

@examples[#:eval results-eval
          (jsexpr->withdrawal withdrawal-json)]}

@defproc[(withdrawal->jsexpr [w withdrawal?]) hash?]{
The node's object @racket[w] was read from.}

@defproc[(fee-history? [v any/c]) boolean?]{
Recognizes a fee history, the answer to @racket[eth-fee-history].  One prints with the blocks it
covers.}

@deftogether[(@defproc[(fee-history-oldest-block [h fee-history?]) uint64/c]
              @defproc[(fee-history-base-fee-per-gas [h fee-history?]) (listof uint256/c)]
              @defproc[(fee-history-gas-used-ratio [h fee-history?]) (listof (real-in 0 1))]
              @defproc[(fee-history-reward [h fee-history?])
                       (or/c #f (listof (listof uint256/c)))]
              @defproc[(fee-history-base-fee-per-blob-gas [h fee-history?])
                       (or/c #f (listof uint256/c))]
              @defproc[(fee-history-blob-gas-used-ratio [h fee-history?])
                       (or/c #f (listof (real-in 0 1)))])]{
The fields of a fee history, each the node's field of the same name in camelCase: per block from
the oldest, with one more base fee, for the block after the newest.  The rewards are the
priority fees at each percentile asked for.  With no percentiles they are @racket['()] from anvil
and @racket[#f] from a node that leaves them out; the blob fields are @racket[#f] from nodes
without blobs.}

@defproc[(jsexpr->fee-history [v any/c]) fee-history?]{
A fee history from the node's JSON.  Raises @racket[exn:fail:web3], naming the field, for
anything else, and for lists of the wrong length: one more base fee and blob base fee than there
are blocks, and one reward row and blob gas ratio per block, or no rewards.}

@defproc[(fee-history->jsexpr [h fee-history?]) hash?]{
The node's object @racket[h] was read from.}

@section{Addresses}

@defmodule[web3/address]

@defproc[(address? [v any/c]) boolean?]{
Whether @racket[v] is an address: @racket["0x"] and 40 hex digits, all lowercase, all uppercase,
or in mixed case matching their @hyperlink["https://eips.ethereum.org/EIPS/eip-55"]{EIP-55}
checksum.  Every procedure that takes an address takes an @racket[address?].

@examples[#:eval abi-eval
          (address? "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed")
          (address? "0x5AAEB6053F3E94C9B9A09F33669435E7EF1BEAED")
          (address? "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed")
          (address? "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD")]}

@defproc[(address->checksum-string [address address?]) string?]{
The @hyperlink["https://eips.ethereum.org/EIPS/eip-55"]{EIP-55} form of @racket[address].

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

@subsection[#:tag "account-messages"]{Messages}

@defproc[(hash-message [message (or/c bytes? string?)]) digest/c]{
The @hyperlink["https://eips.ethereum.org/EIPS/eip-191"]{EIP-191} digest of @racket[message]
as a personal message (version @racket[#x45]): the Keccak-256 hash of the byte @racket[#x19],
@racket["Ethereum Signed Message:\n"], the length of @racket[message] in bytes written in decimal,
and @racket[message].  A string is hashed as its UTF-8 bytes and never read as hexadecimal.

@examples[#:eval account-eval
          (bytes->hex-string (hash-message "hello"))
          (equal? (hash-message "héllo") (hash-message (string->bytes/utf-8 "héllo")))
          (equal? (hash-message "0xdeadbeef") (hash-message (bytes #xde #xad #xbe #xef)))]}

@defproc[(sign-message [key secret-key?] [message (or/c bytes? string?)]) signature?]{
@racket[(sign-digest key (hash-message message))]: what wallets sign for @tt{personal_sign}.
No error shows @racket[key].}

@defproc[(recover-message-address [sig signature?] [message (or/c bytes? string?)])
         (or/c #f checksum-address?)]{
@racket[(recover-address sig (hash-message message))].}

@defproc[(message-signed-by? [sig signature?] [message (or/c bytes? string?)]
                             [address address?])
         boolean?]{
Whether @racket[sig] is @racket[address]'s signature of @racket[message].  A high @racket[s] is
accepted, as by @racket[recover-address].

@examples[#:eval account-eval
          (message-signed-by? (sign-message key "hello") "hello"
                              "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266")]}

@subsection[#:tag "account-errors"]{Errors}

@defstruct*[(exn:fail:web3:key exn:fail:web3) ()]{
Raised for a secret key that is zero or not less than @racket[secp256k1-order].}

@defstruct*[(exn:fail:web3:signature exn:fail:web3) ()]{
Raised for bytes that do not encode a signature, by @racket[sign-digest] for a signature
whose recovery id Ethereum cannot encode, by @racket[signed-transaction], and by
@racket[signer-sign-message] for a node's signature bytes.}

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

@section[#:tag "units"]{Units}

@defmodule[web3/units]

Amounts as exact integers of wei, or of a token's smallest unit, read from and written as
decimal strings.  Nothing is rounded.  A unit is @racket['wei], @racket['gwei]
(10@superscript{9} wei), @racket['ether] (10@superscript{18} wei), or a number of decimals, such
as an ERC-20 token's @tt{decimals()}.

@defproc[(units->wei [amount (or/c string? (and/c rational? exact?))]
                     [unit (or/c 'wei 'gwei 'ether (integer-in 0 255))])
         exact-integer?]{
@racket[amount] of @racket[unit], in wei.  A string is an optional @racket["-"] and ASCII
digits with at most one @racket["."]; there must be a digit, and nothing else is allowed: no
spaces, @racket["+"], exponents or separators.  Raises @racket[exn:fail:web3:units] with reason
@racket['syntax] for any other string, and @racket['precision] when @racket[amount] is not a
whole number of wei.  Zeros past the unit's decimals are allowed.

@examples[#:eval units-eval
          (units->wei "0.25" 'gwei)
          (units->wei "1.500000000" 6)
          (eval:error (units->wei "1.0000005" 6))]}

@deftogether[(@defproc[(ether->wei [amount (or/c string? (and/c rational? exact?))])
                       exact-integer?]
              @defproc[(gwei->wei [amount (or/c string? (and/c rational? exact?))])
                       exact-integer?])]{
@racket[(units->wei amount 'ether)] and @racket[(units->wei amount 'gwei)], raising under their
own names.

@examples[#:eval units-eval
          (gwei->wei "1.5")
          (ether->wei 1/1000)]}

@defproc[(wei->string [n exact-integer?]
                      [unit (or/c 'wei 'gwei 'ether (integer-in 0 255)) 'ether]
                      [#:min-fraction-digits min-digits (integer-in 0 255) 0])
         string?]{
@racket[n] wei in @racket[unit], as a decimal number with every digit it needs and no more,
padded with zeros to at least @racket[min-digits] places after the point.  For every
@racket[n], @racket[(units->wei (wei->string n unit) unit)] is @racket[n].

@examples[#:eval units-eval
          (wei->string 1500000000000000000)
          (wei->string -1 'gwei)
          (wei->string 1500000 6 #:min-fraction-digits 2)]}

@defstruct*[(exn:fail:web3:units exn:fail:web3)
            ([index (or/c #f exact-nonnegative-integer?)]
             [reason (or/c 'syntax 'precision)])]{
An amount that is not a decimal number (@racket['syntax]) or is finer than one wei
(@racket['precision]).  @racket[index] is the position in the string, counted from 0: the
unexpected character, the string's length when it has no digits, or the first nonzero digit past
the unit's decimals.  It is @racket[#f] for an exact number.}

@section[#:tag "transactions"]{Transactions}

@defmodule[web3/transaction]

Legacy (with and without @hyperlink["https://eips.ethereum.org/EIPS/eip-155"]{EIP-155}),
@hyperlink["https://eips.ethereum.org/EIPS/eip-2930"]{EIP-2930} and
@hyperlink["https://eips.ethereum.org/EIPS/eip-1559"]{EIP-1559} transactions: built from their
fields, signed, written as the raw bytes nodes take, and read back.  Values are immutable.
Addresses are returned in their EIP-55 form, and an address given is an @racket[address?].

@subsection{Building Transactions}

@deftogether[(@defthing[uint64/c flat-contract?]
              @defthing[uint256/c flat-contract?])]{
Integers from 0 below 2@superscript{64} and below 2@superscript{256}: the ranges of a
transaction's quantities.}

@defproc[(legacy-transaction [#:chain-id chain-id (or/c #f uint64/c)]
                             [#:nonce nonce uint64/c]
                             [#:gas-price gas-price uint256/c]
                             [#:gas-limit gas-limit uint64/c]
                             [#:to to (or/c #f address?)]
                             [#:value value uint256/c 0]
                             [#:data data bytes? #""])
         legacy-transaction?]{
A transaction of type 0.  @racket[to] is @racket[#f] for a contract creation.  @racket[chain-id]
is @racket[#f] for a transaction signed without replay protection, valid on every chain.

@examples[#:eval transaction-eval
          (define eip-155
            (legacy-transaction #:chain-id 1
                                #:nonce 9
                                #:gas-price (gwei->wei 20)
                                #:gas-limit 21000
                                #:to "0x3535353535353535353535353535353535353535"
                                #:value (ether->wei 1)))
          eip-155]}

@defproc[(eip2930-transaction [#:chain-id chain-id uint64/c]
                              [#:nonce nonce uint64/c]
                              [#:gas-price gas-price uint256/c]
                              [#:gas-limit gas-limit uint64/c]
                              [#:to to (or/c #f address?)]
                              [#:value value uint256/c 0]
                              [#:data data bytes? #""]
                              [#:access-list access-list access-list/c '()])
         eip2930-transaction?]{
A transaction of type 1, with an access list: the accounts and storage keys it will touch.}

@defproc[(eip1559-transaction [#:chain-id chain-id uint64/c]
                              [#:nonce nonce uint64/c]
                              [#:max-priority-fee-per-gas max-priority-fee-per-gas uint256/c]
                              [#:max-fee-per-gas max-fee-per-gas uint256/c]
                              [#:gas-limit gas-limit uint64/c]
                              [#:to to (or/c #f address?)]
                              [#:value value uint256/c 0]
                              [#:data data bytes? #""]
                              [#:access-list access-list access-list/c '()])
         eip1559-transaction?]{
A transaction of type 2, which pays at most @racket[max-fee-per-gas] per unit of gas, including at
most @racket[max-priority-fee-per-gas] to the block's producer.  The priority fee may not exceed
the maximum.

@examples[#:eval transaction-eval
          (eval:error (eip1559-transaction #:chain-id 1 #:nonce 0
                                           #:max-priority-fee-per-gas 2 #:max-fee-per-gas 1
                                           #:gas-limit 21000 #:to #f))]}

@defthing[access-list/c flat-contract?]{
Lists whose elements are an address followed by 32-byte storage keys, such as
@racket[(list (list "0x70997970C51812dc3A010C7d01b50e0d17dc79C8" (make-bytes 32 0)))].}

@deftogether[(@defproc[(transaction? [v any/c]) boolean?]
              @defproc[(legacy-transaction? [v any/c]) boolean?]
              @defproc[(eip2930-transaction? [v any/c]) boolean?]
              @defproc[(eip1559-transaction? [v any/c]) boolean?])]{
Whether @racket[v] is an unsigned transaction, or one of each kind.  A signed transaction is not
a @racket[transaction?].}

@deftogether[(@defproc[(transaction-type [tx transaction?]) (or/c 0 1 2)]
              @defproc[(transaction-chain-id [tx transaction?]) (or/c #f uint64/c)]
              @defproc[(transaction-nonce [tx transaction?]) uint64/c]
              @defproc[(transaction-gas-price
                        [tx (or/c legacy-transaction? eip2930-transaction?)])
                       uint256/c]
              @defproc[(transaction-max-priority-fee-per-gas [tx eip1559-transaction?])
                       uint256/c]
              @defproc[(transaction-max-fee-per-gas [tx eip1559-transaction?]) uint256/c]
              @defproc[(transaction-gas-limit [tx transaction?]) uint64/c]
              @defproc[(transaction-to [tx transaction?]) (or/c #f checksum-address?)]
              @defproc[(transaction-value [tx transaction?]) uint256/c]
              @defproc[(transaction-data [tx transaction?]) (and/c bytes? immutable?)]
              @defproc[(transaction-access-list
                        [tx (or/c eip2930-transaction? eip1559-transaction?)])
                       access-list/c])]{
The fields.  @racket[transaction-gas-price] applies to types 0 and 1, the two fee accessors to
type 2, and @racket[transaction-access-list] to types 1 and 2.

@examples[#:eval transaction-eval
          (transaction-to eip-155)
          (transaction-chain-id eip-155)]}

@subsection{Signing}

@defproc[(transaction->bytes [tx transaction?]) bytes?]{
The bytes a signature of @racket[tx] covers: the EIP-2718 type byte and the RLP of the fields.
Type 0 has no type byte, and its fields are followed by the chain id and two zeros (EIP-155), or,
without a chain id, by nothing.

@examples[#:eval transaction-eval
          (bytes->hex-string (transaction->bytes eip-155))
          (bytes->hex-string
           (transaction->bytes
            (legacy-transaction #:chain-id #f #:nonce 9 #:gas-price (gwei->wei 20)
                                #:gas-limit 21000 #:to #f)))]}

@defproc[(transaction-signing-hash [tx transaction?]) digest/c]{
The Keccak-256 hash of @racket[(transaction->bytes tx)], the digest a signer signs.

@examples[#:eval transaction-eval
          (bytes->hex-string (transaction-signing-hash eip-155))]}

@defproc[(sign-transaction [key secret-key?] [tx transaction?]) signed-transaction?]{
@racket[tx] signed with @racket[key].  The signature's @racket[s] is always low (EIP-2) and the
same arguments always give the same bytes.  No error shows @racket[key].

@examples[#:eval transaction-eval
          (define eip-155-key
            (hex-string->secret-key
             "0x4646464646464646464646464646464646464646464646464646464646464646"))
          (define eip-155-signed (sign-transaction eip-155-key eip-155))
          (signed-transaction-hash eip-155-signed)]}

@defproc[(signed-transaction [tx transaction?] [sig signature?]) signed-transaction?]{
@racket[tx] with a signature made elsewhere of @racket[(transaction-signing-hash tx)].  Raises
@racket[exn:fail:web3:signature] when @racket[sig]'s @racket[s] is high or it recovers no address.
Also a match pattern, @racket[(signed-transaction tx sig)].

@examples[#:eval transaction-eval
          (equal? (signed-transaction tx (sign-digest key (transaction-signing-hash tx)))
                  (sign-transaction key tx))]}

@defproc[(signed-transaction? [v any/c]) boolean?]{
Whether @racket[v] is a signed transaction.}

@deftogether[(@defproc[(signed-transaction-transaction [stx signed-transaction?]) transaction?]
              @defproc[(signed-transaction-signature [stx signed-transaction?]) signature?]
              @defproc[(signed-transaction-sender [stx signed-transaction?]) checksum-address?]
              @defproc[(signed-transaction-hash [stx signed-transaction?])
                       #,hash-string-contract])]{
The transaction, its signature, the address that signed it, and its hash: the Keccak-256 hash of
its raw bytes, which identifies it to a node.}

@subsection{Raw Transactions}

@defproc[(signed-transaction->bytes [stx signed-transaction?]) bytes?]{
The raw bytes nodes take, a fresh byte string.}

@defproc[(bytes->signed-transaction [bs bytes?]) signed-transaction?]{
The signed transaction @racket[bs] encodes.  @racket[bs] is untrusted: it must be exactly one
canonical encoding of a type 0, 1 or 2 transaction whose fields are in range, whose @racket[s] is
low (EIP-2), and whose signature recovers a sender.  The first byte is read first: one that is
neither a type nor the start of an RLP list raises @racket[exn:fail:web3:raw-transaction] with
reason @racket['type].  After it, RLP that is not canonical raises @racket[exn:fail:web3:rlp], and
anything else @racket[exn:fail:web3:raw-transaction].

@examples[#:eval transaction-eval
          (define raw (signed-transaction->bytes eip-155-signed))
          (signed-transaction-sender (bytes->signed-transaction raw))
          (eval:error (bytes->signed-transaction (bytes)))
          (eval:error (bytes->signed-transaction (bytes #xb8)))
          (eval:error (bytes->signed-transaction (subbytes raw 0 100)))]}

@defstruct*[(exn:fail:web3:raw-transaction exn:fail:web3)
            ([field (or/c #f 'chain-id 'nonce 'gas-price 'max-priority-fee-per-gas
                          'max-fee-per-gas 'gas-limit 'to 'value 'data 'access-list
                          'v 'y-parity 'r 's)]
             [reason (or/c 'type 'shape 'leading-zero 'range 'length 'high-s 'no-sender)])]{
Raw bytes that are not a signed transaction, other than RLP that is not canonical.
@racket[field] names the field at fault, or is @racket[#f] when the fault is the whole
transaction's.}

@subsection{Transactions as JSON}

@defproc[(transaction->jsexpr [tx transaction?] [#:from from address?]) hash?]{
@racket[tx] as the transaction object nodes take for @tt{eth_signTransaction} and
@tt{eth_sendTransaction}, sent from @racket[from]: a @racket[jsexpr?] whose numbers are hex
strings.  Every field is given: @racket['type],
@racket['nonce], @racket['gas], the fee fields of its type, @racket['value], @racket['data],
@racket['chainId] and, for types 1 and 2, @racket['accessList].  @racket['to] is left out for a
contract creation, and @racket['chainId] for a legacy transaction without one.

@examples[#:eval transaction-eval
          (transaction->jsexpr eip-155 #:from "0x9d8A62f656a8d1615C1294fd71e9CFb3E4855A4F")]}

@section[#:tag "signers"]{Signers}

@defmodule[web3/signer]

Signing for an address without knowing how.  What every signer returns is checked: a signature
has a low @racket[s] and recovers the signer's address, and a signed transaction is the one asked
for.  No procedure here shows an argument in an error.

@defproc[(signer? [v any/c]) boolean?]{
Whether @racket[v] is an instance of a structure implementing @racket[gen:signer].}

@defproc[(signer-address [signer signer?]) checksum-address?]{
The address @racket[signer] signs for, respelled in its EIP-55 checksum whatever the case of the
address its method returns.}

@defproc[(signer-sign-digest [signer signer?] [digest digest/c]) signature?]{
@racket[digest] signed as given.  Raises @racket[exn:fail:web3:signer] for a signer that signs no
digests, such as a node signer.}

@defproc[(signer-sign-message [signer signer?] [message (or/c bytes? string?)]) signature?]{
@racket[message] signed as an EIP-191 personal message; see @racket[hash-message].}

@defproc[(signer-sign-transaction [signer signer?] [tx transaction?]) signed-transaction?]{
@racket[tx] signed by @racket[signer].}

@subsection{Local Signers}

@defproc[(local-signer [key secret-key?]) local-signer?]{
A signer holding @racket[key].  It signs as @racket[sign-digest], @racket[sign-message] and
@racket[sign-transaction] do, and prints as its address.

@examples[#:eval signer-eval
          (equal? (signer-sign-message signer "hello") (sign-message key "hello"))]}

@defproc[(local-signer? [v any/c]) boolean?]{
Whether @racket[v] is a signer made by @racket[local-signer].}

@subsection{Node Signers}

@defproc[(node-signer [address address?]
                      [#:provider provider (or/c #f provider?) #f])
         node-signer?]{
A signer for an account the node behind @racket[provider], or @racket[(current-provider)] when it
is @racket[#f], holds: messages through @racket[eth-sign], transactions through
@racket[eth-sign-transaction].  It sends nothing until it signs, and cannot sign a digest.  The
node's errors, such as for an account it does not hold, raise @racket[exn:fail:web3:rpc].

@examples[#:eval signer-eval
          (define node-account (node-signer "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266"))
          node-account
          (eval:error (signer-sign-digest node-account (hash-message "hello")))]}

@defproc[(node-signer? [v any/c]) boolean?]{
Whether @racket[v] is a signer made by @racket[node-signer].}

@subsection{Writing a Signer}

@defthing[gen:signer any/c]{
A generic interface.  An implementation defines @racket[signer-address] and at least one of
@racket[signer-sign-digest], @racket[signer-sign-message] and @racket[signer-sign-transaction];
messages and transactions are signed through @racket[signer-sign-digest] when their own method is
not defined.  A method receives a fresh mutable copy of the digest or of the message's bytes;
changing it changes neither the caller's bytes nor what the result is checked against.

@examples[#:eval signer-eval
          (struct liar (key)
            #:methods gen:signer
            [(define (signer-address l) "0x70997970C51812dc3A010C7d01b50e0d17dc79C8")
             (define (signer-sign-digest l digest) (sign-digest (liar-key l) digest))])
          (eval:error (signer-sign-message (liar key) "hello"))]}

@subsection[#:tag "signer-errors"]{Errors}

@defstruct*[(exn:fail:web3:signer exn:fail:web3)
            ([reason (or/c 'unsupported 'malformed 'wrong-signer 'wrong-transaction 'high-s)])]{
A signer that cannot do what was asked, or that returned something other than a low-@racket[s]
signature by its address of what was asked.}

@section[#:tag "sending"]{Sending Transactions}

@defmodule[web3/send]

Completing transactions from a node, signing them with a signer, and sending them.  No error
here shows a signer or a key.

@defproc/filling[(fill-transaction [from (or/c address? signer?)]
                                   [to (or/c #f address?)])
                 transaction?]{
A transaction from @racket[from], an address or a signer, to @racket[to] (@racket[#f] creates a
contract).  It has the fields given; each field given as @racket[#f] comes from the node behind
@racket[provider]:
@itemlist[
 @item{the nonce, from @tt{eth_getTransactionCount} of @racket["pending"];}
 @item{the gas limit, from @tt{eth_estimateGas}, with no margin;}
 @item{for type 2, the priority fee from @tt{eth_maxPriorityFeePerGas}, and a fee cap of twice
       the latest block's base fee plus the priority fee;}
 @item{for types 0 and 1, the gas price from @tt{eth_gasPrice}.}]
The node is always asked for its chain id.  A @racket[chain-id] other than the node's raises
@racket[exn:fail:web3:fill].

The type is @racket[type] when given, else the one the fee fields given allow.  When those allow
type 2 and another, the latest block decides: 2 when it has a base fee, else 0, or 1 with an
@racket[access-list].  Fields of a type other than @racket[type], or of two types, raise
@racket[exn:fail:contract] before any request.  An estimate that reverts raises
@racket[exn:fail:web3:rpc:revert], decoded against @racket[errors].

@examples[#:eval send-eval
          (eval:error (fill-transaction signer recipient #:chain-id 1 #:provider node))
          (eval:error
           (fill-transaction signer recipient #:type 0 #:access-list '() #:provider node))
          (define legacy-node
            (mock-provider (hash-set answers "eth_getBlockByNumber" (hasheq 'number "0x1"))))
          (eval:error (fill-transaction signer recipient #:type 2 #:provider legacy-node))]}

@deftogether[(@defproc/filling[(send-signed-transaction [signer signer?]
                                                        [to (or/c #f address?)])
                               #,hash-string-contract]
              @defproc/filling[(send-signed-transaction/receipt
                                [signer signer?]
                                [to (or/c #f address?)])
                               receipt?])]{
@racket[(fill-transaction signer to ....)] signed by @racket[signer] and sent with
@tt{eth_sendRawTransaction}, returning its hash, or with @racket[send-signed-transaction/receipt]
its receipt once mined.  While @racket[signer] signs, @racket[current-provider] is
@racket[provider].  When filling or signing fails, or the transaction signed is not from the
address it was filled for, nothing is sent.  The node's refusals, such as @tt{nonce too low},
raise @racket[exn:fail:web3:rpc], with a revert decoded against @racket[errors].  An error whose
message starts @tt{sent transaction} means the transaction was sent: a node that answers with
another hash raises one.  Waiting and failures are as for @racket[contract-send]: status 0 raises
@racket[exn:fail:web3:transaction].

Each call reads the pending nonce, so sends in a row from one signer get consecutive nonces.
Sends from one signer in several threads at once may get the same one.}

@deftogether[(@defproc/filling[(send-transaction [from address?]
                                                 [to (or/c #f address?)])
                               #,hash-string-contract]
              @defproc/filling[(send-transaction/receipt
                                [from address?]
                                [to (or/c #f address?)])
                               receipt?])]{
A transaction from @racket[from], an account the node holds, to @racket[to] (@racket[#f] creates a
contract), signed and sent by the node with @tt{eth_sendTransaction}, returning its hash, or with
@racket[send-transaction/receipt] its receipt once mined.  The fields given are sent, with
@racket[from] and @racket[to] checksummed; a nonce and fees not given are left to the node.  When
@racket[type], or the fee fields given, make the transaction type 0 or 1, as for
@racket[fill-transaction], a gas price not given is @tt{eth_gasPrice}, and type 1 sends an empty
access list when none is given, so the node sends that type.  A
@racket[gas-limit] not given is @tt{eth_estimateGas} of the transaction without its type, nonce and
fees, as for @racket[fill-transaction], and a revert there raises
@racket[exn:fail:web3:rpc:revert], decoded against @racket[errors].  The node is asked for its
chain id only when @racket[chain-id] is given, and one other than the node's raises
@racket[exn:fail:web3:fill].  Fields of a type other than @racket[type], or of two types, raise
@racket[exn:fail:contract] before any request.  An answer that is not a hash raises
@racket[exn:fail:web3].  Waiting and failures are as for @racket[send-signed-transaction/receipt];
a failed transaction is replayed without its nonce and fees.

@examples[#:eval send-eval
          (define node-account
            (mock-provider
             (hash "eth_estimateGas" "0x5208"
                   "eth_sendTransaction"
                   "0x353911be23e52e6b62f32b2323be63d06541c4ab17382cde9ff87b3174c3c9f6")))
          (send-transaction "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266" recipient
                            #:value (ether->wei "1.5") #:provider node-account)
          (mock-provider-calls node-account)]}

@defstruct*[(exn:fail:web3:fill exn:fail:web3) ([reason (or/c 'chain-id 'no-base-fee 'fee-cap)])]{
A transaction the node's answers cannot complete: a @racket[#:chain-id] other than the node's, a
type 2 transaction on a chain without a base fee, or a @racket[#:max-fee-per-gas] below the
node's priority fee.  Nothing has been signed or sent.}

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
       (list @tt{address} @elem{a string: @tt{0x} and 40 hex digits in any case; the encoders take
                                only an @racket[address?]})
       (list @tt{bool} @elem{@racket[#t] or @racket[#f]})
       (list @tt{bytesN} @elem{a byte string of exactly N bytes})
       (list @tt{bytes} @elem{a byte string})
       (list @tt{string} @elem{a string, written as UTF-8})
       (list @tt{T[]} @elem{a list of @tt{T} values})
       (list @tt{T[k]} @elem{a list of exactly k @tt{T} values})
       (list @tt{(T1,...,Tn)} @elem{a list of n values, in order}))]

Integers must be exact, and vectors are not lists.  @racket[abi-value?] cannot compute a
checksum, so it accepts a mixed-case address that the encoders, and so every call, send and
topic, refuse with @racket[exn:fail:web3:abi:value].

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
manages or from signers.  Signatures are written as for @racket[encode-call]; a call's
outputs follow its inputs, as in @racket["balanceOf(address)(uint256)"].
Arguments and results are as in @secref["abi-values"].  A revert raises
@racket[exn:fail:web3:rpc:revert], decoded against @racket[errors] and the
built-in @tt{Error} and @tt{Panic}.

@defparam[current-account account (or/c #f address? signer?) #:value #f]{
The sender when no @racket[#:from] is given: an address the node sends from, or a signer.
Sending with no sender raises
@racket[exn:fail:web3]; a call with none has no @tt{from}.}

@defproc[(contract-call [target (or/c address? contract-instance?)]
                        [function (or/c string? symbol? abi-signature?)]
                        [arg any/c] ...
                        [#:from from (or/c #f address? signer?) (current-account)]
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

@defproc[(contract-send [target (or/c address? contract-instance?)]
                        [function (or/c string? symbol? abi-signature?)]
                        [arg any/c] ...
                        [#:from from (or/c #f address? signer?) (current-account)]
                        [#:value value exact-nonnegative-integer? 0]
                        [#:gas gas (or/c #f exact-positive-integer?) #f]
                        [#:errors errors (listof (or/c string? abi-signature?)) '()]
                        [#:provider provider (or/c #f provider?) (current-provider)])
         receipt?]{
Sends a transaction calling @racket[function] from @racket[from], and
returns its receipt once mined.  Outputs in
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

From a signer, the transaction is completed as by @racket[fill-transaction], with @racket[gas]
as its gas limit and @racket[value] its value, so a @racket[gas] of 2@superscript{64} or more or
a @racket[value] of 2@superscript{256} or more raises @racket[exn:fail:contract] before any
request.  It is then signed by the signer and sent with @tt{eth_sendRawTransaction}.  An error
whose message starts @tt{sent transaction} means the transaction was sent.  From an address, the
node signs it (@tt{eth_sendTransaction}).  Calls and estimates from a signer use its address.

@racketblock[
(define receipt (contract-send token "transfer(address,uint256)" bob 250 #:from alice))
(receipt-succeeded? receipt)
(contract-send token "transfer(address,uint256)" bob 250 #:from (local-signer key))
]}

@deftogether[(
@defproc[(deploy-contract [init-code (or/c bytes? (and/c string? #px"^0x(?:[0-9a-fA-F]{2})*$"))]
                          [constructor (or/c string? abi-signature?) "constructor()"]
                          [arg any/c] ...
                          [#:from from (or/c #f address? signer?) (current-account)]
                          [#:value value exact-nonnegative-integer? 0]
                          [#:gas gas (or/c #f exact-positive-integer?) #f]
                          [#:errors errors (listof (or/c string? abi-signature?)) '()]
                          [#:provider provider (or/c #f provider?) (current-provider)])
         checksum-address?]
@defproc[(deploy-contract/receipt
          [init-code (or/c bytes? (and/c string? #px"^0x(?:[0-9a-fA-F]{2})*$"))]
          [constructor (or/c string? abi-signature?) "constructor()"]
          [arg any/c] ...
          [#:from from (or/c #f address? signer?) (current-account)]
          [#:value value exact-nonnegative-integer? 0]
          [#:gas gas (or/c #f exact-positive-integer?) #f]
          [#:errors errors (listof (or/c string? abi-signature?)) '()]
          [#:provider provider (or/c #f provider?) (current-provider)])
         (values checksum-address? receipt?)])]{
Deploys @racket[init-code] with the @racket[arg]s encoded as the inputs of
@racket[constructor], a signature named @tt{constructor}, and returns the
new contract's address, and with @racket[deploy-contract/receipt] its
receipt.  Gas, senders and failures are as for @racket[contract-send].

@racketblock[
(define token
  (deploy-contract token-code "constructor(string,string,uint256)" "Token" "TOK" 1000000))
]}

@defproc[(create-address [sender address?]
                         [nonce (integer-in 0 (- (expt 2 64) 2))])
         checksum-address?]{
The address of the contract @racket[sender] creates with @racket[nonce].  That is the last 20
bytes of the Keccak-256 hash of the RLP list of @racket[sender]'s bytes and
@racket[(natural->rlp-scalar nonce)]; see @secref["guide-rlp"].

@examples[#:eval contract-eval
          (create-address "0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266" 0)]}

@defproc[(create2-address [deployer address?]
                          [salt (or/c (and/c bytes? (property/c bytes-length (=/c 32)))
                                      (integer-in 0 (sub1 (expt 2 256))))]
                          [init-code (or/c bytes? (and/c string? #px"^0x(?:[0-9a-fA-F]{2})*$"))])
         checksum-address?]{
The address @tt{CREATE2} gives @racket[init-code] deployed by
@racket[deployer] with @racket[salt], 32 bytes or an integer.

@examples[#:eval contract-eval
          (create2-address "0x0000000000000000000000000000000000000000" 0 #"\0")]}

@defstruct*[(exn:fail:web3:transaction exn:fail:web3)
            ([receipt receipt?] [revert (or/c #f abi-revert?)])]{
A transaction that was mined and failed.  @racket[revert] is what replaying
it with @racket[eth-call] at its block reverted with, or @racket[#f] when the
replay did not revert or could not be made.  The replay runs after the whole
block, so it may not reproduce the failure when other transactions in the
block changed the state it depends on.}

@subsection{Contract Instances}

@defproc[(contract-instance [abi contract-abi?] [address address?]
                            [#:provider provider (or/c #f provider?) #f]
                            [#:from from (or/c #f address? signer?) #f])
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
                       (or/c #f address? signer?)])]{}

@defproc[(contract-estimate-gas [target (or/c address? contract-instance?)]
                                [function (or/c string? symbol? abi-signature?)]
                                [arg any/c] ...
                                [#:from from
                                 (or/c #f address? signer?) (current-account)]
                                [#:value value exact-nonnegative-integer? 0]
                                [#:errors errors (listof (or/c string? abi-signature?)) '()]
                                [#:provider provider (or/c #f provider?) (current-provider)])
         exact-nonnegative-integer?]{
The gas a @racket[contract-send] of the same arguments would be given.  A
call that would revert raises @racket[exn:fail:web3:rpc:revert].}

@defproc[(contract-logs [target (or/c address? contract-instance?)]
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
                                   [#:from from
                                    (or/c #f address? signer?) (current-account)]
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
any other a @racket[contract-send], with their keywords (@racket[#:from] may be a signer;
@racket[#:value]
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

@deftogether[(@defproc[(log-address [l log?]) checksum-address?]
              @defproc[(log-topics [l log?])
                       (and/c (listof #,hash-string-contract)
                              (property/c length (<=/c 4)))]
              @defproc[(log-data [l log?]) bytes?]
              @defproc[(log-block-number [l log?]) (or/c #f uint64/c)]
              @defproc[(log-block-hash [l log?]) (or/c #f #,hash-string-contract)]
              @defproc[(log-block-timestamp [l log?]) (or/c #f uint64/c)]
              @defproc[(log-transaction-hash [l log?])
                       (or/c #f #,hash-string-contract)]
              @defproc[(log-transaction-index [l log?]) (or/c #f uint64/c)]
              @defproc[(log-index [l log?]) (or/c #f uint64/c)]
              @defproc[(log-removed? [l log?]) boolean?])]{
The fields of a log, each the node's field of the same name in camelCase, except that
@racket[log-index] reads @tt{logIndex}: a checksummed address, up to four lowercase topics, the
data, and its place in the chain, @racket[#f] while pending or when the node leaves it out.
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

@defproc[(event-topic [e abi-event-spec/c]) (or/c #f #,hash-string-contract)]{
Topic 0 of @racket[e]'s logs, or @racket[#f] for an anonymous event,
which has none.

@examples[#:eval event-eval
          (event-topic "Transfer(address indexed from, address indexed to, uint256 value)")]}

@defproc[(abi-value->topic [t abi-type-spec/c] [v any/c]) #,hash-string-contract]{
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
                                              address?
                                              (non-empty-listof address?))
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

@defproc[(receipt-logs [receipt receipt?]
                       [#:events events (listof abi-event-spec/c) '()])
         (listof log?)]{
The logs of @racket[receipt], decoded with @racket[decode-logs].

@examples[#:eval results-eval
          (receipt-logs (jsexpr->receipt receipt-json))
          (receipt-logs (jsexpr->receipt receipt-json)
                        #:events (list "Transfer(address indexed, address indexed, uint256)"))]}

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

@defproc[(anvil-set-code [address address?]
                         [code hex-string?]
                         [#:provider provider (or/c #f provider?) (current-provider)])
         void?]{
Replaces the code at @racket[address].}

@defproc[(anvil-set-balance [address address?]
                            [wei exact-nonnegative-integer?]
                            [#:provider provider (or/c #f provider?) (current-provider)])
         void?]{
Sets the balance of @racket[address].}

@defproc[(anvil-set-storage-at [address address?]
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
          [address address?]
          [#:provider provider (or/c #f provider?) (current-provider)])
         void?]
@defproc[(anvil-stop-impersonating-account
          [address address?]
          [#:provider provider (or/c #f provider?) (current-provider)])
         void?])]{
Lets @racket[send-transaction] send from @racket[address] without
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
@defproc[(debug-trace-transaction [tx-hash #,hash-argument-contract]
                                  [options (and/c hash? jsexpr?)]
                                  [#:provider provider (or/c #f provider?) (current-provider)])
         hash?])]{

Struct-log traces of a call or a mined transaction, with one entry per
executed opcode in @racket['structLogs].  @racket[options] follows
geth's tracer configuration, such as
@racket[(hasheq 'enableMemory #t)].}
