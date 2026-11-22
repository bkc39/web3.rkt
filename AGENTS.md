# AGENTS.md

Guidance for agents working in this repository.

## What this is

`web3`: an Ethereum client library for Racket.  One package, one collection
(`web3`), one manual (`scribblings/web3.scrbl`).  Consumers are yul and
bezzle, which pin it as a flake input.

| module | holds |
|---|---|
| `rpc.rkt` | providers, `rpc-call`, errors, hex helpers, `define-jsonrpc-endpoint` (whose argument specs may be optional keywords, and whose `#:result-reader` gets the method name, the result and the call's arguments) |
| `eth.rkt` | `web3_*`, `net_*`, `eth_*` endpoints (fees, blocks, receipts, proofs, filters), `wait-for-receipt`; an `internal` submodule holds `wait-for-receipt/who`, the poll every wait runs, under the caller's `who` and with the caller's reader |
| `private/eth-params.rkt` | the endpoints' parameter contracts and their conversions to the node's JSON: block parameters, reward percentiles, storage slots, JSON objects, whose address fields `transaction-object/c` and `filter-object/c` check with `address/c`, naming the field; `eth.rkt` exports the documented ones |
| `abi.rkt` | the contract ABI: encoders, decoders, selectors, reverts; an `internal` submodule of entry points that take the caller's `who`, and `encoding-failure`, the value check every encoder and topic runs, `abi/type.rkt`'s with the address checksum |
| `contract.rkt` | deploying, calls and sends, contract instances, define-contract, contract addresses; an `internal` submodule of entry points that take the caller's `who` |
| `private/contract-call.rkt` | the calls, sends, estimates and deploys `contract.rkt` and bound procedures run, under their caller's `who`; the sending itself is `private/send.rkt`'s |
| `send.rkt` | filling transactions from a node, signing them with a signer and sending them raw, and sends from an address the node signs (`send-transaction`); every procedure under `opaque->` |
| `private/send.rkt` | filling, raw sends, node-signed sends (`send-from-node`, which contract sends from an address share), and the estimate/send/wait/replay every send runs, under the caller's `who`; `define-fill-keywords`, the one table of fill keywords their formals, their contracts and the fields a send names come from; JSON objects through `transaction.rkt`'s field mapping |
| `private/contracts.rkt` | the argument contracts the send and contract entry points share: senders, `to`, errors, providers |
| `private/bindings.rkt` | `define-contract` and `contract-bindings-out`, and the procedures they bind |
| `private/define-contract.rkt` | define-contract's expansion-time half; reads JSON at phase 1, never keccak |
| `private/binding-rules.rkt` | the keywords a bound procedure accepts and the messages for what it refuses, shared by the expansion-time checks and the procedures |
| `private/abi-json.rkt` | reading JSON ABIs and artifacts from ports and files |
| `address.rkt` | `address?`, the one check of an address argument (lowercase, uppercase or EIP-55), and EIP-55 checksums; requires only `racket/base` libraries, `keccak` and `private/hex.rkt`; a `private` submodule holds `digest->address`, the address in the last 20 bytes of a hash, and the unchecked `address->checksum-string` that respells what a node or signer returns |
| `account.rkt` | secret keys (libsecp256k1's), addresses, `sign-digest`, EIP-191 messages, `recover-address`, signatures and their 65 bytes; every procedure under `opaque->`, the constructor too: `signature` is a match expander that, as an expression, is the checked constructor, so the struct cannot be extended or copied around it; no network, JSON or node modules; a `private` submodule holds `recoverable->signature`, `sign-digest/who` and `bytes->signature/who` |
| `signer.rkt` | `gen:signer`, local and node signers, and the checks on what any signer returns; every procedure under `opaque->`; an `internal` submodule of entry points that take the caller's `who` |
| `private/signer.rkt` | the `define-generics` form of `gen:signer`, its methods exported with a `generic-` prefix so `signer.rkt` can give the public names to the checked procedures; racket/base and racket/generic only, so `private/argument.rkt` can name a signer |
| `rlp.rkt` | RLP: `rlp-encode`, strict `rlp-decode`, scalars; an `internal` submodule of entry points that take the caller's `who`; requires only `racket/base` libraries and `private/exn`, `hex`, `who` |
| `units.rkt` | amounts: decimal strings and exact numbers in wei, gwei, ether or a token's decimals, and back; requires only `racket/base` libraries and `private/exn.rkt` |
| `transaction.rkt` | legacy, EIP-2930 and EIP-1559 transactions: values, encodings, signing, strict parsing; `transaction->jsexpr`, built without JSON modules; an `internal` submodule holds the parser under the caller's `who`, its reasons, the wire field table, `fields->transaction` (a transaction from a table of its fields, unchecked), `signed-transaction/who`, `field-json-key` and `field->jsexpr` (the one mapping of fields to the node's JSON), the scalar rules of `private/transaction-rules.rkt`, `transaction/c` and `transaction-difference`, none of them public names; no network or JSON modules |
| `private/opaque.rkt` | `opaque/c` and `opaque->`, contracts whose errors name the kind of value, never the value; copied from libsecp256k1, which does not export them, with the kinds only web3 has (`private/argument.rkt`'s `describe-value`) and required keyword arguments added; `describe-kind`, which knows secret and public keys, is registered with `private/argument.rkt` when this module runs. Also the contracts web3's modules share: `secret-key/c` (`account.rkt`, `transaction.rkt`, `signer.rkt`), and the digest and message contracts (`account.rkt`, `transaction.rkt`, `signer.rkt`); `opaque->` keeps `private/argument.rkt`'s contracts as they are, makes every flat contract opaque and refuses, when it is built, any other contract that is not flat, since Racket's combinators print the value they refuse |
| `private/argument.rkt` | `address/c` and `hash-argument/c`, the contracts of every address and hash argument, field and topic; `argument/c`, which makes one from a noun, a predicate and its reasons (`to`), and `address-or/c`, an address or one other kind (senders, a contract call's target); `one-or-more/c`, one of them or a non-empty list; `argument-list/c`, a list of at most so many elements of an `or/c` of the flat contracts and at most one of them it is given, a precondition its contract checks (topics); `argument/c`'s `#:project` and `raise-refusal`, for contracts that look inside a value (`private/eth-params.rkt`'s objects). Chaperone contracts whose first-order checks are their predicates, so an `or/c` of flat contracts and one of them keeps its projection, which raises `exn:fail:contract:blame` with the one short message: who, which argument (an ordinal or a keyword) or field, the value, the reason, whom it blames. `listof` and the other container contracts print a value of the wrong shape and an `or/c` of two that are not flat falls back to Racket's message, so lists of them are `one-or-more/c` or `argument-list/c`; `tests/agents.rkt` reads the library's sources for an argument contract at any depth inside `listof`, `non-empty-listof`, `list/c`, `vectorof`, `cons/c`, `box/c` or `hash/c`, and for an `or/c` with two disjuncts that hold an argument contract or are arrows (`->`, `->*`, `->i`), where an argument contract is a call of `argument/c`, `address-or/c`, `one-or-more/c`, `argument-list/c` or `private/eth-params.rkt`'s `fields/c`, `address/c`, `hash-argument/c`, or a name `define`d to a form that holds one. The value is shown only when it is a string of `0x` and at most 40 hex digits; any other is described by the registered describer, never shown: `describe-value`, then `describe-kind` once `private/opaque.rkt` has run, so a program that has loaded only ffi-free modules sees a secret key as a value of another kind. No libsecp256k1, JSON or network modules, so `abi/event.rkt` and `eth/log.rkt` can use it |
| `private/transaction-rules.rkt` | the rules on a transaction's scalars that `transaction.rkt` and `eth/transaction-info.rkt` share: legacy `v`, y-parity, curve scalars, fee caps; requires only `account.rkt` |
| `private/signature.rkt` | the unchecked `signature` struct, its printer and its 65 bytes, shared by `account.rkt`, which exports it under contracts, and `private/argument.rkt`, whose `describe-value` names a signature |
| `abi/type.rkt` | ABI types, signatures and value checks; phase-1 safe, so it requires only `racket/base` libraries, `private/exn.rkt` and `private/hex.rkt`; a `private` submodule holds the reader and contracts other modules share, and `arguments-failure`, whose `#:address-ok?` is the address check it cannot compute |
| `abi/event-type.rkt` | event descriptions (`abi-event`, string and s-expression forms); phase-1 safe like `abi/type.rkt`, plus `private/who.rkt`; a `private` submodule holds the readers under their caller's `who` |
| `abi/description.rkt` | ABI items and contract descriptions from strings, s-expressions and JSON; phase-1 safe, as abi/type.rkt |
| `abi/event.rkt` | event topics, `event-topics`, decoding logs into `event-log`s; an `internal` submodule of entry points that take the caller's `who` |
| `eth/log.rkt` | logs, a `define-node-record` table (the struct is `log-struct`; nothing binds `log`); an `internal` submodule holds the reader under the caller's `who`, the table and what `abi/event.rkt` extends |
| `eth/receipt.rkt` | receipts, a `define-node-record` table, and `receipt-succeeded?`; an `internal` submodule holds the reader under the caller's `who`, the check of a receipt's transaction hash, the table and the logs accessor `event.rkt` uses |
| `eth/transaction-info.rkt` | transactions as a node describes them, a `define-node-record` table, and `transaction-info->signed-transaction`, which rebuilds the signed transaction and checks the node's hash and sender against it; an `internal` submodule holds the readers under the caller's `who`, the checks of the asked hash, block and index, the table and its noun |
| `eth/block.rkt` | blocks, withdrawals and fee histories, three `define-node-record` tables; a block's transactions are all hashes or all `transaction-info`s; an `internal` submodule holds the readers under the caller's `who`, the block reader that checks the block and form asked for, the fee history reader that checks one reward per percentile, and the tables and their nouns |
| `private/record.rkt` | `define-node-record`: one field table per node object, from which its struct, accessors, contracts, reader, printer hook and `T-fields`, its `field-row`s, come; the field kinds, each a reader under the caller's `who` and a contract; `T-noun`, the noun its messages use; `asked-reader`, a reader that checks the record's hash against the one asked for, and `check-asked` and `check-block-asked`, the checks it and the block readers share |
| `private/field-contracts.rkt` | the field contracts records and transactions share: hashes, `to`, `uint64/c`, `uint256/c`, 32 bytes, access lists (flat, as `transaction.rkt` exports it, so its addresses are `address?`) |
| `event.rkt` | `log-filter`, `get-logs` (chunks, halving on a refused range), `receipt-logs` |
| `abi/vectors.rkt` | conformance vectors, data in `abi/vectors/encode.rktd` (regenerate with `racket tests/generate-abi-vectors.rkt`) and the hand-written `abi/vectors/decode.rktd` |
| `private/abi-decode.rkt` | the decoder core and its reasons; `abi.rkt` puts contracts on it |
| `private/abi-item.rkt` | ABI item structs and their string and s-expression readers under the caller's `who`; phase-1 safe; `abi/description.rkt` puts contracts on it |
| `private/hex.rkt` | unchecked conversions between bytes, hex strings and naturals, and the shapes of hex data, addresses and hashes; callers check their input |
| `private/exn.rkt` | the exception structs (`exn:fail:web3`, `…:rpc`, `…:rpc:revert`, `…:transaction`, `…:fill`, `…:abi`, `…:abi:type`, `…:abi:value`, `…:abi:decode`, `…:key`, `…:rlp`, `…:raw-transaction`, `…:signature`, `…:signer`, `…:units`) and `raise-web3-error`, the one raiser of a plain `exn:fail:web3` under a `who`; racket/base only |
| `private/node.rkt` | checked conversions of node data (hex, quantities, addresses, hashes, naturals of a given width); no JSON or transport modules |
| `private/who.rkt` | `define/who`, which defines `name` and `name/who`, the same procedure taking the `who` its messages start with |
| `anvil.rkt` | anvil's methods and process management |
| `examples/` | numbered programs (`NN-name.rkt`), each with a `main` submodule; `common.rkt` reads the compiled fixture contracts for the tests |

## Commands

Work inside the dev shell; it pins Racket, anvil and solc and links this checkout.

```bash
nix develop -c raco test -x -p web3      # unit tests, anvil tests, every example
nix develop -c racket examples/00-provider.rkt
nix develop -c raco setup --check-pkg-deps --unused-pkg-deps --pkgs web3   # docs + dependency check
nix build -L                             # hermetic build and tests
```

The gates are in `.racket-dev.rktd`.  After changing `info.rkt` deps, update
the `racket-deps` hash in `flake.nix` (the build prints the new one).

## Rules

- Every public binding has a contract, an entry in `scribblings/web3.scrbl`
  and a test.  API documentation lives in the manual, not in comments.
- The manual is terse and shows Racket only.  It never mentions Nix.
- Tests that need a node start their own with `with-anvil` and skip when
  `(current-anvil-path)` is `#f`: the package catalog's build server has no
  anvil.  Anything that can be tested without a node is.
- Fixture contracts live in `examples/contracts/`.  After editing a `.sol`,
  run `racket tests/generate-contracts.rkt` in the dev shell;
  `tests/fixtures.rkt` fails when sources and artifacts disagree.
- Every user-visible feature has a test and a reference entry.  Each area of
  the library has an example: a short program, one flow, whose `main`
  submodule prints a few lines; a Guide subsection that describes it, shows
  its code and names the file; a row in `examples/README.md`; and its own
  `tests/examples/NN-name.rkt`, which checks what `main` prints.  A new
  example takes the next number.
- Errors a caller can meet are `exn:fail:web3` or a subtype, or
  libsecp256k1's `exn:fail:libsecp256k1:load`, which passes through
  unchanged.  No error shows a secret key or the text it was read from.
  Bytes that come from a contract or a node are untrusted: decoders
  validate offsets and lengths and raise `exn:fail:web3`, never an index
  error.
- Macros use `syntax-parse` (`define-syntax-parse-rule`), never
  `define-syntax-rule`.  Conversions are named `a->b`.  Constructors are
  `(type-name ...)`, not `(make-type-name ...)`.
- `master` must always build: the package catalog follows it.  Bump
  `version` in `info.rkt` on API changes.
- Each review finding that is fixed gets a test that fails without the fix.
- Names that `define-contract` generates follow the table in the manual's
  Binding Contracts section.  Changing `solidity-name->racket-name` is a
  breaking change.
- The HTTP provider takes over the connection only from a dead holder; a
  suspended holder blocks the provider's other callers until it resumes.
