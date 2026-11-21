# AGENTS.md

Guidance for agents working in this repository.

## What this is

`web3`: an Ethereum client library for Racket.  One package, one collection
(`web3`), one manual (`scribblings/web3.scrbl`).  Consumers are yul and
bezzle, which pin it as a flake input.

| module | holds |
|---|---|
| `rpc.rkt` | providers, `rpc-call`, errors, hex helpers, `define-jsonrpc-endpoint` |
| `eth.rkt` | `web3_*`, `net_*`, `eth_*` endpoints, receipts |
| `abi.rkt` | the contract ABI: encoders, decoders, selectors, reverts; an `internal` submodule of entry points that take the caller's `who` |
| `contract.rkt` | deploying, calls and sends, contract instances, define-contract, contract addresses; an `internal` submodule of entry points that take the caller's `who` |
| `private/contract-call.rkt` | the calls, sends, estimates and deploys `contract.rkt` and bound procedures run, under their caller's `who`; the sending itself is `private/send.rkt`'s |
| `send.rkt` | filling transactions from a node, signing them with a signer and sending them raw; every procedure under `opaque->` |
| `private/send.rkt` | filling, raw sends, and the estimate/send/wait/replay every send runs, from an address or a signer, under the caller's `who`; `define-fill-keywords`, the one table of fill keywords their formals and contracts come from |
| `private/contracts.rkt` | the argument contracts the send and contract entry points share: senders, `to`, errors, providers, receipts |
| `private/bindings.rkt` | `define-contract` and `contract-bindings-out`, and the procedures they bind |
| `private/define-contract.rkt` | define-contract's expansion-time half; reads JSON at phase 1, never keccak |
| `private/binding-rules.rkt` | the keywords a bound procedure accepts and the messages for what it refuses, shared by the expansion-time checks and the procedures |
| `private/abi-json.rkt` | reading JSON ABIs and artifacts from ports and files |
| `address.rkt` | EIP-55 checksums; requires only `racket/base` libraries, `keccak` and `abi/type.rkt`; a `private` submodule, which also requires `private/hex.rkt`, holds `digest->address`, the address in the last 20 bytes of a hash |
| `account.rkt` | secret keys (libsecp256k1's), addresses, `sign-digest`, EIP-191 messages, `recover-address`, signatures and their 65 bytes; every procedure under `opaque->`, the constructor too: `signature` is a match expander that, as an expression, is the checked constructor, so the struct cannot be extended or copied around it; no network, JSON or node modules; a `private` submodule holds `recoverable->signature`, `sign-digest/who` and `bytes->signature/who` |
| `signer.rkt` | `gen:signer`, local and node signers, and the checks on what any signer returns; every procedure under `opaque->`; an `internal` submodule of entry points that take the caller's `who` |
| `private/signer.rkt` | the `define-generics` form of `gen:signer`, its methods exported with a `generic-` prefix so `signer.rkt` can give the public names to the checked procedures; racket/base and racket/generic only |
| `rlp.rkt` | RLP: `rlp-encode`, strict `rlp-decode`, scalars; an `internal` submodule of entry points that take the caller's `who`; requires only `racket/base` libraries and `private/exn`, `hex`, `who` |
| `units.rkt` | amounts: decimal strings and exact numbers in wei, gwei, ether or a token's decimals, and back; requires only `racket/base` libraries and `private/exn.rkt` |
| `transaction.rkt` | legacy, EIP-2930 and EIP-1559 transactions: values, encodings, signing, strict parsing; `transaction->jsexpr`, built without JSON modules; an `internal` submodule holds the parser under the caller's `who`, its reasons, the wire field table, `transaction/c` and `transaction-difference`, none of them public names; no network or JSON modules |
| `private/opaque.rkt` | `opaque/c` and `opaque->`, contracts whose errors name the kind of value, never the value; copied from libsecp256k1, which does not export them, with `web3-kinds`, the kinds only web3 has, and required keyword arguments added. Also the contracts web3's modules share: `secret-key/c` (`account.rkt`, `transaction.rkt`, `signer.rkt`), and the digest, message and address contracts (`account.rkt`, `transaction.rkt`, `signer.rkt`) |
| `private/signature.rkt` | the unchecked `signature` struct, its printer and its 65 bytes, shared by `account.rkt`, which exports it under contracts, and `private/opaque.rkt` |
| `abi/type.rkt` | ABI types, signatures and value checks; phase-1 safe, so it requires only `racket/base` libraries and `private/exn.rkt`; a `private` submodule holds the reader and contracts other modules share |
| `abi/event-type.rkt` | event descriptions (`abi-event`, string and s-expression forms); phase-1 safe like `abi/type.rkt`, plus `private/who.rkt`; a `private` submodule holds the readers under their caller's `who` |
| `abi/description.rkt` | ABI items and contract descriptions from strings, s-expressions and JSON; phase-1 safe, as abi/type.rkt |
| `abi/event.rkt` | event topics, `event-topics`, decoding logs into `event-log`s; an `internal` submodule of entry points that take the caller's `who` |
| `eth/log.rkt` | the `log` struct, read from and printed like the node's JSON |
| `event.rkt` | `log-filter`, `get-logs` (chunks, halving on a refused range), `receipt-logs` |
| `abi/vectors.rkt` | conformance vectors, data in `abi/vectors/encode.rktd` (regenerate with `racket tests/generate-abi-vectors.rkt`) and the hand-written `abi/vectors/decode.rktd` |
| `private/abi-decode.rkt` | the decoder core and its reasons; `abi.rkt` puts contracts on it |
| `private/abi-item.rkt` | ABI item structs and their string and s-expression readers under the caller's `who`; phase-1 safe; `abi/description.rkt` puts contracts on it |
| `private/hex.rkt` | unchecked conversions between bytes, hex strings and naturals; callers check their input |
| `private/exn.rkt` | the exception structs (`exn:fail:web3`, `…:rpc`, `…:rpc:revert`, `…:transaction`, `…:fill`, `…:abi`, `…:abi:type`, `…:abi:value`, `…:abi:decode`, `…:key`, `…:rlp`, `…:raw-transaction`, `…:signature`, `…:signer`, `…:units`); racket/base only |
| `private/node.rkt` | checked conversions of node data (hex, quantities, addresses, hashes, receipt status, optional fields); no JSON or transport modules |
| `private/who.rkt` | `define/who`, which defines `name` and `name/who`, the same procedure taking the `who` its messages start with |
| `anvil.rkt` | anvil's methods and process management |
| `examples/` | numbered scripts (`NN-name.rkt`) for reading and for the REPL |

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
- Every user-visible feature has an example.  A new example takes the next
  number, gets a row in `examples/README.md`, and is run by
  `tests/examples.rkt`, or, for a program with a `main` submodule, by its
  own `tests/examples/NN-name.rkt`.
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
