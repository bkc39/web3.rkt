# Examples

Numbered scripts, meant to be read top to bottom and then poked at.  Each
starts its own local [anvil](https://book.getfoundry.sh/anvil/) node, so
nothing here touches a real chain.

```
racket examples/00-provider.rkt                         # run it
racket -i -e '(enter! "examples/00-provider.rkt")'      # run it, then stay in its REPL
```

In the REPL every name the script defines (`node`, `provider`, `alice`,
`receipt`, ...) is bound, and `current-provider` points at the script's
node.  `(show expr)` prints an expression and its value.

| script | shows |
|---|---|
| `00-provider.rkt` | providers, `current-provider`, raw `rpc-call`, errors, defining an endpoint, hex helpers |
| `01-chain-state.rkt` | accounts, balances, blocks, sending ether, receipts, code and storage |
| `02-anvil.rkt` | installing code and storage, reverts, snapshots, impersonation, mining, tracing |
| `03-custom-providers.rkt` | `mock-provider` from a table and from a procedure, `raise-rpc-error`, a `gen:provider` struct that logs a real node's calls, `provider-close` |
| `04-abi-encode.rkt` | types in three forms, canonical strings, selectors, standard and packed encoding, typed call data, ABI errors |
| `05-abi-decode.rkt` | decoding values, results and call data, EIP-55 addresses, reverts with Error, Panic and custom errors from a contract, malformed data |
| `06-deploy-and-call.rkt` | deploying from a compiled artifact at its predicted address, calls with one and several outputs, sends and receipts, reverts with custom errors, `Error` and `Panic`, a refused send that sends nothing, transactions mined with status 0, payable calls, senders, CREATE and CREATE2 addresses |
| `07-events.rkt` | event descriptions in three forms and their topics, a receipt's log raw and decoded, arguments by name, `get-logs` for every transfer, filtered by an indexed address, by any of several, and from the zero address, block ranges and chunked fetching, hashed indexed values, anonymous events, events sharing topic 0, several events at once, malformed logs, events and filters |
| `08-contract-bindings.rkt` | define-contract from a compiled artifact and from inline items, deploying, calls and sends through bound procedures, custom errors decoded by name, events by name, contract instances from JSON at run time, ABI descriptions as plain data, call data without sending, overloads, mistakes caught at compile time |

`contracts/` holds the Solidity the examples deploy (`Token.sol`,
`Counter.sol`, `Events.sol`); `contracts/out/` is its compiled form,
committed.  After editing a `.sol`, run `racket tests/generate-contracts.rkt`
in `nix develop`.

anvil must be on `PATH` or named by `WEB3_ANVIL`; `nix develop` provides it.
`tests/examples.rkt` runs every script.
