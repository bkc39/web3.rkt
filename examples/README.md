# Examples

Numbered programs, one flow each; the manual's Guide tells each one.
`racket examples/NN-name.rkt` runs its `main`, which prints a few labelled
lines.

```
racket examples/00-provider.rkt
```

| program | shows |
|---|---|
| `00-provider.rkt` | endpoints, a raw `rpc-call`, defining an endpoint, a node's error code |
| `01-chain-state.rkt` | balances and nonces, sending one ether, waiting for the receipt |
| `02-anvil.rkt` | installing code, funding and impersonating an address, a snapshot reverted |
| `03-custom-providers.rkt` | a `gen:provider` struct that logs, wrapping a `mock-provider` that answers and refuses |
| `04-abi-encode.rkt` | canonical signatures, selectors and the words of call data |
| `05-abi-decode.rkt` | a call's result decoded, and a revert's custom error decoded |
| `06-deploy-and-call.rkt` | deploying a token, a call, a send, a send refused with its custom error |
| `07-events.rkt` | a receipt's transfer decoded, `get-logs` for every transfer and filtered by recipient |
| `08-contract-bindings.rkt` | `define-contract` from an artifact: deploy, calls, sends, an error and events by name |
| `09-accounts.rkt` | a random key, its address, signing a digest, recovering the signer |
| `10-rlp.rkt` | encoding and decoding RLP, scalars, the RLP behind CREATE addresses, refusing non-canonical input |
| `11-units.rkt` | wei, gwei and ether amounts from decimal strings and back, a token's decimals, refused amounts |
| `12-transactions.rkt` | legacy, EIP-2930 and EIP-1559 transactions signed offline, their raw bytes, hashes and recovered senders, a refused truncation |
| `13-signers.rkt` | a local key and a node account as signers: each one's EIP-191 signature verifies, and both sign the same transaction |
| `14-sending.rkt` | a key the node does not hold: a contract deploy and send, an ether transfer |

03, 04 and 09 to 12 need no node; the others start their own local
[anvil](https://book.getfoundry.sh/anvil/), so nothing here touches a real
chain.  anvil must be on `PATH` or named by `WEB3_ANVIL`; `nix develop`
provides it.

`contracts/` holds the Solidity the examples deploy (`Token.sol`,
`Counter.sol`, `Events.sol`); `contracts/out/` is its compiled form,
committed.  After editing a `.sol`, run `racket tests/generate-contracts.rkt`
in `nix develop`.

`tests/examples/NN-name.rkt` runs each program and checks what it prints.
