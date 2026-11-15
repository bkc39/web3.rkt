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

anvil must be on `PATH` or named by `WEB3_ANVIL`; `nix develop` provides it.
`tests/examples.rkt` runs every script.
