web3
====

An Ethereum JSON-RPC client for Racket, with ABI helpers and first-class
support for local [anvil](https://book.getfoundry.sh/anvil/) nodes.

```racket
(require web3)

;; a throwaway local chain, as the current provider
(with-anvil ()
  (define me (car (eth-accounts)))
  (eth-get-balance me "latest")                     ; => 10000000000000000000000
  (anvil-set-code "0x...c0de" "0x602a60005260206000f3")
  (eth-call (hasheq 'to "0x...c0de" 'data "0x") "latest"))

;; any node
(with-provider (url->http-provider "https://eth.llamarpc.com")
  (eth-block-number))
```

## Modules

`(require web3)` provides everything below.

| module | contents |
|---|---|
| `web3/rpc` | `url->http-provider`, `current-provider`, `with-provider`, `rpc-call`, `define-jsonrpc-endpoint`, `exn:fail:web3`, `exn:fail:web3:rpc` (with `code`, `rpc-message`, `data`), hex helpers |
| `web3/eth` | `web3_*`, `net_*` and `eth_*` endpoints (`eth-call`, `eth-send-transaction`, `eth-get-transaction-receipt`, `eth-get-logs`, ...), `wait-for-receipt`, `send-transaction/receipt`, `receipt-succeeded?` |
| `web3/abi` | `function-selector`, `encode-call`, `word->bytes`, `bytes->words`, `decode-revert-reason` (Error(string), Panic(uint256)) |
| `web3/anvil` | `anvil_*`, `evm_snapshot`/`evm_revert`, `debug_traceCall`/`debug_traceTransaction`; `start-anvil`, `stop-anvil`, `with-anvil` |

Endpoints are named after their RPC method in kebab-case
(`eth_getBalance` → `eth-get-balance`), take an optional `#:provider`,
return the call's result (quantities as exact integers), and raise
`exn:fail:web3:rpc` on error responses; revert payloads are in
`exn:fail:web3:rpc-data`.  New endpoints are one line each:

```racket
(define-jsonrpc-endpoint eth_getBalance
  (-> string? [block-param/c block-param->string] exact-nonnegative-integer?)
  #:result-transform hex-string->number)
```

Keccak-256 comes from [keccak-rkt](https://github.com/bkc39/keccak-rkt).

## Documentation

The manual is in `scribblings/web3.scrbl`; `raco setup web3` renders it
locally, and `raco docs web3` opens it.

## Development

The flake pins Racket, foundry (anvil) and keccak-rkt; the build runs the
test suite, including integration tests against a fresh anvil.

```
nix develop                  # shell with pinned tools; provisions web3 on first entry
raco test -x -p web3         # unit + anvil integration tests
nix build                    # hermetic build + tests
```

Outside Nix the anvil tests use `$WEB3_ANVIL` or `anvil` on `PATH`, and are
skipped if neither exists.

## License

Licensed under either of [Apache License, Version 2.0](LICENSE-APACHE) or
[MIT license](LICENSE-MIT), at your option.
