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

`(require web3)` provides everything below except `web3/abi/vectors`.

| module | contents |
|---|---|
| `web3/rpc` | `gen:provider` (`provider-request`, `provider-close`), `url->http-provider`, `mock-provider` (`mock-provider-calls`) for tests without a node, `current-provider`, `with-provider`, `rpc-call`, `define-jsonrpc-endpoint`, `exn:fail:web3`, `exn:fail:web3:rpc` (with `code`, `rpc-message`, `data`), `raise-rpc-error`, hex helpers |
| `web3/eth` | `web3_*`, `net_*` and `eth_*` endpoints (`eth-call`, `eth-send-transaction`, `eth-get-transaction-receipt`, `eth-get-logs`, ...), `wait-for-receipt`, `send-transaction/receipt`, `receipt-succeeded?` |
| `web3/abi` | `abi-encode`, `abi-encode-packed`, `encode-call`, `function-selector`, `word->bytes`, `bytes->words`; `abi-decode`, `decode-result`, `decode-call`; `decode-revert`, `abi-revert->string`, `rpc-exn->revert` (Error(string), Panic(uint256), custom errors); everything in `web3/abi/type` and `web3/abi/event-type` |
| `web3/abi/type` | ABI types as structs (`abi-uint`, `abi-array`, `abi-tuple`, ...), canonical strings or s-expressions; `abi-value?`, `abi-signature`, `exn:fail:web3:abi` and its subtypes. No runtime dependencies, so macros can use it at phase 1 |
| `web3/abi/event-type` | events as structs (`abi-event`, `abi-event-parameter`), strings or s-expressions; `abi-event->signature`. Usable at phase 1 |
| `web3/abi/event` | `event-topic`, `event-topics`, `abi-value->topic`, `any-of`, `indexed-hash`; `decode-log`, `decode-logs` and `event-log` (`event-log-name`, `event-log-arguments`, `event-log-ref`) |
| `web3/eth/log` | logs as structs: `jsexpr->log`, `log->jsexpr`, `log-address`, `log-topics`, `log-data`, `log-block-number`, `log-index`, ... |
| `web3/event` | `log-filter`, `get-logs` (chunked, halving ranges a node refuses), `receipt-logs`; everything in `web3/abi/event` and `web3/eth/log` |
| `web3/abi/vectors` | `abi-encode-vectors`, `abi-decode-vectors`, `abi-reject-vectors`, conformance cases checked against Foundry's `cast` (not part of `(require web3)`) |
| `web3/address` | `address->checksum-string`, `checksum-address?` (EIP-55) |
| `web3/contract` | `deploy-contract`, `deploy-contract/receipt`, `contract-call`, `contract-send`, `current-account`, `create-address`, `create2-address`, `exn:fail:web3:transaction` |
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

## Examples

[`examples/`](examples/) has numbered scripts to read and to try at the
REPL, each against its own local anvil:

```
racket -i -e '(enter! "examples/00-provider.rkt")'
```

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
