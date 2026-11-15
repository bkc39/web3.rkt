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
| `abi.rkt` | the contract ABI: encoders, selectors, revert reasons |
| `abi/type.rkt` | ABI types, signatures and value checks; phase-1 safe, so it requires only `racket/base` libraries and `private/exn.rkt` |
| `abi/vectors.rkt` | conformance vectors, data in `abi/vectors/encode.rktd` (regenerate with `racket tests/generate-abi-vectors.rkt`) |
| `private/exn.rkt` | the exception structs (`exn:fail:web3`, `…:rpc`, `…:abi`, `…:abi:type`, `…:abi:value`); racket/base only |
| `anvil.rkt` | anvil's methods and process management |
| `examples/` | numbered scripts (`NN-name.rkt`) for reading and for the REPL |

## Commands

Work inside the dev shell; it pins Racket and anvil and links this checkout.

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
- Every user-visible feature has an example.  A new example takes the next
  number, gets a row in `examples/README.md`, and is run by
  `tests/examples.rkt`.
- Errors a caller can meet are `exn:fail:web3` or a subtype.  Bytes that
  come from a contract or a node are untrusted: decoders validate offsets
  and lengths and raise `exn:fail:web3`, never an index error.
- Macros use `syntax-parse` (`define-syntax-parse-rule`), never
  `define-syntax-rule`.  Conversions are named `a->b`.  Constructors are
  `(type-name ...)`, not `(make-type-name ...)`.
- `master` must always build: the package catalog follows it.  Bump
  `version` in `info.rkt` on API changes.
- Each review finding that is fixed gets a test that fails without the fix.
- The HTTP provider takes over the connection only from a dead holder; a
  suspended holder blocks the provider's other callers until it resumes.
