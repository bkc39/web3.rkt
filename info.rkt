#lang info
(define collection "web3")
(define deps '("base" "casemate" "keccak"))
(define build-deps '("rackunit-lib"))
(define pkg-desc "Ethereum JSON-RPC client for Racket, with ABI helpers and anvil support")
(define version "0.1")
(define pkg-authors '(bkc))
(define license '(Apache-2.0 OR MIT))
;; Nix build links and direnv state live next to the sources.
(define compile-omit-paths '("result" ".direnv"))
(define test-omit-paths '("result" ".direnv"))
