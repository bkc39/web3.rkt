#lang racket/base

(require json
         racket/runtime-path)

(provide artifact
         artifact-bytecode
         artifact-deployed-bytecode
         artifact-path
         contracts-dir)

(define-runtime-path contracts-dir "contracts")

(define (artifact-path name)
  (build-path contracts-dir "out" (format "~a.sol" name) (format "~a.json" name)))

(define (artifact name)
  (call-with-input-file (artifact-path name) read-json))

(define (artifact-bytecode name)
  (hash-ref (hash-ref (artifact name) 'bytecode) 'object))

(define (artifact-deployed-bytecode name)
  (hash-ref (hash-ref (artifact name) 'deployedBytecode) 'object))
