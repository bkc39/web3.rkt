#lang racket/base

(module+ test
  (require rackunit
           (only-in "../helpers/examples.rkt" example-lines))

  (define expected
    '("nonce 0"
      "  rlp:            0xd694f39fd6e51aad88f6f4ce6ab8827279cfffb9226680"
      "  decoded:        (\"0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266\" \"0x\")"
      "  keccak address: 0x5FbDB2315678afecb367f032d93F642f64180aa3"
      "  create-address: 0x5FbDB2315678afecb367f032d93F642f64180aa3"
      "nonce 1"
      "  rlp:            0xd694f39fd6e51aad88f6f4ce6ab8827279cfffb9226601"
      "  decoded:        (\"0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266\" \"0x01\")"
      "  keccak address: 0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512"
      "  create-address: 0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512"
      "nonce 128"
      "  rlp:            0xd794f39fd6e51aad88f6f4ce6ab8827279cfffb922668180"
      "  decoded:        (\"0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266\" \"0x80\")"
      "  keccak address: 0xB82008565FdC7e44609fA118A4a681E92581e680"
      "  create-address: 0xB82008565FdC7e44609fA118A4a681E92581e680"
      "0xc88363617483646f67: (\"0x636174\" \"0x646f67\")"
      "0x8105: single-byte at byte 0"
      "0xb800: leading-zero at byte 0"
      "0xc0c0: trailing at byte 1"
      "0xc2: short at byte 0"))

  (test-case "X1: main prints the CREATE preimages and addresses, and what each input decodes to"
    (check-equal? (example-lines "10-rlp") expected)))
