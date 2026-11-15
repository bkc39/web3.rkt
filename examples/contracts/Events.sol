// SPDX-License-Identifier: Apache-2.0 OR MIT
pragma solidity ^0.8.24;

// Events with every kind of indexed parameter, for web3's log tests.
contract Events {
    struct Pair {
        uint256 id;
        string label;
    }

    event Tagged(string indexed tag, bytes indexed blob, address indexed sender, string text);
    event Lists(uint256[] indexed ids, string[] indexed names, uint8[2] indexed pair);
    event Nested(uint256[][] indexed grid, Pair[] indexed pairs);
    event Paired(Pair indexed pair, Pair plain);
    event Flags(bool indexed flag, int8 indexed delta, bytes3 indexed code);
    event Transfer(address indexed from, address indexed to, uint256 indexed tokenId);
    event Note(address indexed sender, uint256 value) anonymous;
    event Four(uint256 indexed a, uint256 indexed b, uint256 indexed c, uint256 indexed d) anonymous;
    event Bare(uint256, string);
    event Counted(uint256 indexed i);

    function tag(string calldata tag_, bytes calldata blob, string calldata text) external {
        emit Tagged(tag_, blob, msg.sender, text);
    }

    function lists(uint256[] calldata ids, string[] calldata names, uint8[2] calldata pair) external {
        emit Lists(ids, names, pair);
    }

    function nested(uint256[][] calldata grid, Pair[] calldata pairs) external {
        emit Nested(grid, pairs);
    }

    function paired(uint256 id, string calldata label) external {
        Pair memory p = Pair(id, label);
        emit Paired(p, p);
    }

    function flags(bool flag, int8 delta, bytes3 code) external {
        emit Flags(flag, delta, code);
    }

    function transferNft(address to, uint256 tokenId) external {
        emit Transfer(msg.sender, to, tokenId);
    }

    function note(uint256 value) external {
        emit Note(msg.sender, value);
        emit Four(1, 2, 3, value);
    }

    function bare(uint256 x, string calldata s) external {
        emit Bare(x, s);
    }

    function count(uint256 n) external {
        for (uint256 i = 0; i < n; i++) {
            emit Counted(i);
        }
    }
}
