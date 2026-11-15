// SPDX-License-Identifier: Apache-2.0 OR MIT
pragma solidity ^0.8.24;

// A small token for web3's examples and tests.  Not a complete ERC-20.
contract Token {
    string public name;
    string public symbol;
    uint8 public constant decimals = 18;
    uint256 public totalSupply;
    address public owner;
    mapping(address => uint256) public balanceOf;
    mapping(address => uint256) public deposits;

    event Transfer(address indexed from, address indexed to, uint256 value);
    event Deposit(address indexed from, uint256 value, string memo);

    error InsufficientBalance(uint256 have, uint256 want);
    error Unauthorized(address caller);

    constructor(string memory name_, string memory symbol_, uint256 supply) {
        require(bytes(name_).length > 0, "empty name");
        name = name_;
        symbol = symbol_;
        owner = msg.sender;
        totalSupply = supply;
        balanceOf[msg.sender] = supply;
        emit Transfer(address(0), msg.sender, supply);
    }

    function transfer(address to, uint256 value) external returns (bool) {
        uint256 have = balanceOf[msg.sender];
        if (have < value) revert InsufficientBalance(have, value);
        balanceOf[msg.sender] = have - value;
        balanceOf[to] += value;
        emit Transfer(msg.sender, to, value);
        return true;
    }

    function mint(address to, uint256 value) external {
        if (msg.sender != owner) revert Unauthorized(msg.sender);
        totalSupply += value;
        balanceOf[to] += value;
        emit Transfer(address(0), to, value);
    }

    function burn(uint256 value) external {
        balanceOf[msg.sender] -= value;
        totalSupply -= value;
        emit Transfer(msg.sender, address(0), value);
    }

    function deposit(string calldata memo) external payable {
        require(msg.value > 0, "no value");
        deposits[msg.sender] += msg.value;
        emit Deposit(msg.sender, msg.value, memo);
    }

    function info() external view returns (string memory, string memory, uint8, uint256) {
        return (name, symbol, decimals, totalSupply);
    }

    function balancesOf(address[] calldata accounts) external view returns (uint256[] memory) {
        uint256[] memory result = new uint256[](accounts.length);
        for (uint256 i = 0; i < accounts.length; i++) {
            result[i] = balanceOf[accounts[i]];
        }
        return result;
    }
}
