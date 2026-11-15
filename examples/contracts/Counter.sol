// SPDX-License-Identifier: Apache-2.0 OR MIT
pragma solidity ^0.8.24;

// The smallest contract with state: no constructor arguments.
contract Counter {
    uint256 public count;

    function increment() external returns (uint256) {
        count += 1;
        return count;
    }
}
