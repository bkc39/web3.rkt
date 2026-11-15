// SPDX-License-Identifier: Apache-2.0 OR MIT
pragma solidity ^0.8.24;

// Two libraries declare the same error and event under different
// parameter names; solc lists each of them twice in Mover's ABI.
library Left {
    error Unauthorized(address who);
    event Moved(uint256 x);
}

library Right {
    error Unauthorized(address caller);
    event Moved(uint256 y);
}

contract Mover {
    address public owner = msg.sender;

    function moveLeft(uint256 x) external {
        if (msg.sender != owner) revert Left.Unauthorized(msg.sender);
        emit Left.Moved(x);
    }

    function moveRight(uint256 x) external {
        if (msg.sender != owner) revert Right.Unauthorized(msg.sender);
        emit Right.Moved(x);
    }
}
