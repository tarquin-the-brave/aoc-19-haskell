# day9

Need to extend intcode:
* relative mode, mode `2`:
  + like position mode but the position is added to the relative base.
  + relative base starts at 0
* op code `9`: add value of only parameter to relative base.
* positive addresses beyond the initial program.

First of all, go write some tests for the current functionality, then extend.
