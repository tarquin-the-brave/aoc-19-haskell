# intcode

Safe computation of an intcode program.

On failure program is returned in the state it failed in:
* The intcode
* The instruction pointer
* inputs and outputs
* `progState` of `TerminatedBadly`

This is good for debugging when an instruction is faulty.
It's not so good for when a previous instruction successfully
created the faulty instruction.
