# day2

Tricky one.  Had to come back to this after doing days 3, 4, & 6.

The problem description basically gives you an algorithm for doing this by
mutating an array.

I spent a while getting swamped in documentation on State and ST Monads and
got very confuse.  Annoyingly I wasn't confused about the concepts, I just
couldn't find good examples on how to expresss the damn things.  And there
seemed a million ways to do the same (but not the same) things...

Getting a bit frustrated I decided to try to implement a solution using only
the tools I know, and then later I might find that I've basically
reimplemented so part of a Monad and swap that in.

Supprizingly, I found recursion was the answer... again... I was hoping
exercise would teach me about mutation and stateful computation, but turns
out not.

## Improvements to make

### Usage of `List`

From familiarity I used `List` to store the intcode program.

Lists are good for iterating over but inefficient for looking up an index.
I need to use something else: `Vector`, `Sequence`, ...

### Error handling

I've used `!!` to lookup values in a list at an index. A faulty intcode
might reference an index out of range. If the index is out of range the
program crashes without giving much help as to where the error came from.
