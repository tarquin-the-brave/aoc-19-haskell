# day13

* Really starting to repeat this pattern of using the State monad:
  + What data matters?
  + Stick it in a type
  + define a function `evolveData' :: s -> (a,s)`
  + pop it in a State monad
  ```haskell
  evolveData :: State s a
  evolveData = state evolveData'
  ```

* despite my intended approach of "don't worry about perf, I'll come back to that"
  I did have to go back and fixup some things.
  + don't concat to the end of lists
  + scrub the intcode program output between runs so the whole output
    isn't recalculated into the display each run.
* program still took about 2 mins after that, so plenty to come back and improve.

* I would have liked to print out the display on every step
  so I could watch the game.  I need to work out how to put
  in IO monad inside a state monad for that I think.

