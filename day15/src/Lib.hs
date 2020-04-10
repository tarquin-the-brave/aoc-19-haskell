module Lib
    ( someFunc
    ) where

import qualified IntcodeImport as IC
  ( new
  , run
  , Prog (..)
  , scrubOutput
  , setInput
  )

import qualified Display as D
  ( DisplayOf
  , Coord
  , edit
  , grid
  , getDefault
  )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Object = Wall | NotWall | Empty deriving(Show)
type Displ = D.DisplayOf Object

objToChar :: Object -> Char
objToChar Wall = '#'
objToChar NotWall = '.'
objToChar Empty = ' '

data Move = North | East | South | West deriving(Show)

moveFromInt :: MonadFail m => Int -> m Move
moveFromInt 1 = return North
moveFromInt 2 = return South
moveFromInt 3 = return West
moveFromInt 4 = return East
moveFromInt _ = fail "Not a direction"

moveToInt :: Move -> Int
moveToInt North = 1
moveToInt South = 2
moveToInt West = 3
moveToInt East = 4

-- State that matters:
-- - The coordinates visited - our Displ
-- - The intcode state, the droid's brain - IC.Prog
-- - Current D.Coord
-- - Previous Move
data Droid = Droid {
  brain::IC.Prog,
  location::D.Coord,
  lastMove::Move,
  display::Displ
}

stepDroid :: Move -> Droid -> Droid
stepDroid = undefined
