module Lib
    ( Droid (..)
    , newDroid
    , runDroid
    , runDroid'
    ) where

import qualified IntcodeImport as IC
  ( new
  , run
  , ProgState (..)
  , Prog (..)
  , scrubOutput
  , setInput
  , status
  )

import qualified Display as D
import qualified Control.Monad.State.Lazy as S
import qualified Data.HashMap.Strict as HM
import qualified System.Console.ANSI as C
import Control.Concurrent.Thread.Delay
import System.IO.Unsafe
import System.Random

-- sum types to make our problem domain well typed.
-- Boring, but the right thing to do.
data Object = Wall | NotWall | Empty | Oxygen | OurDroid | Dunno deriving(Show)

objToChar :: Object -> Char
objToChar Wall = '#'
objToChar NotWall = '.'
objToChar Empty = ' '
objToChar Oxygen = 'O'
objToChar OurDroid = 'D'
objToChar Dunno = '?'

objFromInt :: Int -> Object
objFromInt 0 = Wall
objFromInt 1 = NotWall
objFromInt 2 = Oxygen
objFromInt _ = Dunno

type Move = D.Direction

moveToInt :: Move -> Int
moveToInt D.U = 1
moveToInt D.D = 2
moveToInt D.L = 3
moveToInt D.R = 4

moveFromInt :: Int -> Move
moveFromInt 1 = D.U
moveFromInt 2 = D.D
moveFromInt 3 = D.L
moveFromInt 4 = D.R
moveFromInt _ = D.U

-- State that matters:
-- - The coordinates visited - our Displ
-- - The intcode state, the droid's brain - IC.Prog
-- - Current D.Coord
-- - Previous Move
type Displ = D.DisplayOf Object

data Droid = Droid {
  brain::IC.Prog,
  location::D.Coord,
  lastMove::Move,
  display::Displ
}

newDroid :: [Int] -> Droid
newDroid intCode = Droid {
  brain = IC.new intCode [],
  location = (0,0),
  lastMove = D.U,
  display = HM.empty
}

showDispl :: Droid -> Displ
showDispl dr = HM.insert (location dr) OurDroid (display dr)

-- If we've already visited somewhere, don't rerun the intcode program
stepDroid' :: Move -> Droid -> (Object, Droid)
stepDroid' move droid = case HM.lookup newLocation (display droid) of
  Just obj -> case obj of
    Wall -> (Wall, droid)
    otherObj -> (otherObj, Droid {
      brain = brain droid,
      location = newLocation,
      lastMove = move,
      display = display droid
    })
  Nothing -> stepDroid'' move droid
  where
    newLocation = D.coordMove move (location droid)

stepDroid'' :: Move -> Droid -> (Object, Droid)
stepDroid'' move droid =
  (
    objFound,
    Droid {
      brain = newBrain,
      location = newLocation,
      lastMove = newLastMove,
      display = newDisplay
    }
  )
  where
    newProg = IC.run . IC.setInput [moveToInt move] $ brain droid
    objFound = objFromInt . head . IC.output $ newProg
    objFoundAt = D.coordMove move $ location droid
    newBrain = IC.scrubOutput newProg
    (newLastMove, newLocation) = case objFound of
      Wall -> (\d -> (lastMove d, location d)) droid
      _ -> (move, objFoundAt)
    newDisplay = D.edit (display droid) [(objFoundAt, objFound)]

-- State transformer for Droid and IO
type DroidIOT = S.StateT Droid IO

stepDroid :: Move -> DroidIOT Object
-- stepDroid = S.state . stepDroid'
stepDroid = S.state . stepDroid''

runDroid :: DroidIOT (D.Coord, Int)
runDroid = runDroid' D.U

runDroid' :: Move -> DroidIOT (D.Coord, Int)
runDroid' move = do
  obj <- stepDroid move
  droid <- S.get
  let grid = reverse $ gridDispl droid
  _ <- S.liftIO . mapM print $ grid
  S.liftIO . C.cursorUp $ length grid
  -- S.liftIO $ delay 500000
  case obj of
    Oxygen -> return $ (location droid, length grid)
    _ -> case IC.status . brain $ droid of
      IC.AwaitInput -> runDroid' $ nextMove droid
      _ -> return $ (location droid, length grid)

gridDispl :: Droid -> [[Char]]
gridDispl droid = D.grid (D.getDefault Empty) objToChar (showDispl droid)

nextMove :: Droid -> Move
nextMove dr = case (objInfront dr, objLeft dr, objRight dr) of
  -- Explore the unknown: precedence: forward, left, right
  (Empty, _, _) -> lastMove dr
  (_, Empty, _) -> D.turnLeft $ lastMove dr
  (_, _, Empty) -> D.turnRight $ lastMove dr
  -- Encased by walls: only go back if you're otehrwise surrounded
  (Wall, Wall, Wall) -> D.goBack $ lastMove dr
  -- -- Walls on two sides: prefer L/R to back
  (Wall, Wall, _) -> D.turnRight $ lastMove dr
  (Wall, _, Wall) -> D.turnLeft $ lastMove dr
  (_, Wall, Wall) -> lastMove dr
  -- -- Wall infront only: go left
  (Wall, _, _) -> if randomBool then D.turnLeft $ lastMove dr else D.turnRight $ lastMove dr
  -- Pick something random
  (_, _, _) -> randomMove
  where
    objInfront d = D.getDefault Empty (infrontOf d) (display d)
    objLeft d = D.getDefault Empty (leftOf d) (display d)
    objRight d = D.getDefault Empty (rightOf d) (display d)
    infrontOf d = inDirectionOf (lastMove d) d
    leftOf d = inDirectionOf (D.turnLeft $ lastMove d) d
    rightOf d = inDirectionOf (D.turnRight $ lastMove d) d
    inDirectionOf dir d = D.coordMove dir (location d)
    randomMove = moveFromInt $ unsafePerformIO (getStdRandom (randomR (1, 4)))
    randomInt = (unsafePerformIO (getStdRandom (randomR (0, 1)))) :: Int
    randomBool = randomInt == 0

