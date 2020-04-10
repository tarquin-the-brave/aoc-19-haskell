module Lib
    ( Robot (..)
    , Colour (..)
    , runRobot
    , newRobot
    , displayRobot
) where

import qualified Intcode as IC
  ( newProg
  , runProg
  , Prog (..)
  , ProgState (..)
  , setInputProg
  , scrubOutput)
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Lazy

-- what state matters?
-- - intcode program state - Intcode.Prog
-- - current panel & direction pointing in
-- - panels coloured in - coordinates and color

data Heading = U | D | L | R deriving(Show)
data Colour = Black | White deriving(Show)
type Panel = (Int, Int)
data Move = MoveL | MoveR deriving(Show)

data Robot = Robot {
  brain::IC.Prog,
  currentPanel::Panel,
  heading::Heading,
  panels::HM.HashMap Panel Colour
} deriving(Show)

newRobot :: [Int] -> Robot
newRobot intCode = Robot {
  brain = IC.newProg intCode [],
  currentPanel = (0,0),
  heading = U,
  panels = HM.empty
}

runRobot :: Colour -> State Robot ()
runRobot currentColour = do
  colour <- stepRobot' currentColour
  brain' <- get
  case status brain' of
    IC.AwaitInput -> runRobot colour
    _ -> return ()

stepRobot' :: Colour -> State Robot Colour
stepRobot' = state . stepRobot

stepRobot :: Colour -> Robot -> (Colour, Robot)
stepRobot currentColour robot = (getColour newCurPanel newPanelColMap, Robot {
  brain = IC.scrubOutput ranBrain,
  currentPanel = newCurPanel,
  heading = newHeading,
  panels = newPanelColMap
})
  where
    ranBrain = IC.runProg . IC.setInputProg [colourToInt currentColour] $ brain robot
    colour = colourFromInt $ (IC.output ranBrain) !! 1
    move = moveFromInt $ (IC.output ranBrain) !! 0
    newHeading = getTranslation move (heading robot)
    newCurPanel = movePanel newHeading (currentPanel robot)
    newPanelColMap = HM.insert (currentPanel robot) colour (panels robot)

status :: Robot -> IC.ProgState
status = IC.progState . brain

getColour :: Panel -> HM.HashMap Panel Colour -> Colour
getColour panel panels' = case HM.lookup panel panels' of
   Nothing -> Black
   Just c -> c

movePanel :: Heading -> Panel -> Panel
movePanel U (x,y) = (x,y+1)
movePanel D (x,y) = (x,y-1)
movePanel L (x,y) = (x-1,y)
movePanel R (x,y) = (x+1,y)

getTranslation :: Move -> Heading -> Heading
getTranslation MoveL U = L
getTranslation MoveL L = D
getTranslation MoveL D = R
getTranslation MoveL R = U
getTranslation MoveR U = R
getTranslation MoveR L = U
getTranslation MoveR D = L
getTranslation MoveR R = D

moveFromInt :: Int -> Move
moveFromInt 0 = MoveL
moveFromInt 1 = MoveR

colourFromInt :: Int -> Colour
colourFromInt 0 = Black
colourFromInt 1 = White

colourToInt :: Colour -> Int
colourToInt Black = 0
colourToInt White = 1

colourToChar :: Colour -> Char
colourToChar Black = '.'
colourToChar White = '#'

displayRobot :: Robot -> [[Char]]
-- displayRobot robot =
  -- [[colourToChar $ getColour (x,y) (panels robot) | x <- [minX..maxX]] | y <- [minY..maxY]]
  -- where
  --   (minX, maxX) = (\k -> (minimum k, maximum k)) . fmap fst . HM.keys $ panels robot
  --   (minY, maxY) = (\k -> (minimum k, maximum k)) . fmap snd . HM.keys $ panels robot
displayRobot robot = do
  y <- [minY..maxY]
  return $ do
    x <- [minX..maxX]
    return . colourToChar $ getColour (x,y) (panels robot)
  where
    (minX, maxX) = (\k -> (minimum k, maximum k)) . fmap fst . HM.keys $ panels robot
    (minY, maxY) = (\k -> (minimum k, maximum k)) . fmap snd . HM.keys $ panels robot


