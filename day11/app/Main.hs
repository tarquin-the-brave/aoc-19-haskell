module Main where

import Lib
import Data.List.Split (splitOn)
import Control.Monad.State.Lazy (execState)
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
  -- PART 1: How many squares do we paint?
  contents <- fmap lines . readFile $ "input.txt"
  let code = fmap read . head . fmap (splitOn ",") $ contents :: [Int]

  let initRobot = newRobot code
  let finalRobot = execState (runRobot Black) initRobot
  print . HM.size . panels $ finalRobot

  -- Part 2: Start with a White panel - draw the output on screen
  let initRobot2 = newRobot code
  let finalRobot2 = execState (runRobot White) initRobot2
  print . HM.size . panels $ finalRobot2
  mapM print (reverse . displayRobot $ finalRobot2)
  print "done"

