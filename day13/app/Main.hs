module Main where

import Lib
import Data.List.Split (splitOn)
import Control.Monad.State.Lazy (runState)
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
  -- PART 1: How many Block tiles do we make?
  contents <- fmap lines . readFile $ "input.txt"
  let code = fmap read . head . fmap (splitOn ",") $ contents :: [Int]

  let x = newProg code []
  let x' = runProg x
  let commands = buildCommands . reverse . output $ x'

  print . length . filter (\i -> snd i == Block) $ commands

  let grid = gridDisplay . editDisplay HM.empty $ commands

  _ <- mapM print grid

  -- PART 2: Run the game til all the blocks are gone
  let code2 = 2:(tail code)
  let (myScore, finalGame) = runState playGame (newGame code2)

  print "Final Program State"
  print . progState . gameProg $ finalGame

  let grid2 = gridDisplay $ gameDisplay finalGame
  _ <- mapM print (grid2)
  print "your score:"
  print myScore

