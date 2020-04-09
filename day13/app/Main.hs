module Main where

import Lib
import Data.List.Split (splitOn)
import Control.Monad.State.Lazy (runStateT)
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
  -- PART 1: How many Block tiles do we make?
  contents <- fmap lines . readFile $ "input.txt"
  let code = fmap read . head . fmap (splitOn ",") $ contents :: [Int]

  let x = newProg code []
  let x' = runProg x
  let commands = buildCommands . reverse . output $ x'

  print ("Number of blocks to kill: " ++ (show . length . filter (\i -> snd i == Block) $ commands))

  -- PART 2: Run the game til all the blocks are gone
  let code2 = 2:(tail code)
  (myScore, finalGame) <- runStateT playGame (newGame code2)

  print "Final Program State"
  print . progState . gameProg $ finalGame

  let grid2 = gridDisplay $ gameDisplay finalGame
  _ <- mapM print (grid2)
  print "your score:"
  print myScore

