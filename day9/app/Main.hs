module Main where

import Lib
import Data.List.Split (splitOn)

main :: IO ()
main = do
  -- PART 1: Run diagnostics in BOOST program with input 1
  contents <- fmap lines . readFile $ "input.txt"
  let code = fmap read . head . fmap (splitOn ",") $ contents :: [Int]

  let x = runProg $ newProg code [1]
  print $ output x

  -- PART 2: use input to to get the coordinates of the distress signal
  let y = runProg $ newProg code [2]
  print $ output y
