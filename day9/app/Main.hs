module Main where

import Lib
import Data.List.Split (splitOn)

main :: IO ()
main = do
  -- PART 1: Run diagnostics in BOOST program with input 1
  contents <- fmap lines . readFile $ "input.txt"
  let code = fmap read . head . fmap (splitOn ",") $ contents :: [Int]

  print $ output . runProg $ newProg code [1]
