module Main where

import           Data.List.Split (splitOn)
import           Lib

main :: IO ()
main = do
  contents <- fmap lines . readFile $ "input.txt"
  let diagscode = fmap read . head . fmap (splitOn ",") $ contents :: [Int]

  print . head . output . runProg . newProg diagscode $ [1]

  print . head . output . runProg . newProg diagscode $ [5]

