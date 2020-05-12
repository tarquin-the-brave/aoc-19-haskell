module Main where

import           Data.List
import           Lib

main :: IO ()
main = do
  contents <- readFile "inputs.txt"
  let inputs = fmap read . lines $ contents
  print . Lib.tot_1 $ inputs
  print . Lib.tot_2 $ inputs
