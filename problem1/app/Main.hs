module Main where

import Lib
import Data.List

main :: IO ()
main = do
  contents <- readFile "inputs.txt"
  let inputs = fmap read . lines $ contents
  print . Lib.tot_1 $ inputs
  print . Lib.tot_2 $ inputs
