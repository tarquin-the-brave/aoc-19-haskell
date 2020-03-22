module Main where

import Lib
import Data.List.Split (splitOn)

main :: IO ()
main = do
  contents <- fmap lines . readFile $ "input.txt"
  let input = fmap read . head . fmap (splitOn ",") $ contents :: [Int]
  print . head . program . runProgram $ Program{program = code1202 input, state = Running, progressIndex = 0}

-- NOTE: should only run this function if input has more than 12 elements.
code1202 :: [Int] -> [Int]
code1202 (x0:x1:x2:xs) = x0:12:2:xs

