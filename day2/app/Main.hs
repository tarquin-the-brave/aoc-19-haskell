module Main where

import           Data.List.Split (splitOn)
import           Lib

main :: IO ()
main = do
  contents <- fmap lines . readFile $ "input.txt"
  let input = fmap read . head . fmap (splitOn ",") $ contents :: [Int]

  -- Part 1: apply code 1202 and get output.
  print . runIntcode . code1202 $ input

  -- Part 2: what code give output 19690720
  let allResults = (\x y-> (runIntcode . codeXY x y $ input, (x,y))) <$> [0..99] <*> [0..99]
  print [snd x | x <- allResults, fst x == 19690720]


-- NOTE: should only run this function if input has more than 12 elements.
code1202 :: [Int] -> [Int]
code1202 = codeXY 12 2

codeXY :: Int -> Int -> [Int] -> [Int]
codeXY x y (x0:x1:x2:xs) = x0:x:y:xs

