#!/usr/bin/env stack
-- stack --resolver lts-15.4 script
import           Data.List

f :: Integral a => a -> a
f x = x `div` 3 - 2

tot_1 :: Integral a => [a] -> a
tot_1 = tot f

g :: Integral a => a -> a
g x
  | f x >= 0 = f x + g(f x)
  | otherwise = 0

tot_2 :: Integral a => [a] -> a
tot_2 = tot g

tot :: Integral a => (a -> a) -> [a] -> a
tot f = sum . fmap f

main :: IO ()
main = do
  contents <- readFile "day1-inputs.txt"
  let inputs = fmap read . lines $ contents
  print . tot_1 $ inputs
  print . tot_2 $ inputs
