module Lib
    ( tot_1
    , tot_2
    ) where

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

