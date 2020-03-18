module Lib
    ( f,
    tot
    ) where

f :: Integral a => a -> a
f x = x `div` 3 - 2

tot :: Integral a => [a] -> a
tot = sum . fmap f
