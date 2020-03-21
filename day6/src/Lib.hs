module Lib
    ( roots
    , forestBuilder
    , sumDepthsForest
    ) where

import Data.List (genericLength)
import Data.Tree

-- Which bodies do not orbit anything?
roots :: Eq a => [(a,a)] -> [a]
roots orbits = [x | (x, y) <- orbits, notElem x . fmap snd $ orbits]

-- Given the orbit data, what orbits a given body?
forestBuilder :: Eq a => [(a,a)] -> a -> (a,[a])
forestBuilder orbits body = (body, [snd orbit | orbit <- orbits, body == fst orbit])

sumDepthsForest :: Integral b => Forest a -> b
sumDepthsForest = foldl (\acc x -> acc + sumDepthsTree x) 0

-- Data.Tree.levels :: Tree a -> [[a]]

sumDepthsTree :: Integral b => Tree a -> b
sumDepthsTree = snd . foldl foldFunc (0, 0) . levels

foldFunc :: Integral a => (a,a) -> [b] -> (a,a)
foldFunc (depth, acc) x = (depth + 1, acc + depth * genericLength x)
