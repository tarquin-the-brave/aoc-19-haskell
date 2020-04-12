module Lib
    ( roots
    , forestBuilder
    , sumDepthsForest
    , orbitalHops
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

orbitalHops :: Tree String -> String -> String -> Int
orbitalHops orbs n1 n2 = (HS.size onlyInPath1) + (HS.size onlyInPath2) - 2
  where
    onlyInPath1 = HS.difference (HS.fromList path1) (HS.fromList path2)
    onlyInPath2 = HS.difference (HS.fromList path2) (HS.fromList path1)
    path1 = path n1
    path2 = path n2
    path n = pathTo n orbs
    pathTo nodeName = splitOn "," . foldTree (ff nodeName)
    ff match nodeName childAccs = if nodeName == match
      then
        nodeName
      else
        case Safe.head [acc | acc <- childAccs, length acc > 0] of
          Nothing -> ""
          Just path -> nodeName ++ "," ++ path

