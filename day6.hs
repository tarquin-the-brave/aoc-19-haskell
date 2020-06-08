#!/usr/bin/env stack
-- stack --resolver lts-15.4 script
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.HashSet    as HS
import qualified Data.List.Safe  as Safe
import           Data.List (genericLength)
import qualified Data.Tree as T
import           Data.Foldable (foldl')
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.Text as Txt
import Data.Monoid ((<>))

main :: IO ()
main = do
  contents <- fmap (Txt.lines . TE.decodeUtf8) . B.readFile $ "day6-input.txt"
  let orbits = fmap parseOrbit contents

  let orbitForest = T.unfoldForest (forestBuilder orbits) (roots orbits)

  -- putStr . drawForest $ orbitForest

  -- PART 1: sum of the depths of each node
  print . sumDepthsForest $ orbitForest

  -- PART 2: Number of steps to get to from the immediate parent of both
  -- you and santa to your common parent.
  print $ "number of trees: " <> ( show . length $ orbitForest )

  -- By this point we happen to know the data given is a single tree.
  let orbitTree = head orbitForest

  orbitalHops orbitTree "YOU" "SAN" >>= print
  orbitalHops orbitTree "SAN" "YOU" >>= print
  orbitalHops orbitTree "SANTA" "YOU" >>= print

parseOrbit :: Txt.Text -> (Txt.Text, Txt.Text)
parseOrbit = (\(x:y:_) -> (x,y)) . Txt.splitOn ")"

-- Which bodies do not orbit anything?
roots :: Eq a => [(a,a)] -> [a]
roots orbits = [x | (x, y) <- orbits, notElem x . fmap snd $ orbits]

-- Given the orbit data, what orbits a given body?
forestBuilder :: Eq a => [(a,a)] -> a -> (a,[a])
forestBuilder orbits body = (body, [snd orbit | orbit <- orbits, body == fst orbit])

sumDepthsForest :: Integral b => T.Forest a -> b
sumDepthsForest = foldl' (\acc x -> acc + sumDepthsTree x) 0

sumDepthsTree :: Integral b => T.Tree a -> b
sumDepthsTree = snd . foldl' ff (0, 0) . T.levels
  where
    ff (depth, acc) x = (depth + 1, acc + depth * genericLength x)

orbitalHops :: MonadFail m => T.Tree Txt.Text -> Txt.Text -> Txt.Text -> m Int
orbitalHops orbs n1 n2 = do
  _ <- orbs `contains` n1
  _ <- orbs `contains` n2
  return $ (HS.size onlyInPath1) + (HS.size onlyInPath2) - 2
  where
    contains :: MonadFail m => T.Tree Txt.Text -> Txt.Text -> m ()
    contains tree node = if node `elem` T.flatten tree then return () else fail . Txt.unpack $ "Could not find element: " <> node
    onlyInPath1 = HS.difference (HS.fromList path1) (HS.fromList path2)
    onlyInPath2 = HS.difference (HS.fromList path2) (HS.fromList path1)
    path1 = path n1
    path2 = path n2
    path n = pathTo n orbs
    pathTo nodeName = Txt.splitOn "," . T.foldTree (ff nodeName)
    ff match nodeName childAccs = if nodeName == match
      then
        nodeName
      else
        case Safe.head [acc | acc <- childAccs, Txt.length acc > 0] of
          Nothing   -> ""
          Just path -> nodeName <> "," <> path


