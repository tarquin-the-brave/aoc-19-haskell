module Main where

import Lib
import Data.List.Split
import Text.Read
import Control.Monad.Fail

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let input = fmap (splitOn ",") . lines $ contents

  case superUnwrap . pathsRead $ input of
    Just paths -> do
      let path1 = paths!!0
      let path2 = paths!!1
      let crossings = pathCrossings path1 path2

      -- PART 1: Manhattan distance of closest intersection
      print . minimum $ fmap (\(a,b)-> a+b) crossings

      -- PART 2: Intersection with the smallest travel down wires.
      case sequence $ fmap (sumWireLength path1 path2) crossings of
        Nothing -> print "crossing points weren't found on wires"
        Just wireLengths -> print $ minimum wireLengths

    Nothing -> print "input parsing failure"

parseTranslation :: String -> Maybe Translation
parseTranslation s = Translation <$> (parseDirection . head $ s) <*> (readMaybe . tail $ s)

parseDirection :: Char -> Maybe Direction
parseDirection 'U' = Just U
parseDirection 'D' = Just D
parseDirection 'L' = Just L
parseDirection 'R' = Just R
parseDirection _ = Nothing

pathsRead :: [[String]] -> [[Maybe Translation]]
pathsRead = fmap (fmap parseTranslation)

superUnwrap :: [[Maybe a]] -> Maybe [[a]]
superUnwrap = sequence . fmap sequence
