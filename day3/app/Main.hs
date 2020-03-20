module Main where

import Lib
import Data.List.Split
import Text.Read

main :: IO ()
main = do
  contents <- readFile "input.txt"

  -- paths :: [[String]]
  let input = fmap (splitOn ",") . lines $ contents

  -- pathsParsed :: Maybe [[Translation]]
  case superUnwrap . pathsRead $ input of
    Just paths -> print $ nearestIntersection (paths!!0) (paths!!1)
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

nearestIntersection :: [Translation] -> [Translation] -> Int
nearestIntersection p1 p2 = minimum . fmap (\(a,b)-> a+b) $ pathCrossings p1 p2
