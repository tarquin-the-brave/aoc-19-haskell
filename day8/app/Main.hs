module Main where

import Lib
import Data.List

main :: IO ()
main = do
  contents <- readFile "input-example.txt"
  let digits = head . lines $ contents :: [Char]
  print . splitLayers 3 2 $ digits

splitLayers :: Integral a => a -> a -> [b] -> [[[b]]]
splitLayers width height = fmap (sublists width) . sublists (width * height)

sublists :: Integral a => a -> [b] -> [[b]]
sublists n xs
  | n - 1 > genericLength xs = []
  | otherwise = (\(x, y) -> [x] ++ sublists n y) . genericSplitAt n $ xs
