module Main where

import Lib
import Data.List

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let digits = head . lines $ contents :: [Char]
  let message = splitLayers 25 6 digits

  -- Part 1: product of the number of '2's and '1's for the layer with the fewest '0's
  let answer = (\(x,y)-> x * y) . sndMinFst . fmap count012 $ message  :: Int
  print answer

  -- Part 2: print out the message. Top layer first.
  let resolvedLayer = foldl addLayers (replicate (25*6) '2') message
  mapM_ print . sublists 25 . fmap displayChar $ resolvedLayer

-- Convert the pixels to make the message easier to see.
-- 0: black -> '_'
-- 1: white -> 'X'
displayChar :: Char -> Char
displayChar c = if c == '1' then 'X' else ' '

addLayers :: String -> String -> String
addLayers = zipWith overlayChar

overlayChar :: Char -> Char -> Char
overlayChar c d = if c == '2' then d else c

splitLayers :: Integral a => a -> a -> [b] -> [[b]]
splitLayers width height = sublists (width * height)

sublists :: Integral a => a -> [b] -> [[b]]
sublists n xs
  | n - 1 > genericLength xs = []
  | otherwise = (\(x, y) -> [x] ++ sublists n y) . genericSplitAt n $ xs

count012 :: Integral a => String -> (a, (a, a))
count012 s = (count '0' s, (count '1' s, count '2' s))

count :: Integral a => Char -> String -> a
count c = foldl (\acc x -> if x == c then acc + 1 else acc) 0

sndMinFst :: Bounded a => Integral a => [(a , b)] -> b
sndMinFst = snd . foldl (\acc x -> if fst x < fst acc then x else acc) (maxBound, undefined)

