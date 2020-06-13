#!/usr/bin/env stack
-- stack --resolver lts-15.4 script
import Data.List.Split (chunksOf)

main :: IO ()
main = do
  contents <- readFile "day8-input.txt"
  let layers = chunksOf (25*6) . head . lines $ contents

  -- Part 1: product of the number of '2's and '1's for the layer with the fewest '0's
  print $  sndMinFst . fmap (\l -> (count '0' l, (count '1' l) * (count '2' l))) $ layers

  -- Part 2: print out the message. Top layer first.
  mapM_ print . chunksOf 25 . map displayChar . foldl1 (zipWith overlayChar) $ layers

  where
    -- Convert the pixels to make the message easier to see.
    -- 0: black -> ' '
    -- 1: white -> 'X'
    displayChar c = if c == '1' then 'X' else ' '
    overlayChar c d = if c == '2' then d else c
    count c = foldl (\acc x -> if x == c then acc + 1 else acc) 0

    sndMinFst :: Ord a => [(a , b)] -> b
    sndMinFst = snd . foldl1 (\acc x -> if fst x < fst acc then x else acc)

