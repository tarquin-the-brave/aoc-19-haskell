#!/usr/bin/env stack
-- stack --resolver lts-15.4 script
{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl1')
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

main :: IO ()
main = do
  contents <- TIO.readFile "day8-input.txt"
  let layers = T.chunksOf (25*6) . head . T.lines $ contents

  -- Part 1: product of the number of '2's and '1's for the layer with the fewest '0's
  print $  sndMinFst . fmap (\l -> (T.count "0" l, (T.count "1" l) * (T.count "2" l))) $ layers

  -- Part 2: print out the message. Top layer first.
  mapM_ print . T.chunksOf 25 . T.map displayChar . foldl1' (T.zipWith overlayChar) $ layers

  where
    -- Convert the pixels to make the message easier to see.
    -- 0: black -> ' '
    -- 1: white -> 'X'
    displayChar c = if c == '1' then 'X' else ' '
    overlayChar c d = if c == '2' then d else c

    sndMinFst :: Ord a => [(a , b)] -> b
    sndMinFst = snd . foldl1' (\acc x -> if fst x < fst acc then x else acc)

