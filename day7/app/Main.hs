module Main where

import Lib
import Data.List.Split (splitOn)
import Data.List (permutations)
import Control.Monad
import qualified Data.List.NonEmpty as NEL

main :: IO ()
main = do
  -- PART 1: Max thruster for single pass through
  contents <- fmap lines . readFile $ "input.txt"
  -- contents <- fmap lines . readFile $ "example-1.txt"
  -- contents <- fmap lines . readFile $ "example-2.txt"
  -- contents <- fmap lines . readFile $ "example-3.txt"
  let ampCode = fmap read . head . fmap (splitOn ",") $ contents :: [Int]

  let runAmp = amplifyerPart1 ampCode

  let results = fmap (\comb -> (foldM runAmp 0 comb, comb)) phaseCombinations :: [(Maybe Int, [Int])]

  let max = maxOut results

  print max

  -- PART 2:


phaseCombinations :: [[Int]]
phaseCombinations = permutations [0..4]

phaseCombinations' :: [NEL.NonEmpty Int]
phaseCombinations' = fmap NEL.fromList phaseCombinations

maxOut :: [(Maybe Int, [Int])] -> Maybe (Int, [Int])
maxOut = foldl maxOutFoldFunc (Just (minBound, []))

maxOutFoldFunc :: Maybe (Int, [Int]) -> (Maybe Int, [Int]) -> Maybe (Int, [Int])
maxOutFoldFunc acc x = do
  acc <- acc
  case fst x of
    Nothing -> return acc
    Just xOut -> return (if (xOut > fst acc) then (xOut, snd x) else acc)