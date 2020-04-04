module Main where

import Lib
import Data.List.Split (splitOn)
import Data.List (permutations)
import Control.Monad (foldM)
import Control.Monad.State.Lazy (runState, evalState)

main :: IO ()
main = do
  -- PART 1: Max thruster for single pass through
  contents <- fmap lines . readFile $ "input.txt"
  -- contents <- fmap lines . readFile $ "example-1.txt"
  -- contents <- fmap lines . readFile $ "example-2.txt"
  -- contents <- fmap lines . readFile $ "example-3.txt"
  let ampCode = fmap read . head . fmap (splitOn ",") $ contents :: [Int]

  let runAmp = amplifyerPart1 ampCode

  let results = fmap (\comb -> (foldM runAmp 0 comb, comb)) phaseCombinations1 :: [(Maybe Int, [Int])]

  let max = maxOut results

  print max

  -- PART 2:
  contents <- fmap lines . readFile $ "input.txt"
  -- contents <- fmap lines . readFile $ "example-part2-1.txt"
  -- contents <- fmap lines . readFile $ "example-part2-2.txt"
  let ampCode = fmap read . head . fmap (splitOn ",") $ contents :: [Int]

  let newAmps' = newAmps ampCode

  let results = fmap (\comb -> (evalState runAmpsLoop $ newAmps' comb, comb)) phaseCombinations2 :: [(Maybe Int, [Int])]
  print $ maxOut results

phaseCombinations1 :: [[Int]]
phaseCombinations1 = permutations [0..4]

phaseCombinations2 :: [[Int]]
phaseCombinations2 = permutations [5..9]

maxOut :: [(Maybe Int, [Int])] -> Maybe (Int, [Int])
maxOut = foldl maxOutFoldFunc (Just (minBound, []))

maxOutFoldFunc :: Maybe (Int, [Int]) -> (Maybe Int, [Int]) -> Maybe (Int, [Int])
maxOutFoldFunc acc x = do
  acc <- acc
  case fst x of
    Nothing -> return acc
    Just xOut -> return (if (xOut > fst acc) then (xOut, snd x) else acc)

