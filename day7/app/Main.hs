module Main where

import Lib
import Data.List.Split (splitOn)
import Data.List (permutations)
import Control.Monad
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

  -- let phases = [9,8,7,6,5]
  -- let x = newAmps' phases
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf x)
  -- let a = manualLoopStep x
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf a)
  -- let aa = manualLoopStep a
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aa)
  -- let aaa = manualLoopStep aa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaa)
  -- let aaaa = manualLoopStep aaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaa)
  -- let aaaaa = manualLoopStep aaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaa)
  -- let aaaaaa = manualLoopStep aaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaa)
  -- let aaaaaaa = manualLoopStep aaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaa)
  -- let aaaaaaaa = manualLoopStep aaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaa)
  -- let aaaaaaaaa = manualLoopStep aaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaa)
  -- let aaaaaaaaaa = manualLoopStep aaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaa)
  -- let aaaaaaaaaaa = manualLoopStep aaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaa)
  -- let aaaaaaaaaaaa = manualLoopStep aaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaa)
  -- let aaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
  -- let aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = manualLoopStep aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  -- print $ fmap (\p-> (input p, output p, progState p)) (ampsOf aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
  --

  -- let b = manualLoopStep a
  -- let c = manualLoopStep b
  -- let d = manualLoopStep c
  -- let e = manualLoopStep d
  -- let f = manualLoopStep e
  --
  -- print a
  -- print b
  -- print c
  -- print d
  -- print e
  -- print f
  --
  -- print "run full loop"
  -- let y = evalState runAmpsLoop x
  -- print y

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

manualLoopStep :: Amps -> Amps
manualLoopStep x = x1am2
  where x1 = runActiveAmpOutput x
        x1a = snd x1
        x1a' = activeAmpScrubOutput x1a
        x1am1 = nextAmp x1a'
        x1am2 = activeAmpAppendInput (fst x1) x1am1

