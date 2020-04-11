module Main where

import Lib

main :: IO ()
main = do
  -- putStrLn "Enter lower bound: "
  -- minstr <- getLine
  -- let min = read minstr::Int
  --
  -- putStrLn "Enter upper bound: "
  -- maxstr <- getLine
  -- let max = read maxstr::Int

  let ruleOneTwoList = [x | x <-[234208..765869], ruleOne x, ruleTwo x]
  let ruleOneTwoThreeList = [x | x <- ruleOneTwoList, ruleThree x]

  print "Numbers that never decrease and have two adjacent digits:"
  print . length $ ruleOneTwoList

  print "Rule out the numbers where the adjacent digits are part of larger group match:"
  print .length $ ruleOneTwoThreeList

