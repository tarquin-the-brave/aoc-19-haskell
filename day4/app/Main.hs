module Main where

import Lib

main :: IO ()
main = do
  putStrLn "Enter lower bound: "
  minstr <- getLine
  let min = read minstr::Int

  putStrLn "Enter upper bound: "
  maxstr <- getLine
  let max = read maxstr::Int

  print . length $ [x | x <-[min..max], ruleOne x, ruleTwo x]
