module Main where

import Lib
import Data.List.Split (splitOn)
import Control.Monad.State.Lazy
import System.Console.ANSI
import qualified Display as D

main :: IO ()
main = do
  contents <- fmap lines . readFile $ "input.txt"
  let code = fmap read . head . fmap (splitOn ",") $ contents :: [Int]

  (oxygenLocation, gridHeight) <- evalStateT (runDroid' D.U) (newDroid code)

  cursorDown gridHeight

  print oxygenLocation

