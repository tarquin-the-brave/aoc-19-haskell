module Main where

import Lib
import Data.Tree
import Data.List.Split (splitOn)

main :: IO ()
main = do
  contents <- fmap lines . readFile $ "input.txt"
  -- contents <- fmap lines . readFile $ "input-example.txt"
  let orbits = fmap parseOrbit contents

  let everything = unfoldForest (forestBuilder orbits) (roots orbits)

  -- putStr . drawForest $ everything

  print . sumDepthsForest $ everything


parseOrbit :: String -> (String, String)
parseOrbit = tuplize . splitOn ")"

tuplize :: [a] -> (a,a)
tuplize (x:y:z) = (x,y)

