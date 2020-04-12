module Main where

import Lib
import Data.Tree
import Data.List.Split (splitOn)
import qualified Data.List.Safe as Safe
import qualified Data.HashSet as HS

main :: IO ()
main = do
  contents <- fmap lines . readFile $ "input.txt"
  -- contents <- fmap lines . readFile $ "input-example.txt"
  -- contents <- fmap lines . readFile $ "input-example-2.txt"
  let orbits = fmap parseOrbit contents

  let everything = unfoldForest (forestBuilder orbits) (roots orbits)

  -- putStr . drawForest $ everything

  -- PART 1: sum of the depths of each node
  print . sumDepthsForest $ everything

  -- PART 2: Number of steps to get to from the immediate parent of both
  -- you and santa to your common parent.
  print $ "number of trees: " ++ ( show . length $ everything )

  let orbitTree = head everything

  print $ orbitalHops orbitTree "YOU" "SAN"
  print $ orbitalHops orbitTree "SAN" "YOU"

parseOrbit :: String -> (String, String)
parseOrbit = tuplize . splitOn ")"

tuplize :: [a] -> (a,a)
tuplize (x:y:z) = (x,y)
