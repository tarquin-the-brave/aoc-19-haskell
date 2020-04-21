import qualified IntcodeProgram as IC

import Test.Tasty
import Test.Tasty.HUnit
import Data.List.Split (splitOn)

main :: IO()
main = do
  testProg <- fmap (fmap read . splitOn "," . head . lines) . readFile $ "day9-input.txt"
  defaultMain $ tests testProg

tests :: [Int] -> TestTree
tests ic = testGroup "Day 9 mega test"
  [
    testCase "Intcode from day 9 tests all operations work correctly" $
    (fmap IC.output . IC.run $ IC.new ic [1]) @?= IC.End [2171728567]
  ]
