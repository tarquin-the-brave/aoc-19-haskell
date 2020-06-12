import qualified IntcodeProgram as IC
import Lens.Micro.Platform (view)

import Test.Tasty
import Test.Tasty.HUnit
import Data.List.Split (splitOn)
import qualified Test.Tasty.Laws.Applicative as A
import qualified Test.Tasty.Laws.Functor as F
import qualified Test.Tasty.Laws.Monad as M

main :: IO()
main = do
  testProg <- fmap (fmap read . splitOn "," . head . lines) . readFile $ "day9-input.txt"
  defaultMain $ tests testProg

tests :: [Int] -> TestTree
tests ic = testGroup "All Tests" [functionalTests ic, testLaws]

functionalTests :: [Int] -> TestTree
functionalTests ic = testGroup "Tests of intcode computer function"
  [
    testCase "Intcode from day 9 tests all operations work correctly" $
    (fmap IC.out . IC.run $ IC.new ic [1]) @?= IC.End [2171728567]
  ]

testLaws :: TestTree
testLaws = testGroup "Test Functor, Applicative, & Monad laws" [testF]
-- testLaws = testGroup "Test Functor, Applicative, & Monad laws" [testF, testA, testM]

testF :: TestTree
testF = testGroup "Functor Laws (Unit)"
  [
    F.testUnit [IC.Running (), IC.AwaitInput (), IC.End (), IC.Crashed "woops"]
  ]

-- testA :: TestTree
-- testA = testGroup "Applicative Laws (Unit)"
--   [
--     A.testUnit [IC.Running (), IC.AwaitInput (), IC.End (), IC.Crashed "woops"]
--   ]

-- testM :: TestTree
-- testM = testGroup "Monad Laws (Unit)"
--   [
--     M.testUnit [IC.Running (), IC.AwaitInput (), IC.End (), IC.Crashed "woops"]
--   ]
--

