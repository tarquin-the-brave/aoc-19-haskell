import qualified IntcodeProgram as IC

import Test.Tasty
import Test.Tasty.HUnit
import Data.List.Split (splitOn)

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
    (fmap IC.output . IC.run $ IC.new ic [1]) @?= IC.End [2171728567]
  ]

testLaws :: TestTree
-- testLaws = testGroup "Test Functor, Applicative, & MOnad laws" [testF, testA, testM]
testLaws = testGroup "Test Functor, Applicative, & Monad laws" [testF]

testF :: TestTree
-- testF = testGroup "Functor Laws" [fLaw1, fLaw2]
testF = testGroup "Functor Laws" [fLaw1]

fLaw1 :: TestTree
fLaw1 = testGroup "Law 1: fmap id = id"
  [
    testCase "Running varient" $
    (fmap id $ IC.Running ()) @?= (id $ IC.Running ())

    , testCase "Crashed varient" $
    (fmap id $ IC.Crashed "woops") @?= ((id $ IC.Crashed "woops")::IC.Prog ())
  ]

