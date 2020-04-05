import Intcode

import Test.Tasty
import Test.Tasty.HUnit

main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [singleStepTests, runProgramTests, op56Testing]

singleStepTests :: TestTree
singleStepTests = testGroup "Test progressing the program by one instruction"
  [ testCase "Day 2 example 1 step 1" $
    stepProg day2Example1Init  @?= day2Example1Step1

    , testCase "Day 2 example 1 step 2" $
    stepProg day2Example1Step1  @?= day2Example1Step2

    , testCase "Day 2 example 1 step 3 - finish" $
    stepProg day2Example1Step2  @?= day2Example1Fin

    , testCase "Day 2 example 1 - try step finished prog" $
    stepProg day2Example1Fin  @?= day2Example1Fin
  ]

day2Example1Init :: Prog
day2Example1Init = newProg [1,9,10,3,2,3,11,0,99,30,40,50] []

day2Example1Step1 :: Prog
day2Example1Step1 = Prog{
  input=[],
  intCode=[1,9,10,70,2,3,11,0,99,30,40,50],
  progState=Running,
  ip=4,
  rb=0,
  output=[]
}

day2Example1Step2 :: Prog
day2Example1Step2 = Prog{
  input=[],
  intCode=[3500,9,10,70,2,3,11,0,99,30,40,50],
  progState=Running,
  ip=8,
  rb=0,
  output=[]
}

day2Example1Fin :: Prog
day2Example1Fin = Prog{
  input=[],
  intCode=[3500,9,10,70,2,3,11,0,99,30,40,50],
  progState=Terminated,
  ip=8,
  rb=0,
  output=[]
}

runProgramTests :: TestTree
runProgramTests = testGroup "Test runnning the program til it stops"
  [
    day2Examples
    , day5Examples
    , day9Examples
  ]

day2Examples :: TestTree
day2Examples = testGroup "Test examples from day 2 of AOC"
  [
    testCase "Day 2 example 2 - (1 + 1 = 2)" $
    runProg day2Example2Init @?= day2Example2Fin

    , testCase "Day 2 example 3 (3 * 2 = 6)" $
    runProg day2Example3Init @?= day2Example3Fin

    , testCase "Day 2 example 4 (99 * 99 = 9801)" $
    runProg day2Example4Init @?= day2Example4Fin

    , testCase "Day 2 example 5" $
    runProg day2Example5Init @?= day2Example5Fin
  ]

--
-- Examples from day 2 where only opcodes 1 & 2 are required.
--
day2Example2Init :: Prog
day2Example2Init = newProg [1,0,0,0,99] []

day2Example3Init :: Prog
day2Example3Init = newProg [2,3,0,3,99] []

day2Example4Init :: Prog
day2Example4Init = newProg [2,4,4,5,99,0] []

day2Example5Init :: Prog
day2Example5Init = newProg [1,1,1,4,99,5,6,0,99] []

day2Example2Fin :: Prog
day2Example2Fin = Prog{
  input=[],
  intCode=[2,0,0,0,99],
  progState=Terminated,
  ip=4,
  rb=0,
  output=[]
}

day2Example3Fin :: Prog
day2Example3Fin = Prog{
  input=[],
  intCode=[2,3,0,6,99],
  progState=Terminated,
  ip=4,
  rb=0,
  output=[]
}

day2Example4Fin :: Prog
day2Example4Fin = Prog{
  input=[],
  intCode=[2,4,4,5,99,9801],
  progState=Terminated,
  ip=4,
  rb=0,
  output=[]
}

day2Example5Fin :: Prog
day2Example5Fin = Prog{
  input=[],
  intCode=[30,1,1,4,2,5,6,0,99],
  progState=Terminated,
  ip=8,
  rb=0,
  output=[]
}

--
-- Examples from day 5 where opcodes 3, 4, 5, 6, 7, & 8 and
-- parameter modes are introduced.
--
day5Examples :: TestTree
day5Examples = testGroup "Test examples from day 5 of AOC"
  [
    testCase "Day 5 example 1 - echo" $
    runProg day5Example1Init @?= day5Example1Fin

    , testCase "Day 5 example 2 - immediate mode" $
    runProg day5Example2Init @?= day5Example2Fin

    , testCase "Day 5 example 3 - input equal to 8? yes" $
    (head $ output $ runProg $ newProg day5ExampleIncode3 [8]) @?= 1

    , testCase "Day 5 example 3 - input equal to 8? no" $
    (head $ output $ runProg $ newProg day5ExampleIncode3 [7]) @?= 0

    , testCase "Day 5 example 4 - input less than 8? yes" $
    (head $ output $ runProg $ newProg day5ExampleIncode4 [-20]) @?= 1

    , testCase "Day 5 example 4 - input less than 8? no" $
    (head $ output $ runProg $ newProg day5ExampleIncode4 [8]) @?= 0

    , testCase "Day 5 example 4 - input less than 8? no" $
    (head $ output $ runProg $ newProg day5ExampleIncode4 [9]) @?= 0

    , testCase "Day 5 example 5 - input equal to 8? yes" $
    (head $ output $ runProg $ newProg day5ExampleIncode5 [8]) @?= 1

    , testCase "Day 5 example 5 - input equal to 8? no" $
    (head $ output $ runProg $ newProg day5ExampleIncode5 [7]) @?= 0

    , testCase "Day 5 example 6 - input less than 8? yes" $
    (head $ output $ runProg $ newProg day5ExampleIncode6 [-20]) @?= 1

    , testCase "Day 5 example 6 - input less than 8? no" $
    (head $ output $ runProg $ newProg day5ExampleIncode6 [8]) @?= 0

    , testCase "Day 5 example 6 - input less than 8? no" $
    (head $ output $ runProg $ newProg day5ExampleIncode6 [9]) @?= 0

    , testCase "Day 5 big example - input less than 8" $
    (head $ output $ runProg $ newProg day5BigExample [7]) @?= 999

    , testCase "Day 5 big example - input equal to 8" $
    (head $ output $ runProg $ newProg day5BigExample [8]) @?= 1000

    , testCase "Day 5 big example - input more than 8" $
    (head $ output $ runProg $ newProg day5BigExample [9]) @?= 1001
  ]

day5Example1Init :: Prog
day5Example1Init = newProg [3,0,4,0,99] [42]

day5Example2Init :: Prog
day5Example2Init = newProg [1002,4,3,4,33] []

day5Example1Fin :: Prog
day5Example1Fin = Prog {
  input=[],
  intCode=[42,0,4,0,99],
  progState=Terminated,
  ip=4,
  rb=0,
  output=[42]
}

day5Example2Fin :: Prog
day5Example2Fin = Prog {
  input=[],
  intCode=[1002,4,3,4,99],
  progState=Terminated,
  ip=4,
  rb=0,
  output=[]
}

day5ExampleIncode3 :: [Int]
day5ExampleIncode3 = [3,9,8,9,10,9,4,9,99,-1,8]

day5ExampleIncode4 :: [Int]
day5ExampleIncode4 = [3,9,7,9,10,9,4,9,99,-1,8]

day5ExampleIncode5 :: [Int]
day5ExampleIncode5 = [3,3,1108,-1,8,3,4,3,99]

day5ExampleIncode6 :: [Int]
day5ExampleIncode6 = [3,3,1107,-1,8,3,4,3,99]

day5BigExample :: [Int]
day5BigExample = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                  1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                  999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]

--
-- day9 examples.  day 9 introduced relative mode and op code 9
-- that changes the relative base offset to the value of its
-- parameter.
--
day9Examples :: TestTree
day9Examples = testGroup "Test examples from day 9 of AOC"
  [
    testCase "Day 9 example 1 - copy of self" $
    (output $ runProg $ newProg day9e1 []) @?= reverse day9e1

    , testCase "Day 9 example 2 - output 16 digit number" $
    (length $ show $ head $ output $ runProg $ newProg day9e2 []) @?= 16

    , testCase "Day 9 example 3 - output large munber in the middle" $
    (output $ runProg $ newProg day9e3 []) @?= [day9e3 !! 1]

    , testCase "Write value to beyond end of program" $
    (intCode . runProg $ newProg [1101,2,3,10,99] []) @?= [1101,2,3,10,99,0,0,0,0,0,5]

    , testCase "Read value to beyond end of program - should be 0" $
    (output . runProg $ newProg [4,22,99] []) @?= [0]

    , testCase "specific problem" $
    (output . stepProg . stepProg $ newProg day9e1 []) @?= [109]

    , testCase "try a bit" $
    (runProg $ newProg [109,1,204,-1,99] []) @?= Prog { input=[], intCode=[109,1,204,-1,99], progState=Terminated, ip=4, rb=1, output=[109]}
  ]

day9e1 :: [Int]
day9e1 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

day9e2 :: [Int]
day9e2 = [1102,34915192,34915192,7,4,7,99,0]

day9e3 :: [Int]
day9e3 = [104,1125899906842624,99]

--
-- OpCode 5 & 6 testing
--
op56Testing :: TestTree
op56Testing = testGroup "Some testing of Opcodes 5 & 6"
  [
    testCase "op code 6 - test 1" $
    (stepProg $ newProg [6,4,5,99,0,8,0,0,99] []) @?= op6test1out
  ]


op6test1out :: Prog
op6test1out = Prog {
  input=[],
  intCode=[6,4,5,99,0,8,0,0,99],
  progState=Running,
  ip=8,
  rb=0,
  output=[]
}



