module Lib
    ( runIntcode
    ) where

runIntcode :: [Int] -> Int
runIntcode input = head . program . runProgram $ Program{program = input, state = Running, progressIndex = 0}

data RunState = Running | Terminated | TerminatedBadly deriving(Show)
data Program = Program {program::[Int], state::RunState, progressIndex::Int} deriving(Show)

runProgram :: Program -> Program
runProgram prog = case state prog of
   Running -> runProgram . runStep $ prog
   _       -> prog

runStep :: Program -> Program
runStep prog
  | opcode == 1 = Program{program = op1 idx . program $ prog, state = Running, progressIndex = (idx) + 4}
  | opcode == 2 = Program{program = op2 idx . program $ prog, state = Running, progressIndex = (idx) + 4}
  | opcode == 99 = Program{program = program prog, state = Terminated, progressIndex = idx}
  | otherwise = Program{program = program prog, state = TerminatedBadly, progressIndex = idx}
  -- unsafe
  where
    idx = progressIndex prog
    opcode = (program prog) !! (idx)

opGeneral :: (Int -> Int -> Int) -> Int -> [Int] -> [Int]
opGeneral f idx xs = replaceNth (xs!!(idx + 3)) (f (xs!!(xs!!(idx + 1))) (xs!!(xs!!(idx + 2)))) xs

op1 :: Int -> [Int] -> [Int]
op2 :: Int -> [Int] -> [Int]
op1 = opGeneral (+)
op2 = opGeneral (*)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs
