module Lib
    ( runIntcode
    ) where

import Control.Monad.State.Lazy

runIntcode :: [Int] -> Int
runIntcode input = head . program $ execState runProgram Program{program = input, programState = Running, progressIndex = 0}

runProgram :: State Program ()
runProgram = do
  prog <- get
  case programState prog of
    Running -> do
      runStep
      runProgram
    _ -> return ()

data ProgramState = Running | Terminated | TerminatedBadly deriving(Show)
data Program = Program {program::[Int], programState::ProgramState, progressIndex::Int} deriving(Show)

runStep :: State Program ()
runStep = state stepProgram

stepProgram :: Program -> ((), Program)
stepProgram prog
  | opcode == 1 = ((), Program{program = op1 idx . program $ prog, programState = Running, progressIndex = (idx) + 4})
  | opcode == 2 = ((), Program{program = op2 idx . program $ prog, programState = Running, progressIndex = (idx) + 4})
  | opcode == 99 = ((), Program{program = program prog, programState = Terminated, progressIndex = idx})
  | otherwise = ((), Program{program = program prog, programState = TerminatedBadly, progressIndex = idx})
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
