module Intcode
    ( runIntcode
    ) where

import Control.Monad.State.Lazy
import qualified Data.List.Safe as Safe

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
  | opcode == Just 1 = ((), Program{program = op1 idx . program $ prog, programState = Running, progressIndex = (idx) + 4})
  | opcode == Just 2 = ((), Program{program = op2 idx . program $ prog, programState = Running, progressIndex = (idx) + 4})
  | opcode == Just 99 = ((), Program{program = program prog, programState = Terminated, progressIndex = idx})
  | otherwise = ((), Program{program = program prog, programState = TerminatedBadly, progressIndex = idx})
  -- unsafe
  where
    idx = progressIndex prog
    opcode = (program prog) Safe.!! (idx) :: Maybe Int

opBinary :: (Int -> Int -> Int) -> Int -> [Int] -> Maybe [Int]
-- opBinary f idx xs = replaceNth (xs!!(idx + 3)) (f (xs!!(xs!!(idx + 1))) (xs!!(xs!!(idx + 2)))) xs
opBinary f idx xs = do
  n <- xs Safe.!! (idx + 3)
  p1idx <- xs Safe.!! (idx + 1)
  p1 <- xs Safe.!! p1idx
  p2idx <- xs Safe.!! (idx + 2)
  p2 <- xs Safe.!! p2idx
  replaceNth n (f p1 p2) xs

op1 :: Int -> [Int] -> Maybe [Int]
op2 :: Int -> [Int] -> Maybe [Int]
op1 = opBinary (+)
op2 = opBinary (*)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs
