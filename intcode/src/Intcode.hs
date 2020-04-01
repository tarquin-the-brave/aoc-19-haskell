module Intcode
    ( newProg
    , runProg
    , stepProg
    , Prog (..)) where

import qualified Data.List.Safe as Safe

newProg :: [Int] -> [Int] -> Prog
newProg newIntCode input = Prog{
  input = input,
  intCode = newIntCode,
  progState = Running,
  progressIndex = 0,
  output = []
}

runProg :: Prog -> Prog
runProg prog = case progState prog of
   Running -> runProg . stepProg $ prog
   _ -> prog

data ProgState = Running | Terminated | TerminatedBadly deriving(Show)
data Prog = Prog {
  input::[Int],
  intCode::[Int],
  progState::ProgState,
  progressIndex::Int,
  output::[Int]
} deriving(Show)

stepProg :: Prog -> Prog
stepProg prog = runCode (getCode prog) prog

getCode prog = do
  code <- (intCode prog) Safe.!! (progressIndex prog)
  parseOpCode code

runCode (Just (m1, m2, One)) = code1 m1 m2
runCode (Just (m1, m2, Two)) = code2 m1 m2
runCode (Just (_, _, Three)) = code3
runCode (Just (m, _, Four)) = code4 m
runCode (Just (_, _, NinetyNine)) = code99
runCode Nothing = codeX

code1 :: ParamMode -> ParamMode -> Prog -> Prog
code1 mode1 mode2 prog = case op1 mode1 mode2 idx . intCode $ prog of
  Nothing -> Prog{
    input = input prog,
    intCode = intCode prog,
    progState = TerminatedBadly,
    progressIndex = idx,
    output = output prog
  }
  Just newIntCode -> Prog{
    input = input prog,
    intCode = newIntCode,
    progState = Running,
    progressIndex = idx + 4,
    output = output prog
  }
  where idx = progressIndex prog

code2 :: ParamMode -> ParamMode -> Prog -> Prog
code2 mode1 mode2 prog = case op2 mode1 mode2 idx . intCode $ prog of
  Nothing -> Prog{
    input = input prog,
    intCode = intCode prog,
    progState = TerminatedBadly,
    progressIndex = idx,
    output = output prog
  }
  Just newIntCode -> Prog{
    input = input prog,
    intCode = newIntCode,
    progState = Running,
    progressIndex = idx + 4,
    output = output prog
  }
  where idx = progressIndex prog

applyOp3 prog = do
  i <- Safe.head (input prog)
  op3 i (progressIndex prog) (intCode prog)

code3 :: Prog -> Prog
code3 prog = case applyOp3 prog of
  Nothing -> Prog{
    input = input prog,
    intCode = intCode prog,
    progState = TerminatedBadly,
    progressIndex = idx,
    output = output prog
  }
  Just newIntCode -> Prog{
    input = tail . input $ prog,
    intCode = newIntCode,
    progState = Running,
    progressIndex = idx + 2,
    output = output prog
  }
  where idx = progressIndex prog

code4 :: ParamMode -> Prog -> Prog
code4 mode prog = case op4 mode idx . intCode $ prog of
  Nothing -> Prog{
    input = input prog,
    intCode = intCode prog,
    progState = TerminatedBadly,
    progressIndex = idx,
    output = output prog
  }
  Just newOutput -> Prog{
    input = input $ prog,
    intCode = intCode prog,
    progState = Running,
    progressIndex = idx + 2,
    output = newOutput:(output prog)
  }
  where idx = progressIndex prog

code99 :: Prog -> Prog
code99 prog = Prog{
  input = input prog,
  intCode = intCode prog,
  progState = Terminated,
  progressIndex = progressIndex prog,
  output = output prog
}

codeX :: Prog -> Prog
codeX prog = Prog{
  input = input prog,
  intCode = intCode prog,
  progState = TerminatedBadly,
  progressIndex = progressIndex prog,
  output = output prog
}

op1 :: ParamMode -> ParamMode -> Int -> [Int] -> Maybe [Int]
op2 :: ParamMode -> ParamMode -> Int -> [Int] -> Maybe [Int]
op1 = opBinary (+)
op2 = opBinary (*)

opBinary :: (Int -> Int -> Int) -> ParamMode -> ParamMode -> Int -> [Int] -> Maybe [Int]
opBinary f mode1 mode2 idx xs = do
  n <- xs Safe.!! (idx + 3)
  p1 <- getParam mode1 (idx + 1) xs
  p2 <- getParam mode2 (idx + 2) xs
  replaceNth n (f p1 p2) xs

op3 :: Int -> Int -> [Int] -> Maybe [Int]
op3 i idx xs = do
  n <- xs Safe.!! (idx + 1)
  replaceNth n i xs

op4 :: ParamMode -> Int -> [Int] -> Maybe Int
op4 mode idx xs = getParam mode (idx + 1) xs

replaceNth :: Int -> a -> [a] -> Maybe [a]
replaceNth n newVal xs
  | n < 0 = Nothing
  | n < length xs = Just $ replaceNthInner n newVal xs
  | otherwise = Nothing

replaceNthInner :: Int -> a -> [a] -> [a]
replaceNthInner _ _ [] = []
replaceNthInner n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNthInner (n-1) newVal xs

data ParamMode = Pos | Imm deriving(Show)

getParam :: ParamMode -> Int -> [Int] -> Maybe Int
getParam Pos idx xs = do
  p <- xs Safe.!! idx
  xs Safe.!! p
getParam Imm idx xs = xs Safe.!! idx

data OpCode = One | Two | Three | Four | NinetyNine deriving(Show)

parseOpCode :: Int -> Maybe (ParamMode, ParamMode, OpCode)
parseOpCode num
  | num == 1 = Just (Pos, Pos, One)
  | num == 101 = Just (Imm, Pos, One)
  | num == 1001 = Just (Pos, Imm, One)
  | num == 1101 = Just (Imm, Imm, One)
  | num == 2 = Just (Pos, Pos, Two)
  | num == 102 = Just (Imm, Pos, Two)
  | num == 1002 = Just (Pos, Imm, Two)
  | num == 1102 = Just (Imm, Imm, Two)
  | num == 3 = Just (Pos, Pos, Three)
  | num == 4 = Just (Pos, Pos, Four)
  | num == 104 = Just (Imm, Pos, Four)
  | num == 99 = Just (Pos, Pos, NinetyNine)

