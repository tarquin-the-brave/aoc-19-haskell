module Intcode
    ( newProg
    , runProg
    , stepProg
    , Prog (..)
    , ProgState (..)
    , setInputProg) where

import qualified Data.List.Safe as Safe

newProg :: [Int] -> [Int] -> Prog
newProg newIntCode input = Prog{
  input = input,
  intCode = newIntCode,
  progState = Running,
  ip = 0,
  output = []
}

runProg :: Prog -> Prog
runProg prog = case progState prog of
   Running -> runProg . stepProg $ prog
   _ -> prog

data ProgState = Running | AwaitInput | Terminated | TerminatedBadly deriving(Show)
data Prog = Prog {
  input::[Int],
  intCode::[Int],
  progState::ProgState,
  -- ip: Instruction Pointer
  ip::Int,
  output::[Int]
} deriving(Show)

crashProg :: Prog -> Prog
crashProg prog = Prog {
    input = input prog,
    intCode = intCode prog,
    progState = TerminatedBadly,
    ip = ip prog,
    output = output prog
}

endProg :: Prog -> Prog
endProg prog = Prog {
    input = input prog,
    intCode = intCode prog,
    progState = Terminated,
    ip = ip prog,
    output = output prog
}

awaitInputProg :: Prog -> Prog
awaitInputProg prog = Prog {
    input = input prog,
    intCode = intCode prog,
    progState = AwaitInput,
    ip = ip prog,
    output = output prog
}

movePointer :: Int -> Prog -> Prog
movePointer i prog = setPointer (i + ip prog) prog

setPointer :: Int -> Prog -> Prog
setPointer i prog = Prog{
    input = input prog,
    intCode = intCode prog,
    progState = progState prog,
    ip = i,
    output = output prog
}

updateIntCode :: [Int] -> Prog -> Prog
updateIntCode newIntCode prog = Prog{
    input = input prog,
    intCode = newIntCode,
    progState = progState prog,
    ip = ip prog,
    output = output prog
}

-- unsafe
tailInput :: Prog -> Prog
tailInput prog = Prog{
    input = tail $ input prog,
    intCode = intCode prog,
    progState = progState prog,
    ip = ip prog,
    output = output prog
}

setInputProg :: [Int] -> Prog -> Prog
setInputProg i prog = Prog{
    input = i,
    intCode = intCode prog,
    progState = progState prog,
    ip = ip prog,
    output = output prog
}

consOutput :: Int -> Prog -> Prog
consOutput o prog = Prog{
    input = input prog,
    intCode = intCode prog,
    progState = progState prog,
    ip = ip prog,
    output = o:(output prog)
}

stepProg :: Prog -> Prog
stepProg prog = runCode (getCode prog) prog

getCode prog = do
  code <- (intCode prog) Safe.!! (ip prog)
  parseOpCode code

runCode (Just (m1, m2, One)) = code1 m1 m2
runCode (Just (m1, m2, Two)) = code2 m1 m2
runCode (Just (_, _, Three)) = code3

runCode (Just (m, _, Four)) = code4 m
runCode (Just (m1, m2, Five)) = code5 m1 m2
runCode (Just (m1, m2, Six)) = code6 m1 m2
runCode (Just (m1, m2, Seven)) = code7 m1 m2
runCode (Just (m1, m2, Eight)) = code8 m1 m2
runCode (Just (_, _, NinetyNine)) = code99
runCode Nothing = codeX

code1 :: ParamMode -> ParamMode -> Prog -> Prog
code1 mode1 mode2 prog = case op1 mode1 mode2 (ip prog) . intCode $ prog of
  Nothing -> crashProg prog
  Just newIntCode -> movePointer 4 . updateIntCode newIntCode $ prog

code2 :: ParamMode -> ParamMode -> Prog -> Prog
code2 mode1 mode2 prog = case op2 mode1 mode2 (ip prog) . intCode $ prog of
  Nothing -> crashProg prog
  Just newIntCode -> movePointer 4 . updateIntCode newIntCode $ prog

applyOp3 prog = do
  i <- Safe.head (input prog)
  op3 i (ip prog) (intCode prog)

code3 :: Prog -> Prog
code3 prog = case Safe.head (input prog) of
  Nothing -> awaitInputProg prog
  Just inputValue -> case op3 inputValue (ip prog) (intCode prog) of
    Nothing -> crashProg prog
    Just newIntCode -> movePointer 2 . tailInput . updateIntCode newIntCode $ prog

code4 :: ParamMode -> Prog -> Prog
code4 mode prog = case op4 mode (ip prog) . intCode $ prog of
  Nothing -> crashProg prog
  Just newOutput -> movePointer 2 . consOutput newOutput $ prog

code5 :: ParamMode -> ParamMode -> Prog -> Prog
code5 mode1 mode2 prog = case op5 mode1 mode2 (ip prog) . intCode $ prog of
  Nothing -> movePointer 3 prog
  Just newIp -> if newIp > 0 && newIp < length (intCode prog)
    then setPointer newIp prog
    else crashProg prog

code6 :: ParamMode -> ParamMode -> Prog -> Prog
code6 mode1 mode2 prog = case op6 mode1 mode2 (ip prog) . intCode $ prog of
  Nothing -> movePointer 3 prog
  Just newIp -> if newIp > 0 && newIp < length (intCode prog)
    then setPointer newIp prog
    else crashProg prog

code7 :: ParamMode -> ParamMode -> Prog -> Prog
code7 mode1 mode2 prog = case op7 mode1 mode2 (ip prog) . intCode $ prog of
  Nothing -> crashProg prog
  Just newIntCode -> movePointer 4 . updateIntCode newIntCode $ prog

code8 :: ParamMode -> ParamMode -> Prog -> Prog
code8 mode1 mode2 prog = case op8 mode1 mode2 (ip prog) . intCode $ prog of
  Nothing -> crashProg prog
  Just newIntCode -> movePointer 4 . updateIntCode newIntCode $ prog

code99 :: Prog -> Prog
code99 = endProg

codeX :: Prog -> Prog
codeX = crashProg

op1 = opBinary (+)
op2 = opBinary (*)
op7 = opBinary (\p1 p2 -> if p1 < p2 then 1 else 0)
op8 = opBinary (\p1 p2 -> if p1 == p2 then 1 else 0)

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

op5 = opJumpIf (/=0)
op6 = opJumpIf (==0)

opJumpIf :: (Int -> Bool) -> ParamMode -> ParamMode -> Int -> [Int] -> Maybe Int
opJumpIf condition mode1 mode2 idx xs = do
  p1 <- getParam mode1 (idx + 1) xs
  p2 <- getParam mode2 (idx + 2) xs
  if condition p1 then Just p2 else Nothing

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

data OpCode = One | Two | Three | Four | Five | Six | Seven | Eight | NinetyNine deriving(Show)

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
  | num == 5 = Just (Pos, Pos, Five)
  | num == 105 = Just (Imm, Pos, Five)
  | num == 1005 = Just (Pos, Imm, Five)
  | num == 1105 = Just (Imm, Imm, Five)
  | num == 6 = Just (Pos, Pos, Six)
  | num == 106 = Just (Imm, Pos, Six)
  | num == 1006 = Just (Pos, Imm, Six)
  | num == 1106 = Just (Imm, Imm, Six)
  | num == 7 = Just (Pos, Pos, Seven)
  | num == 107 = Just (Imm, Pos, Seven)
  | num == 1007 = Just (Pos, Imm, Seven)
  | num == 1107 = Just (Imm, Imm, Seven)
  | num == 8 = Just (Pos, Pos, Eight)
  | num == 108 = Just (Imm, Pos, Eight)
  | num == 1008 = Just (Pos, Imm, Eight)
  | num == 1108 = Just (Imm, Imm, Eight)
  | num == 99 = Just (Pos, Pos, NinetyNine)
  | otherwise = Nothing

