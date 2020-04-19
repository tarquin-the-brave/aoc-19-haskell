module Intcode.Operations
 ( op1
 , op2
 , op3
 , op4
 , op5
 , op6
 , op7
 , op8
 , ParamMode (..)
 , OpCode (..)
 , parseModesCode
 , (!!!)
 ) where

op1 :: MonadFail m => ParamMode -> ParamMode -> ParamMode -> Int -> [Int] -> m [Int]
op2 :: MonadFail m => ParamMode -> ParamMode -> ParamMode -> Int -> [Int] -> m [Int]
op7 :: MonadFail m => ParamMode -> ParamMode -> ParamMode -> Int -> [Int] -> m [Int]
op8 :: MonadFail m => ParamMode -> ParamMode -> ParamMode -> Int -> [Int] -> m [Int]

op1 = opBinary (+)
op2 = opBinary (*)
op7 = opBinary (\p1 p2 -> if p1 < p2 then 1 else 0)
op8 = opBinary (\p1 p2 -> if p1 == p2 then 1 else 0)

opBinary :: MonadFail m => (Int -> Int -> Int) -> ParamMode -> ParamMode -> ParamMode -> Int -> [Int] -> m [Int]
opBinary f mode1 mode2 mode3 idx xs = do
  n <- xs !!! (idx + 3)
  p1 <- getParam mode1 (idx + 1) xs
  p2 <- getParam mode2 (idx + 2) xs
  case mode3 of
    Rel base -> replaceNth (n+base) (f p1 p2) xs
    _ -> replaceNth n (f p1 p2) xs

op3 :: MonadFail m => Int -> ParamMode -> Int -> [Int] -> m [Int]
op3 i mode idx xs = do
  n <- xs !!! (idx + 1)
  case mode of
    Rel base -> replaceNth (n+base) i xs
    _ -> replaceNth n i xs

op4 :: MonadFail m => ParamMode -> Int -> [Int] -> m Int
op4 mode idx xs = getParam mode (idx + 1) xs

op5 :: MonadFail m => ParamMode -> ParamMode -> Int -> [Int] -> m Int
op6 :: MonadFail m => ParamMode -> ParamMode -> Int -> [Int] -> m Int

op5 = opJumpIf (/=0)
op6 = opJumpIf (==0)

opJumpIf :: MonadFail m => (Int -> Bool) -> ParamMode -> ParamMode -> Int -> [Int] -> m Int
opJumpIf condition mode1 mode2 idx xs = do
  p1 <- getParam mode1 (idx + 1) xs
  p2 <- getParam mode2 (idx + 2) xs
  if condition p1 then return p2 else fail "Condition not satisfied"

replaceNth :: Integral a => MonadFail m => Int -> a -> [a] -> m [a]
replaceNth n newVal xs
  | n < 0 = fail "Cannot replace element with negative index"
  | n < length xs = return $ replaceNthInner n newVal xs
  | n == length xs = return $ xs ++ [newVal]
  | n > length xs = return $ xs ++ [0 | _ <- [1..(n - length xs)]] ++ [newVal]
  | otherwise = fail "compiler told me this was missing, not sure what isn't covered above..."

replaceNthInner :: Int -> a -> [a] -> [a]
replaceNthInner _ _ [] = []
replaceNthInner n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNthInner (n-1) newVal xs

(!!!) :: Integral a => MonadFail m => [a] -> Int -> m a
infixl 9 !!!
xs !!! i
  | i < 0 = fail $ "index " ++ show i ++ " less that zero"
  | i >= length xs = return 0
  | otherwise = return $ xs !! i

getParam :: MonadFail m => ParamMode -> Int -> [Int] -> m Int
getParam Pos idx xs = do
  p <- xs !!! idx
  xs !!! p
getParam Imm idx xs = xs !!! idx
getParam (Rel base) idx xs = do
  p <- xs !!! idx
  xs !!! (p + base)

data OpCode = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | NinetyNine deriving(Show)

parseModesCode :: MonadFail m => Int -> Int -> m (OpCode, ParamMode, ParamMode, ParamMode)
parseModesCode relbase num = do
  op <- parseOpCode $ num `mod` 100
  m1 <- parseParamMode relbase $ num `div` 100 `mod` 10
  m2 <- parseParamMode relbase $ num `div` 1000 `mod` 10
  m3 <- parseParamMode relbase $ num `div` 10000 `mod` 10
  return (op, m1, m2, m3)

parseOpCode :: MonadFail m => Int -> m OpCode
parseOpCode num
  | num == 1 = return One
  | num == 2 = return Two
  | num == 3 = return Three
  | num == 4 = return Four
  | num == 5 = return Five
  | num == 6 = return Six
  | num == 7 = return Seven
  | num == 8 = return Eight
  | num == 9 = return Nine
  | num == 99 = return NinetyNine
  | otherwise = fail "not an opcode"

data ParamMode = Pos | Imm | Rel Int deriving(Show)

parseParamMode :: MonadFail m => Int -> Int -> m ParamMode
parseParamMode relbase num
  | num == 0 = return Pos
  | num == 1 = return Imm
  | num == 2 = return $ Rel relbase
  | otherwise = fail "Not a param mode"

