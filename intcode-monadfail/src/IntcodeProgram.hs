module IntcodeProgram
  ( new
  , run
  , Prog (..)
  , Intcode (..)
  ) where

import Intcode
import Program

new :: [Int] -> [Int] -> Prog Intcode
new ic i = return $ newIC ic i

run :: Prog Intcode -> Prog Intcode
run (Running ic) = run . runInstruction $ (ic)
run (AwaitInput ic) =  if (length . input $ ic) > 0
     then run . runInstruction $ (ic)
     else AwaitInput ic
run (End ic) = End ic
run x = x

runInstruction :: Intcode -> Prog Intcode
runInstruction ic = do
   opcodes <- currentOpCode ic
   case opcodes of
      (One, m1, m2, m3) -> do
        newIc <- op1 m1 m2 m3 (ip ic) (code ic)
        return $ moveIp 4 . updateCode newIc $ ic

      (Two, m1, m2, m3) -> do
        newIc <- op2 m1 m2 m3 (ip ic) (code ic)
        return $ moveIp 4 . updateCode newIc $ ic

      (Three, m, _, _) -> do
        newIc <- op3 (head $ input ic) m (ip ic) (code ic)
        return $ moveIp 2 . tailInput . updateCode newIc $ ic

      (Four, m, _, _) -> do
        newOutput <- op4 m (ip ic) (code ic)
        return $ moveIp 2 . consOutput newOutput $ ic

      (Five, m1, m2, _) -> case op5 m1 m2 (ip ic) (code ic) of
        Nothing -> return $ moveIp 3 ic
        Just newIp -> if newIp >= 0 && newIp < length (code ic)
          then return $ setIp newIp ic
          else Crashed ("Can't set instruction pointer to: " ++ show newIp)

      (Six, m1, m2, _) -> case op6 m1 m2 (ip ic) (code ic) of
        Nothing -> return $ moveIp 3 ic
        Just newIp -> if newIp >= 0 && newIp < length (code ic)
          then return $ setIp newIp ic
          else Crashed ("Can't set instruction pointer to: " ++ show newIp)

      (Seven, m1, m2, m3) -> do
        newIc <- op7 m1 m2 m3 (ip ic) (code ic)
        return $ moveIp 4 . updateCode newIc $ ic

      (Eight, m1, m2, m3) -> do
        newIc <- op8 m1 m2 m3 (ip ic) (code ic)
        return $ moveIp 4 . updateCode newIc $ ic

      (Nine, m, _, _) -> do
        baseChange <- op4 m (ip ic) (code ic)
        return $ moveIp 2 . changeRb baseChange $ ic

      (NinetyNine, _, _, _) -> End ic

