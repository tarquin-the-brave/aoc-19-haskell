module IntcodeProgram
  ( new
  , run
  , Prog (..)
  , Intcode
  -- Intcode Lenses
  , output
  ) where

import Intcode
import Program
import Lens.Micro.Platform (over, set, view)
import qualified Data.Sequence as S

new :: [Int] -> [Int] -> Prog Intcode
new ic = return . newIC (S.fromList ic)

run :: Prog Intcode -> Prog Intcode
run (Running ic) = run . runInstruction $ (ic)
run (AwaitInput ic) =  if (length $ view input ic) > 0
     then run . runInstruction $ (ic)
     else AwaitInput ic
run (End ic) = End ic
run x = x

runInstruction :: Intcode -> Prog Intcode
runInstruction ic = do
   opcodes <- currentOpCode ic
   case opcodes of
      (One, m1, m2, m3) -> do
        newIc <- op1 m1 m2 m3 (view ip ic) (view code ic)
        return $ over ip (+4) . set code newIc $ ic

      (Two, m1, m2, m3) -> do
        newIc <- op2 m1 m2 m3 (view ip ic) (view code ic)
        return $ over ip (+4) . set code newIc $ ic

      (Three, m, _, _) -> do
        newIc <- op3 (head $ view input ic) m (view ip ic) (view code ic)
        return $ over ip (+2) . over input tail . set code newIc $ ic

      (Four, m, _, _) -> do
        newOutput <- op4 m (view ip ic) (view code ic)
        return $ over ip (+2) . over output (\out -> newOutput:out) $ ic

      (Five, m1, m2, _) -> case op5 m1 m2 (view ip ic) (view code ic) of
        Nothing -> return $ over ip (+3) ic
        Just newIp -> if newIp >= 0 && newIp < length (view code ic)
          then return $ set ip newIp ic
          else Crashed ("Can't set instruction pointer to: " ++ show newIp)

      (Six, m1, m2, _) -> case op6 m1 m2 (view ip ic) (view code ic) of
        Nothing -> return $ over ip (+3) ic
        Just newIp -> if newIp >= 0 && newIp < length (view code ic)
          then return $ set ip newIp ic
          else Crashed ("Can't set instruction pointer to: " ++ show newIp)

      (Seven, m1, m2, m3) -> do
        newIc <- op7 m1 m2 m3 (view ip ic) (view code ic)
        return $ over ip (+4) . set code newIc $ ic

      (Eight, m1, m2, m3) -> do
        newIc <- op8 m1 m2 m3 (view ip ic) (view code ic)
        return $ over ip (+4) . set code newIc $ ic

      (Nine, m, _, _) -> do
        baseChange <- op4 m (view ip ic) (view code ic)
        return $ over ip (+2) . over rb (+baseChange) $ ic

      (NinetyNine, _, _, _) -> End ic

