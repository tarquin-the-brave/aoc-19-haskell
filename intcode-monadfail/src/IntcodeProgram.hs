module IntcodeProgram
  ( ) where

import Intcode
import Program

run :: Prog Intcode -> Prog Intcode
run (Running ic) = run . stepProg $ (Running ic)
run (AwaitInput ic) =  if (length . input $ ic) > 0
     then run . stepProg $ (AwaitInput ic)
     else AwaitInput ic
run (End ic) = End ic
run x = crash "Cant run a crashed program" x

stepProg :: Prog Intcode -> Prog Intcode
-- stepProg = undefined
stepProg = do
   opcode <- fmap currentOpCode
   runCode opcode

runCode :: (ParamMode, ParamMode, ParamMode, OpCode) -> Prog Intcode -> Prog Intcode
runCode (m1, m2, m3, One) = code1 m1 m2 m3
runCode (m1, m2, m3, Two) = code2 m1 m2 m3
runCode (m, _, _, Three) = code3 m
runCode (m, _, _, Four) = code4 m
runCode (m1, m2, _, Five) = code5 m1 m2
runCode (m1, m2, _, Six) = code6 m1 m2
runCode (m1, m2, m3, Seven) = code7 m1 m2 m3
runCode (m1, m2, m3, Eight) = code8 m1 m2 m3
runCode (m, _, _, Nine) = code9 m
runCode (_, _, _, NinetyNine) = code99

code1 :: ParamMode -> ParamMode -> ParamMode -> Prog Intcode -> Prog Intcode
code1 = undefined
-- code1 m1 m2 m3 = do
--   newIntcode <- fmap (\ic -> op1 m1 m2 m3 (ip ic) (code ic))
--   return $ fmap (moveIp 4 . updateCode newIntcode)
-- code1 mode1 mode2 mode3 prog = case op1 mode1 mode2 mode3 (ip prog) . intCode $ prog of
--   Nothing -> crashProg prog
--   Just newIntCode -> moveIp 4 . updateIntCode newIntCode $ prog

code2 :: ParamMode -> ParamMode -> ParamMode -> Prog Intcode -> Prog Intcode
code2 = undefined
-- code2 mode1 mode2 mode3 prog = case op2 mode1 mode2 mode3 (ip prog) . intCode $ prog of
--   Nothing -> crashProg prog
--   Just newIntCode -> moveIp 4 . updateIntCode newIntCode $ prog

code3 :: ParamMode -> Prog Intcode -> Prog Intcode
code3 = undefined
-- code3 mode prog = case Safe.head (input prog) of
--   Nothing -> awaitInputProg prog
--   Just inputValue -> case op3 inputValue mode (ip prog) (intCode prog) of
--     Nothing -> crashProg prog
--     Just newIntCode -> moveIp 2 . runningProg . tailInput . updateIntCode newIntCode $ prog

code4 :: ParamMode -> Prog Intcode -> Prog Intcode
code4 = undefined
-- code4 mode prog = case op4 mode (ip prog) . intCode $ prog of
--   Nothing -> crashProg prog
--   Just newOutput -> moveIp 2 . consOutput newOutput $ prog

code5 :: ParamMode -> ParamMode -> Prog Intcode -> Prog Intcode
code5 = undefined
-- code5 mode1 mode2 prog = case op5 mode1 mode2 (ip prog) . intCode $ prog of
--   Nothing -> moveIp 3 prog
--   Just newIp -> if newIp >= 0 && newIp < length (intCode prog)
--     then setIp newIp prog
--     else crashProg prog

code6 :: ParamMode -> ParamMode -> Prog Intcode -> Prog Intcode
code6 = undefined
-- code6 mode1 mode2 prog = case op6 mode1 mode2 (ip prog) . intCode $ prog of
--   Nothing -> moveIp 3 prog
--   Just newIp -> if newIp >= 0 && newIp < length (intCode prog)
--     then setIp newIp prog
--     else crashProg prog

code7 :: ParamMode -> ParamMode -> ParamMode -> Prog Intcode -> Prog Intcode
code7 = undefined
-- code7 mode1 mode2 mode3 prog = case op7 mode1 mode2 mode3 (ip prog) . intCode $ prog of
--   Nothing -> crashProg prog
--   Just newIntCode -> moveIp 4 . updateIntCode newIntCode $ prog

code8 :: ParamMode -> ParamMode -> ParamMode -> Prog Intcode -> Prog Intcode
code8 = undefined
-- code8 mode1 mode2 mode3 prog = case op8 mode1 mode2 mode3 (ip prog) . intCode $ prog of
--   Nothing -> crashProg prog
--   Just newIntCode -> moveIp 4 . updateIntCode newIntCode $ prog

code9 :: ParamMode -> Prog Intcode -> Prog Intcode
code9 = undefined
-- code9 mode prog = case op4 mode (ip prog) . intCode $ prog of
--   Nothing -> crashProg prog
--   Just baseChange -> moveIp 2 . changeRb baseChange $ prog

code99 :: Prog Intcode -> Prog Intcode
code99 = undefined
-- code99 = endProg

codeX :: Prog Intcode -> Prog Intcode
codeX = undefined
-- codeX = crashProg


