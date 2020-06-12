{-# LANGUAGE TemplateHaskell #-}
module Intcode
  ( currentOpCode
  , module Intcode.Data
  , module Intcode.Operations
  -- , Intcode
  -- , input
  -- , code
  -- , ip
  -- , rb
  -- , output
  ) where

import Intcode.Data
import Intcode.Operations
import Lens.Micro.Platform (view)

currentOpCode :: MonadFail m => Intcode -> m (OpCode, ParamMode, ParamMode, ParamMode)
currentOpCode ic = do
  c <- (view code ic) !!! (view ip ic)
  parseModesCode (view rb ic) c

-- data Intcode = Intcode
--   { _input::[Int]
--   , _code::[Int]
--   -- ip: Instruction Pointer
--   , _ip::Int
--   -- rb: Relative Base
--   , _rb::Int
--   , _output::[Int]
--   } deriving(Show, Eq)
--
-- makeLenses''
-- newIC :: [Int] -> [Int] -> Intcode
-- newIC newCode newInput = Intcode{
--   input = newInput,
--   code = newCode,
--   ip = 0,
--   rb = 0,
--   output = []
-- }
--
