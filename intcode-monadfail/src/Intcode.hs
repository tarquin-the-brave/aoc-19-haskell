module Intcode
  ( currentOpCode
  , module Intcode.Data
  , module Intcode.Operations
  ) where

import Intcode.Data
import Intcode.Operations

currentOpCode :: MonadFail m => Intcode -> m (ParamMode, ParamMode, ParamMode, OpCode)
currentOpCode ic = do
  c <- (code ic) !!! (ip ic)
  parseModesCode (rb ic) c

