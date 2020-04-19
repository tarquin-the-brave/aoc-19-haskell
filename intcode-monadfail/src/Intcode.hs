module Intcode
  ( currentOpCode
  , module Intcode.Data
  , module Intcode.Operations
  ) where

import Intcode.Data
import Intcode.Operations

currentOpCode :: MonadFail m => Intcode -> m (OpCode, ParamMode, ParamMode, ParamMode)
currentOpCode ic = do
  c <- (code ic) !!! (ip ic)
  parseModesCode (rb ic) c

