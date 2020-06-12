{-# LANGUAGE TemplateHaskell #-}
module Intcode
  ( currentOpCode
  , module Intcode.Data
  , module Intcode.Operations
  ) where

import Intcode.Data
import Intcode.Operations
import Lens.Micro.Platform (view)

currentOpCode :: MonadFail m => Intcode -> m (OpCode, ParamMode, ParamMode, ParamMode)
currentOpCode ic = do
  c <- (view code ic) !!! (view ip ic)
  parseModesCode (view rb ic) c

