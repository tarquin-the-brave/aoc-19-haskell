{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Intcode.Data
    ( Intcode
    -- Intcode Lenses
    , input
    , code
    , ip
    , rb
    , output
    -- init
    , newIC
    ) where

import Lens.Micro.Platform (makeLenses, set)
import qualified Data.Sequence as S
import GHC.Generics (Generic)
import Data.Default (Default, def)

--
-- Intcode data
--
data Intcode = Intcode
  { _input::[Int]
  , _code :: S.Seq Int
  -- ip: Instruction Pointer
  , _ip::Int
  -- rb: Relative Base
  , _rb::Int
  , _output::[Int]
  }
  deriving(Generic, Default, Show, Eq)

makeLenses ''Intcode

newIC :: S.Seq Int -> [Int] -> Intcode
newIC c i = set code c . set input i $ def
