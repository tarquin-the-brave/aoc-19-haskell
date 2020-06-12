{-# LANGUAGE TemplateHaskell #-}
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

import Lens.Micro.Platform (makeLenses, over, set)
import qualified Data.Sequence as S

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
  } deriving(Show, Eq)

makeLenses ''Intcode

newIC :: S.Seq Int -> [Int] -> Intcode
newIC newCode newInput = Intcode{
  _input = newInput,
  _code = newCode,
  _ip = 0,
  _rb = 0,
  _output = []
}
