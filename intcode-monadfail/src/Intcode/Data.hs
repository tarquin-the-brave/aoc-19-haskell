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
    -- old code
    , moveIp
    , setIp
    , changeRb
    , updateCode
    , tailInput
    , setInput
    , consInput
    , appendInput
    , setOutput
    , consOutput
    , scrubOutput
    ) where

import Lens.Micro.Platform (makeLenses, over, set)

--
-- Intcode data
--
data Intcode = Intcode
  { _input::[Int]
  , _code::[Int]
  -- ip: Instruction Pointer
  , _ip::Int
  -- rb: Relative Base
  , _rb::Int
  , _output::[Int]
  } deriving(Show, Eq)

makeLenses ''Intcode

newIC :: [Int] -> [Int] -> Intcode
newIC newCode newInput = Intcode{
  _input = newInput,
  _code = newCode,
  _ip = 0,
  _rb = 0,
  _output = []
}

moveIp :: Int -> Intcode -> Intcode
moveIp i = over ip (+i)

setIp :: Int -> Intcode -> Intcode
setIp = set ip

changeRb :: Int -> Intcode -> Intcode
changeRb b = over rb (+b)

updateCode :: [Int] -> Intcode -> Intcode
updateCode newIntCode = set code newIntCode

-- unsafe
tailInput :: Intcode -> Intcode
tailInput = over input tail

setInput :: [Int] -> Intcode -> Intcode
setInput = set input

consInput :: Int -> Intcode -> Intcode
consInput i = over input (\inp -> i:inp)

appendInput :: Int -> Intcode -> Intcode
appendInput i = over input (\inp -> inp ++ [i])

setOutput :: [Int] -> Intcode -> Intcode
setOutput = set output

consOutput :: Int -> Intcode -> Intcode
consOutput o = over output (\out -> o:out)

scrubOutput :: Intcode -> Intcode
scrubOutput = set output []


