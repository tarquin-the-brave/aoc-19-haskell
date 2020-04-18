module Intcode.Data
    ( Intcode (..)
    , new
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

--
-- Intcode data
--
data Intcode = Intcode {
  input::[Int],
  code::[Int],
  -- ip: Instruction Pointer
  ip::Int,
  -- rb: Relative Base
  rb::Int,
  output::[Int]
} deriving(Show, Eq)

new :: [Int] -> [Int] -> Intcode
new newCode newInput = Intcode{
  input = newInput,
  code = newCode,
  ip = 0,
  rb = 0,
  output = []
}

moveIp :: Int -> Intcode -> Intcode
moveIp i ic = setIp (i + ip ic) ic

setIp :: Int -> Intcode -> Intcode
setIp i ic = Intcode{
    input = input ic,
    code = code ic,
    ip = i,
    rb = rb ic,
    output = output ic
}

changeRb :: Int -> Intcode -> Intcode
changeRb b ic = Intcode{
    input = input ic,
    code = code ic,
    ip = ip ic,
    rb = (rb ic) + b,
    output = output ic
}

updateCode :: [Int] -> Intcode -> Intcode
updateCode newIntCode ic = Intcode{
    input = input ic,
    code = newIntCode,
    ip = ip ic,
    rb = rb ic,
    output = output ic
}

-- unsafe
tailInput :: Intcode -> Intcode
tailInput ic = Intcode{
    input = tail $ input ic,
    code = code ic,
    ip = ip ic,
    rb = rb ic,
    output = output ic
}

setInput :: [Int] -> Intcode -> Intcode
setInput i ic = Intcode{
    input = i,
    code = code ic,
    ip = ip ic,
    rb = rb ic,
    output = output ic
}

consInput :: Int -> Intcode -> Intcode
consInput i ic = Intcode{
    input = i:(input ic),
    code = code ic,
    ip = ip ic,
    rb = rb ic,
    output = output ic
}

appendInput :: Int -> Intcode -> Intcode
appendInput i ic = Intcode{
    input = (input ic) ++ [i],
    code = code ic,
    ip = ip ic,
    rb = rb ic,
    output = output ic
}

setOutput :: [Int] -> Intcode -> Intcode
setOutput o ic = Intcode{
    input = input ic,
    code = code ic,
    ip = ip ic,
    rb = rb ic,
    output = o
}

consOutput :: Int -> Intcode -> Intcode
consOutput o ic = Intcode{
    input = input ic,
    code = code ic,
    ip = ip ic,
    rb = rb ic,
    output = o:(output ic)
}

scrubOutput :: Intcode -> Intcode
scrubOutput ic = Intcode{
    input = input ic,
    code = code ic,
    ip = ip ic,
    rb = rb ic,
    output = []
}


