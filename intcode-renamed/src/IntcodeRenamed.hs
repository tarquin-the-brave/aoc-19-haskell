module IntcodeRenamed
  ( new
  , run
  , IC.Prog (..)
  , IC.ProgState (..)
  , status
  , setInput
  , IC.scrubOutput
  ) where

import Intcode as IC
  ( newProg
  , runProg
  , Prog (..)
  , ProgState (..)
  , scrubOutput
  , setInputProg
  )

new = IC.newProg
run = IC.runProg
setInput = IC.setInputProg
status = IC.progState
