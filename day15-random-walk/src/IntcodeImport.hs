-- In the knowledge that Lib will do a qualified import
-- of the Intcode funtions, Let's rename them here to make
-- life easier.
module IntcodeImport
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
