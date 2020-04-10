-- In the knowledge that Lib will do a qualified import
-- of the Intcode funtions, Let's rename them here to make
-- life easier.
module IntcodeImport
  ( new
  , run
  , IC.Prog (..)
  , Status (..)
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

type Status = IC.ProgState

new = IC.newProg
run = IC.runProg
setInput = IC.setInputProg
