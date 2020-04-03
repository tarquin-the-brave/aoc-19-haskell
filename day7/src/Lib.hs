module Lib
    ( newProg
    , runProg
    , Prog (..)
    , ProgState (..)
    , amplifyerPart1) where

import Intcode
import qualified Data.List.NonEmpty as NEL

-- In part 1 we didn't car about keeping the state from running
-- each amplifyer, so we can take the output of finishedProg
-- and throw away that state.
amplifyerPart1 :: [Int] -> Int -> Int -> Maybe Int
amplifyerPart1 ampCode i phase = case progState finishedProg of
  Terminated -> if (input finishedProg == []) && ((length . output $ finishedProg) == 1)
    then Just (head $ output finishedProg)
    else Nothing
  _ -> Nothing
  where finishedProg = runProg . newProg ampCode $ [phase, i]

-- In part 2 amplifiers run in a loop till the programs terminate.
-- I think this means that the intcode computer will need a new state
-- of AwaitInput.  As the amplifiers are looped through they will
-- rest in AwaitInput state until the "final" loop.
-- Behaviour of runProg will need take account of input program not
-- being fresh.
-- Suggest a giant state monad: s -> (a, s)
-- s = [Amp]
-- a = Int
data Amp = Amp {ampProg::Prog, ampPhase::Int}

newAmp :: [Int] -> Int -> Amp
newAmp intcode phase = Amp {
  ampProg = newProg intcode [],
  ampPhase = phase
}

setInputAmp :: [Int] -> Amp -> Amp
setInputAmp newInput amp = Amp {
  ampProg = setInputProg newInput (ampProg amp),
  ampPhase = ampPhase amp
}

data Amps = Amps { amps::NEL.NonEmpty Amp, activeAmp::Int }

newAmps :: [Int] -> NEL.NonEmpty Int -> Amps
newAmps intcode phases = Amps {
  amps=fmap (newAmp intcode) phases,
  activeAmp=0
}


