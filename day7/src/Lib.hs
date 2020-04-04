module Lib
    ( newProg
    , runProg
    , Prog (..)
    , ProgState (..)
    , amplifyerPart1
    , Amps (..)
    , newAmps
    , runAmpsLoop
    -- for debugging
    , runActiveAmpOutput
    , runActiveAmp
    , atFinalAmp
    , nextAmp
    , activeAmpSetInput
    , activeAmpScrubOutput
    , activeAmpAppendInput
    , activeAmpStatus) where

import Intcode
import Control.Monad.State.Lazy

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

--
-- exported functions
--
data Amps = Amps { ampsOf::[Prog], activeAmp::Int } deriving(Show)

newAmps :: [Int] -> [Int] -> Amps
newAmps intcode phases = activeAmpAppendInput 0 Amps {
  ampsOf = fmap (\p-> newProg intcode [p]) phases,
  activeAmp = 0
}

runAmpsLoop :: State Amps (Maybe Int)
runAmpsLoop = do
  out <- runActiveAmpOutput'
  amps <- get
  case (atFinalAmp amps, activeAmpStatus amps) of
    (_, TerminatedBadly) -> return Nothing
    (_, Running) -> return Nothing
    (True, Terminated) -> return (Just out)
    (False, Terminated) -> do
      modify nextAmp
      modify (activeAmpAppendInput out)
      runAmpsLoop
    (_, AwaitInput) -> do
      modify nextAmp
      modify (activeAmpAppendInput out)
      runAmpsLoop

--
-- Internal functions
--
nextAmp :: Amps -> Amps
nextAmp amps = Amps {
  ampsOf = ampsOf amps,
  activeAmp = (activeAmp amps + 1) `mod` (length $ ampsOf amps)
}

atFinalAmp :: Amps -> Bool
atFinalAmp amps = (activeAmp amps) == ((length . ampsOf $ amps) - 1)

-- Functions on The Active Amps
--
runActiveAmpOutput' :: State Amps Int
runActiveAmpOutput' = state runActiveAmpOutput

runActiveAmpOutput :: Amps -> (Int, Amps)
runActiveAmpOutput amps = (head . output . getActiveAmp $ newAmps, activeAmpScrubOutput newAmps)
  where newAmps = runActiveAmp amps

runActiveAmp :: Amps -> Amps
runActiveAmp = editActiveAmp runProg

activeAmpAppendInput :: Int -> Amps -> Amps
activeAmpAppendInput newIn = editActiveAmp (appendInputProg newIn)

activeAmpSetInput :: Int -> Amps -> Amps
activeAmpSetInput newIn = editActiveAmp (setInputProg [newIn])

activeAmpConsInput :: Int -> Amps -> Amps
activeAmpConsInput newIn = editActiveAmp (consInputProg newIn)

activeAmpScrubOutput :: Amps -> Amps
activeAmpScrubOutput = editActiveAmp scrubOutput

editActiveAmp :: (Prog -> Prog) -> Amps -> Amps
editActiveAmp f amps = Amps {
  ampsOf = replaceNthInner (activeAmp amps) (f . getActiveAmp $ amps) (ampsOf amps),
  activeAmp = activeAmp amps
}

getActiveAmp :: Amps -> Prog
getActiveAmp amps = (ampsOf amps)!!(activeAmp amps)

activeAmpStatus :: Amps -> ProgState
activeAmpStatus = progState . getActiveAmp
