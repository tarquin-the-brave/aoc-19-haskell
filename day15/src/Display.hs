-- Module for holding data on the whereabouts of things
-- in a 2D space and printing them to screen.
module Display
  ( DisplayOf
  , Coord
  , edit
  , grid
  , getDefault
  ) where

import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)

type Coord = (Int,Int)
type DisplayOf = HM.HashMap Coord

edit :: DisplayOf v -> [(Coord, v)] -> DisplayOf v
edit d = foldl(\acc x-> HM.insert (fst x) (snd x) acc) d

getDefault :: (Eq k, Hashable k) => v -> k -> HM.HashMap k v -> v
getDefault = HM.lookupDefault

-- Given a rule for getting values from your display, i.e. what value
-- to give when a coordinate is not in the display, **and** a rule
-- for converting the values to Chars, produce a girdof chars.
grid :: DisplayOf v -> (Coord -> DisplayOf v -> v) -> (v -> Char) -> [[Char]]
grid d getV charFrom = do
  y <- [minY..maxY]
  return $ do
    x <- [minX..maxX]
    return . charFrom $ getV (x,y) d
  where
    (minX, maxX) = (\k -> (minimum k, maximum k)) . fmap fst . HM.keys $ d
    (minY, maxY) = (\k -> (minimum k, maximum k)) . fmap snd . HM.keys $ d
