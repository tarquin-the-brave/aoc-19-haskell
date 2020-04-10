-- Module for holding data on the whereabouts of things
-- in a 2D space and printing them to screen.
module Display
  ( DisplayOf
  , edit
  , grid
  , getDefault
  , member
  , Coord
  , Direction (..)
  , coordMove
  , turnLeft
  , turnRight
  , goBack
  ) where

import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)

type Coord = (Int,Int)
type DisplayOf = HM.HashMap Coord

edit :: DisplayOf v -> [(Coord, v)] -> DisplayOf v
edit d = foldl(\acc x-> HM.insert (fst x) (snd x) acc) d

getDefault :: (Eq k, Hashable k) => v -> k -> HM.HashMap k v -> v
getDefault = HM.lookupDefault

member :: Coord -> DisplayOf a -> Bool
member = HM.member

-- Given a rule for getting values from your display, i.e. what value
-- to give when a coordinate is not in the display, **and** a rule
-- for converting the values to Chars, produce a girdof chars.
grid :: (Coord -> DisplayOf v -> v) -> (v -> Char) -> DisplayOf v -> [[Char]]
grid getV charFrom d = do
  y <- [minY..maxY]
  return $ do
    x <- [minX..maxX]
    return . charFrom $ getV (x,y) d
  where
    (minX, maxX) = (\k -> (minimum k, maximum k)) . fmap fst . HM.keys $ d
    (minY, maxY) = (\k -> (minimum k, maximum k)) . fmap snd . HM.keys $ d

data Direction = U | D | L | R deriving(Show)

coordMove :: Direction -> Coord -> Coord
coordMove U (x,y) = (x, y+1)
coordMove D (x,y) = (x, y-1)
coordMove L (x,y) = (x-1, y)
coordMove R (x,y) = (x+1, y)

turnLeft :: Direction -> Direction
turnLeft U = L
turnLeft D = R
turnLeft L = D
turnLeft R = U

turnRight :: Direction -> Direction
turnRight U = R
turnRight D = L
turnRight L = U
turnRight R = D

goBack :: Direction -> Direction
goBack U = D
goBack D = U
goBack L = R
goBack R = L
