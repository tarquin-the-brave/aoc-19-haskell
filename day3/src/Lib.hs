module Lib
    ( Direction(U,D,L,R)
    , Translation(Translation)
    , pathCrossings
    ) where

import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Data to hold our coordinates for the points a wire passes through.
-- The direction of the wire is important as wire can only cross at 90 degrees.
data CrossDirection = Vertical | Horizontal deriving(Eq,Show)
data Point = Point { x::Int, y::Int, direction::CrossDirection } deriving(Show)

cross ::  Point -> Point -> Bool
cross a b = (x a == x b) && (y a == y b) && (direction a /= direction b)

-- The input data to our program is in the form of translation data: R6, U2, etc.
data Direction = U | D | L | R deriving(Show)
data Translation = Translation Direction Int deriving(Show)

-- The list of translations given by the input define the path a wire follows,
-- we can build the wires with a function that builds up a "snail trail" of
-- Points following these translations.
--
-- We can get a section of wire leading from the point we give.
-- The last point in the produced section can go with the next translation
-- to get the next section.
section :: Point -> Translation -> [Point]
section point (Translation d mod) = take mod $ infiniteSection point d

-- Create an infinite list of points in a given direction from a point.
infiniteSection :: Point -> Direction -> [Point]
infiniteSection Point{x=x, y=y, direction=_} U =
  [Point{x=x, y=y+a, direction= Vertical}| a <- [1,2..]]
infiniteSection Point{x=x, y=y, direction=_} D =
  [Point{x=x, y=y-a, direction= Vertical}| a <- [1,2..]]
infiniteSection Point{x=x, y=y, direction=_} L =
  [Point{x=x-a, y=y, direction= Horizontal}| a <- [1,2..]]
infiniteSection Point{x=x, y=y, direction=_} R =
  [Point{x=x+a, y=y, direction= Horizontal}| a <- [1,2..]]

-- To build a wire, we take a starting Point and list of translations
-- and create segments, with the last element in the segment being the
-- starting Point for the next segment.
wire :: Point -> [Translation] -> [Point]
wire point path  = foldl wireExtend [point] path

wireExtend :: [Point] -> Translation -> [Point]
wireExtend w t = w ++ section (last w) t

wireFromOrigin :: [Translation] -> [Point]
wireFromOrigin = wire Point{x=0, y=0, direction=Vertical}

-- Wires cross when they contain Points that cross.
wireCrossings :: [Point] -> [Point] -> [(Point, Point)]
wireCrossings w1 w2 = filter (\(x, y) -> cross x y) ((\x y-> (x, y)) <$> w1 <*> w2)

-- Pull out the coordinates of crossing points as tuples,
-- so we don't need to expose the Point data aggregate
-- outside this module.
wireCrossCoordinates :: [Point] -> [Point] -> [(Int, Int)]
wireCrossCoordinates w1 w2 = map (\(p, _)-> (x p, y p)) $ wireCrossings w1 w2

-- Now express this data in terms of the paths that
-- are the input to our program
pathCrossings :: [Translation] -> [Translation] -> [(Int, Int)]
pathCrossings p1 p2 = wireCrossCoordinates (wireFromOrigin p1) (wireFromOrigin p2)
