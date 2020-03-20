module Lib
    ( someFunc
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
wire = undefined

-- Wires cross when they contain Points that cross.
wireCrossings :: [Point] -> [Point] -> [Point]
wireCrossings = undefined
