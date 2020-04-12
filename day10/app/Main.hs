module Main where

-- import Lib
import qualified Display as D
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as L
import Data.Ratio as R

main :: IO ()
main = do
  -- contents <- readFile "input-example-5.txt"
  contents <- readFile "input.txt"
  let points = getPoints $ lines contents
  let pointSet = Set.fromList points
  let pointsWithAst = filter pAst points

  -- PART 1: Which asteroid do we put a monitoring station on?
  let numAstFromPoints = [((px p, py p), asteroidsSeen . raysFromPoint p $ Set.delete p pointSet) | p <- pointsWithAst ]
  let ourMax = maxSnd numAstFromPoints

  print ourMax

data Direction = PosInfinity | NegInfinity | Infront (R.Ratio Int) | Behind (R.Ratio Int) deriving(Show, Eq, Ord)
data Point = Point {px::Int, py::Int, pAst::Bool} deriving(Show, Eq, Ord)
type Rays = Map.Map Direction [Point]

getPoints :: [[Char]] -> [Point]
getPoints = L.intercalate [] . fmap (\(y, row) -> fmap (\(x, b) -> Point{px=x,py=y,pAst=b}) row) . zip [0..] . fmap (zip [0..]) . fmap (fmap (=='#'))

raysFromPoint :: Point -> Set.Set Point -> Rays
raysFromPoint p0 = foldl (\rays p -> rayInsert (directionFromPoint p0 p) [p] rays) Map.empty

directionFromPoint :: Point -> Point -> Direction
directionFromPoint p0 p
  | px p == px p0 = if (py p) > (py p0) then PosInfinity else NegInfinity
  | px p < px p0 = Behind ratio
  | otherwise = Infront ratio
  where
    ratio = (((py p) - (py p0)) R.% ((px p) - (px p0)))

rayInsert :: Direction -> [Point] -> Rays -> Rays
rayInsert = Map.insertWith (\p ps -> p ++ ps)

asteroidsSeen :: Rays -> Int
asteroidsSeen = Map.size . Map.filter pointsHaveAst

pointsHaveAst :: [Point] -> Bool
pointsHaveAst = any id . fmap pAst

maxSnd :: [(D.Coord , Int)] -> (D.Coord, Int)
maxSnd = foldl (\acc x -> if snd x > snd acc then x else acc) ((0,0), 0)


-- Got into a right kurfuffle trying to manage verticals as Infinity from Fractional
-- Implemented my own instead.
--
-- data Num a => Point a = Point {px::a, py::a, pAst::Bool} deriving(Show, Eq, Ord)
-- getPoints :: Num a => Enum a => [[Char]] -> [Point a]
-- raysFromPoint :: Eq d => Num a => Fractional d => Point a -> Set.Set (Point a) -> Rays d a
-- raysFromPoint p0 = foldl (\rays p -> rayInsert (fractionalDir p0 p) [p] rays) Map.empty
-- fractionalDir :: Fractional a => Fractional d => Point a -> Point a -> d
-- fractionalDir p0 p = ((py p) - (py p0))/((px p) - (px p0))
-- rayInsert :: Eq d => Num a => Fractional d => d -> [Point a] -> Rays d a -> Rays d a
-- rayInsert = Map.insertWith (\p ps -> p ++ ps)
-- asteroidsSeen :: Num a => Rays d a -> Int
-- pointsHaveAst :: Num a => [Point a] -> Bool
