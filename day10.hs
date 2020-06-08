#!/usr/bin/env stack
-- stack --resolver lts-15.4 script --package mtl --package containers --package diagrams-lib

-- import Lib
import           Control.Monad.State.Lazy
import qualified Data.List                as L
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import           Diagrams.Angle

main :: IO ()
main = do
  contents <- readFile "day10-input.txt"
  let points = getPoints $ lines contents
  let pointSet = Set.fromList points
  let pointsWithAst = filter pAst points

  -- PART 1: Which asteroid do we put a monitoring station on?
  let numAstFromPoints = [((px p, py p), asteroidsSeen . raysFromPoint p $ Set.delete p pointSet) | p <- pointsWithAst ]
  let ourMax = maxSnd numAstFromPoints

  print ourMax

  -- PART 2: Spinning lazer
  let ourPoint = Point { px = fst $ fst ourMax, py = snd $ fst ourMax, pAst = True }
  let ourRaysWithAsteroids = Map.filter (/=[]) . Map.map (\ps -> [p|p<-ps, pAst p]) . raysFromPoint ourPoint $ Set.delete ourPoint pointSet
  let raysSorted = sortRayPoints ourPoint ourRaysWithAsteroids

  print $ Map.size raysSorted
  -- print raysSorted
  print $ vapourize 200 raysSorted


data Point = Point {px::Int, py::Int, pAst::Bool} deriving(Show, Eq, Ord)
type Rays = Map.Map (Angle Float) [Point]

getPoints :: [[Char]] -> [Point]
getPoints = L.intercalate [] . fmap (\(y, row) -> fmap (\(x, b) -> Point{px=x,py=y,pAst=b}) row) . zip [0..] . fmap (zip [0..]) . fmap (fmap (=='#'))

raysFromPoint :: Point -> Set.Set Point -> Rays
raysFromPoint p0 = foldl (\rays p -> rayInsert (angleFromPoints p0 p) [p] rays) Map.empty

angleFromPoints :: Point -> Point -> Angle Float
angleFromPoints p0 p = fmap (*(-1)) $ atan2A (fromIntegral ((px p) - (px p0))) (fromIntegral ((py p) - (py p0)))

rayInsert :: Angle Float -> [Point] -> Rays -> Rays
rayInsert = Map.insertWith (\p ps -> p ++ ps)

asteroidsSeen :: Rays -> Int
asteroidsSeen = Map.size . Map.filter pointsHaveAst

pointsHaveAst :: [Point] -> Bool
pointsHaveAst = any id . fmap pAst

maxSnd :: [((Int, Int), Int)] -> ((Int, Int), Int)
maxSnd = foldl (\acc x -> if snd x > snd acc then x else acc) ((0,0), 0)

sortRayPoints :: Point -> Rays -> Rays
sortRayPoints p0 = Map.map $ L.sortBy (\p1 p2-> compare (manmod p2) (manmod p1))
  where
    manmod p = ((px p) - (px p0)) + ((py p) - (py p0))

shoot' :: Int -> Rays -> ((Point, Int), Rays)
shoot' count rays = ((asteroid, count + 1), newRays)
  where
    idx = (count `mod` (Map.size rays))
    (angle, ray) = Map.elemAt idx rays
    asteroid = head ray
    remainingRay = tail ray
    newRays = Map.insert angle remainingRay rays

shoot :: Int -> State Rays (Point, Int)
shoot = state . shoot'

shootUntil :: Int -> Int -> State Rays Point
shootUntil imax i = do
  (point, shot) <- shoot i
  if shot == imax then
    return point
  else
    shootUntil imax shot

vapourize :: Int -> Rays -> Point
vapourize n = evalState (shootUntil (n) 0)
