#!/usr/bin/env stack
-- stack --resolver lts-15.4 script --package mtl --package containers --package diagrams-lib --package bytestring --package text --package vector --package statistics
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Monad.State
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import           Diagrams.Angle (Angle, atan2A)
import qualified Data.ByteString          as B
import qualified Data.Text.Encoding       as TE
import qualified Data.Text                as T
import qualified Data.Vector              as V
import qualified Statistics.Function      as Stats

main :: IO ()
main = do
  contents <- fmap TE.decodeUtf8 . B.readFile $ "day10-input.txt"
  let points = getPoints . V.fromList . T.lines $ contents
  let pointSet = Set.fromList . V.toList $ points
  let pointsWithAst = V.filter pAst points

  -- PART 1: Which asteroid do we put a monitoring station on?
  let numAstFromPoints = fmap (\p -> ((px p, py p), asteroidsSeen . raysFromPoint p $ Set.delete p pointSet)) pointsWithAst
  let ourMax = maxSnd numAstFromPoints

  print ourMax

  -- PART 2: Spinning lazer
  let ourPoint = Point { px = fst $ fst ourMax, py = snd $ fst ourMax, pAst = True }
  let ourRaysWithAsteroids = Map.filter (/=V.empty) . Map.map (V.filter pAst) . raysFromPoint ourPoint $ Set.delete ourPoint pointSet
  let raysSorted = sortRayPoints ourPoint ourRaysWithAsteroids

  print $ Map.size raysSorted
  -- print raysSorted
  print $ vapourize 200 raysSorted


data Point = Point {px :: !Int, py :: !Int, pAst :: !Bool} deriving(Show, Eq, Ord)
type Rays = Map.Map (Angle Float) (V.Vector Point)

getPoints :: V.Vector T.Text -> V.Vector Point
getPoints = V.concat . V.toList . fmap (\(y, row) -> fmap (\(x, b) -> Point{px=x,py=y,pAst=b}) row) . V.indexed . fmap (V.indexed . fmap (=='#') . V.fromList . T.unpack)

raysFromPoint :: Point -> Set.Set Point -> Rays
raysFromPoint p0 = Set.foldl (\rays p -> Map.insertWith (V.++) (angleFromPoints p0 p) (V.singleton p) rays) Map.empty

angleFromPoints :: Point -> Point -> Angle Float
angleFromPoints p0 p = fmap (*(-1)) $ atan2A (fromIntegral ((px p) - (px p0))) (fromIntegral ((py p) - (py p0)))

asteroidsSeen :: Rays -> Int
asteroidsSeen = Map.size . Map.filter (any pAst)

maxSnd :: V.Vector ((Int, Int), Int) -> ((Int, Int), Int)
maxSnd = V.foldl1 (\acc x -> if snd x > snd acc then x else acc)

sortRayPoints :: Point -> Rays -> Rays
sortRayPoints p0 = Map.map $ Stats.sortBy (\p1 p2-> compare (manmod p2) (manmod p1))
  where
    manmod p = ((px p) - (px p0)) + ((py p) - (py p0))

shootInner :: Int -> Rays -> ((Point, Int), Rays)
shootInner count rays = ((asteroid, count + 1), newRays)
  where
    idx = (count `mod` (Map.size rays))
    (angle, ray) = Map.elemAt idx rays
    asteroid = V.head ray
    remainingRay = V.tail ray
    newRays = Map.insert angle remainingRay rays

shoot :: Int -> State Rays (Point, Int)
shoot = state . shootInner

shootUntil :: Int -> Int -> State Rays Point
shootUntil imax i = do
  (point, shot) <- shoot i
  if shot == imax then
    return point
  else
    shootUntil imax shot

vapourize :: Int -> Rays -> Point
vapourize n = evalState (shootUntil (n) 0)

