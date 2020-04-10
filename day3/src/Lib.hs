module Lib
    ( D.Direction(..)
    , Translation(..)
    , Path
    , pathCrossings
    , sumWireLength
    ) where

import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Display as D
import Control.Monad

data Translation = Translation D.Direction Int deriving(Show)
type Path = [Translation]

pathCrossings :: Path -> Path -> [D.Coord]
pathCrossings p1 p2 = HS.toList $ HS.intersection (points p1) (points p2)

points :: Path -> HS.HashSet D.Coord
points = snd . L.foldl addPoints ((0,0), HS.empty)

type Acc = (D.Coord, HS.HashSet D.Coord)
addPoints :: Acc -> Translation -> Acc
addPoints (lp, ps) (Translation d n) = foldl (addPoint d) (lp, ps) [1..n]

addPoint :: D.Direction -> Acc -> Int -> Acc
addPoint d (lp, ps) _ = (newPoint, HS.insert newPoint ps)
  where newPoint = D.coordMove d lp

sumWireLength :: Path -> Path -> D.Coord -> Maybe Int
sumWireLength path1 path2 point = do
  l1 <- lineIntegral point path1
  l2 <- lineIntegral point path2
  return (l1 + l2)

lineIntegral :: D.Coord -> Path -> Maybe Int
lineIntegral point path = case foldM (f point) ((0,0), 0) path of
  Left (_, len) -> Just len
  _ -> Nothing

f :: D.Coord -> (D.Coord, Int) -> Translation -> Either (D.Coord, Int) (D.Coord, Int)
f endP (lp, tot) (Translation d n) = foldM (f' endP) (lp, tot) $ fmap (\i -> D.coordMoveN i d lp) [1..n]

f' :: D.Coord -> (D.Coord, Int) -> D.Coord -> Either (D.Coord, Int) (D.Coord, Int)
f' endP (_, tot) curP = if curP == endP then Left (curP, tot + 1) else Right (curP, tot + 1)

