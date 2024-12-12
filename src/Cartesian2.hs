module Cartesian2 (Bounds2, Grid2, Point2, bounds, count, find, inbounds, neighbours, north, south, east, west) where

import qualified Data.List as L
import qualified Data.Map as M

type Point2 = (Int, Int)
type Bounds2 = (Point2, Point2)

type Grid2 = M.Map Point2 Char

bounds :: M.Map Point2 a -> Bounds2
bounds xs = ((minx, miny), (maxx, maxy))
  where
    (maxx, maxy) = L.maximum (M.keys xs)
    (minx, miny) = L.minimum (M.keys xs)

count :: Grid2 -> (Char -> Bool) -> Int
count g f = M.size $ M.filter f g

find :: Grid2 -> (Char -> Bool) -> Maybe Point2
find g f = case M.toList (M.filter f g) of
                [] -> Nothing
                ((k,_):_) -> Just k

inbounds :: Bounds2 -> Point2 -> Bool
inbounds ((minx, miny), (maxx, maxy)) (x, y) = (x >= minx) && (x <= maxx) && (y >= miny) && (y <= maxy)

neighbours :: Point2 -> [Point2]
neighbours p = [east p, west p, north p, south p]

east :: Point2 -> Point2
east (x, y) = (x+1, y)

west :: Point2 -> Point2
west (x, y) = (x-1, y)

north :: Point2 -> Point2
north (x, y) = (x, y-1)

south :: Point2 -> Point2
south (x, y) = (x, y+1)