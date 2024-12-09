module Cartesian2 (Bounds2, Grid2, Point2, bounds, count, find, inbounds) where

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