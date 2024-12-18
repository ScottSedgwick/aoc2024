module Cartesian2 (Bounds2, Dir(..), Grid2, Grid, Point2, bounds, count, find, inbounds, move, neighbours, north, south, east, west, showGrid) where

import qualified Data.List as L
import qualified Data.Map as M

type Point2 = (Int, Int)
type Bounds2 = (Point2, Point2)
data Dir = N | S | E | W deriving stock (Show, Eq, Ord)

type Grid a = M.Map Point2 a
type Grid2 = Grid Char

showGrid :: (Show a) => Char -> Grid a -> String
showGrid def g = L.intercalate "\n" $ map mkRow [0..y2]
  where
    (_,(x2,y2)) = bounds g
    mkRow y     = map (mkPos y) [0..x2]
    mkPos y x   = case M.lookup (x,y) g of
                    Nothing -> def
                    Just a -> hd (show a)
    hd []       = def
    hd (c:_)    = c

bounds :: Grid a -> Bounds2
bounds g = ((minX, minY), (maxX, maxY))
  where
    xs = map fst (M.keys g)
    ys = map snd (M.keys g)
    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys

count :: Grid a -> (a -> Bool) -> Int
count g f = M.size $ M.filter f g

find :: Grid a -> (a -> Bool) -> Maybe Point2
find g f = case M.toList (M.filter f g) of
                [] -> Nothing
                ((k,_):_) -> Just k

inbounds :: Bounds2 -> Point2 -> Bool
inbounds ((minx, miny), (maxx, maxy)) (x, y) = (x >= minx) && (x <= maxx) && (y >= miny) && (y <= maxy)

neighbours :: Point2 -> [Point2]
neighbours p = [east p, west p, north p, south p]

move :: Dir -> Point2 -> Point2
move E = east
move W = west
move N = north
move S = south

east :: Point2 -> Point2
east (x, y) = (x+1, y)

west :: Point2 -> Point2
west (x, y) = (x-1, y)

north :: Point2 -> Point2
north (x, y) = (x, y-1)

south :: Point2 -> Point2
south (x, y) = (x, y+1)

-- mapGrid :: Show a => Grid a -> Grid Char
-- mapGrid g = M.map (\a -> f (show a)) g
--   where
--     f "" = ' '
--     f (x:_) = x