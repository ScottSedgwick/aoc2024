{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day08 (Input, filename, parser, solve1, solve2) where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import Text.Trifecta (Parser)

import Cartesian2
import Parser (parseGrid2)

type Input = Grid2

filename :: String
filename = "data/day08.txt"

parser :: Parser Input
parser = parseGrid2

solve1 :: Input -> Int
solve1 = solve mkAntinode1

solve :: (Input -> Bounds2 -> [Point2]) -> Input -> Int
solve mkAnt xs = S.size $ S.unions $ map (mkAntinodes mkAnt xs) $ getAntennae xs

getAntennae :: Input -> [[Point2]]
getAntennae xs = map (map fst) $ L.groupBy (\a b -> snd a == snd b) (L.sortOn snd ys)
  where
    ys = filter (\(_,c)-> c /= '.') (M.toList xs)
    
mkAntinodes :: (Input -> Bounds2 -> [Point2]) -> Input -> [Point2] -> S.Set Point2
mkAntinodes mkAnt ws xs = S.fromList zs
  where
    -- generate a list of all pairs of points from xs
    ys = [(x,y) | x <- xs, y <- xs, x /= y]
    -- for each pair of points, convert to antinodes
    zs = concat $ map (mkAnt ws) ys

mkAntinode1 :: Input -> Bounds2 -> [Point2]
mkAntinode1 ws ((x1, y1), (x2, y2)) = filter (inbounds bs) [(x1 - dx, y1 - dy), (x2 + dx, y2 + dy)]
  where
    bs = bounds ws
    dx = x2 - x1
    dy = y2 - y1

-- as solve1, except antenna positions are also included, and we keep going in the line until we reach the edges of the map
solve2 :: Input -> Int
solve2 = solve mkAntinode2

mkAntinode2 :: Input -> (Point2, Point2) -> [Point2]
mkAntinode2 xs ((x1, y1), (x2, y2)) = [ (x1, y1), (x2, y2) ]
    <> takeWhile (inbounds (bounds xs)) [ (x1 - dx * n, y1 - dy * n) | n <- [1..]]
    <> takeWhile (inbounds (bounds xs)) [ (x2 + dx * n, y2 + dy * n) | n <- [1..]]
  where
    dx = x2 - x1
    dy = y2 - y1