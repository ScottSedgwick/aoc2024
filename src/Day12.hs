{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day12 (Input, filename, parser, solve1, solve2, getEdges) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Trifecta (Parser)

import Cartesian2
import Parser (parseGrid2)

type Input = Grid2

filename :: String
filename = "data/day12.txt"

-- ===========================================================================
-- Parser

parser :: Parser Input
parser = parseGrid2

-- ===========================================================================
-- Part 1

-- Split the grid into regions of contiguous characters.
-- Find the area of each region, and the perimeter of each region.
-- Multiply area by perimeter, and sum the total.

solve1 :: Input -> Int
solve1 xs = 
    sum (map (\(a,b) -> a * b) (zip ps as))
  where
    bs = L.nub $ M.elems xs
    cs = map (\b -> M.filter (==b) xs) bs
    ds = map (\c -> S.fromList $ map fst (M.toList c)) cs
    rs = concat $ map getRegions ds
    ps = map getPerimeter rs
    as = map getArea rs

getRegions :: S.Set Point2 -> [S.Set Point2]
getRegions ps = 
    case S.toList ps of
        [] -> []
        (x:_) -> 
            let
                (bs, cs) = buildRegion x (S.singleton x, S.delete x ps)
            in
                bs : getRegions cs

buildRegion :: Point2 -> (S.Set Point2, S.Set Point2) -> (S.Set Point2, S.Set Point2)
buildRegion x (xs, ys) = foldr (\a b -> buildRegion a b) (xs', ys') ns
  where
    ns = filter (\n -> S.member n ys) (neighbours x)
    xs' = foldr (\a b -> S.insert a b) xs ns
    ys' = foldr (\a b -> S.delete a b) ys ns

-- For each element in the set, calculate the number of neighbours it has which are not in the set.
-- Add them together to get yur perimeter
getPerimeter :: S.Set Point2 -> Int
getPerimeter ps = sum ns
  where
    ns = map (countNonNeighbours ps) (S.toList ps)

countNonNeighbours :: S.Set Point2 -> Point2 -> Int
countNonNeighbours ps p = length $ filter (\q -> S.notMember q ps) (neighbours p)

getArea :: S.Set Point2 -> Int
getArea = S.size

-- ===========================================================================
-- Part 2

solve2 :: Input -> Int
solve2 xs = 
    sum (map (\(a,b) -> a * b) (zip es as))
  where
    bs = L.nub $ M.elems xs
    cs = map (\b -> M.filter (==b) xs) bs
    ds = map (\c -> S.fromList $ map fst (M.toList c)) cs
    rs = concat $ map getRegions ds
    es = map getEdges rs
    as = map getArea rs

-- Edges == Corners!
getEdges :: S.Set Point2 -> Int
getEdges xs =
    sum cs
  where
    cs = map (corners xs) (S.toList xs)

corners :: S.Set Point2 -> Point2 -> Int
corners ps p = length $ filter id [out_ne, out_nw, out_se, out_sw, in_ne, in_nw, in_se, in_sw]
  where
    e = S.notMember (east p) ps
    ne = S.notMember (north (east p)) ps
    w = S.notMember (west p) ps
    nw = S.notMember (north (west p)) ps
    s = S.notMember (south p) ps
    se = S.notMember (south (east p)) ps
    n = S.notMember (north p) ps
    sw = S.notMember (south (west p)) ps
    out_ne = e && n
    out_nw = w && n
    out_se = s && e
    out_sw = s && w
    in_ne = not n && not e && ne
    in_nw = not n && not w && nw
    in_se = not s && not e && se
    in_sw = not s && not w && sw

