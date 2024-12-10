{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day10 (Input, filename, parser, solve1, solve2) where

import Data.Char (digitToInt)
import qualified Data.List as L
import qualified Data.Map as M
import Text.Trifecta (Parser)

import Parser (parseGrid2)

type Input = M.Map (Int, Int) Int

filename :: String
filename = "data/day10.txt"

-- ===========================================================================
-- Parser

parser :: Parser Input
parser = do
    xs <- parseGrid2
    pure $ M.map digitToInt xs

-- ===========================================================================
-- Part 1

-- A track is a sequence of points starting from a zero, and increases by exactly 1 on each step until it reaches 9.
-- A step goes north, south, west or east, but not diagonally.

solve1 :: Input -> Int
solve1 xs = 
    sum scs
  where
    zeros = map fst $ M.toList $ M.filter (== 0) xs
    paths = map (scores1 xs 0) zeros
    upaths = map L.nub paths
    scs = map length upaths

scores1 :: Input -> Int -> (Int, Int) -> [(Int, Int)]
scores1 _  9 p = 
    [p]
scores1 xs n p = 
    concat $ map (scores1 xs (n + 1)) nexts
  where
    nexts = filter (heightIs xs (n + 1)) (getSteps p)

getSteps :: (Int, Int) -> [(Int, Int)]
getSteps (x,y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

heightIs :: Input -> Int -> (Int, Int) -> Bool
heightIs xs n p = case M.lookup p xs of
                    Nothing -> False
                    (Just h) -> h == n

-- ===========================================================================
-- Part 2

solve2 :: Input -> Int
solve2 xs = 
    sum paths
  where
    zeros = map fst $ M.toList $ M.filter (== 0) xs
    paths = map (scores2 xs 0) zeros

scores2 :: Input -> Int -> (Int, Int) -> Int
scores2 _  9 _ = 1
scores2 xs n p =
    sum $ map (scores2 xs (n + 1)) nexts
  where
    nexts = filter (heightIs xs (n + 1)) (getSteps p)
