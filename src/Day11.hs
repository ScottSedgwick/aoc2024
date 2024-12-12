{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day11 (Input, filename, parser, solve1, solve2) where

import qualified Data.IntMap as M
import Text.Trifecta (Parser)

import Parser (numbers)
import Utils (runN)

type Input = M.IntMap Int

filename :: String
filename = "data/day11.txt"

-- ===========================================================================
-- Parser

parser :: Parser Input
parser = do
    xs <- numbers " "
    pure $ M.fromListWith (+) [(i,1) | i <- xs]

-- ===========================================================================
-- Part 1

solve1 :: Input -> Int
solve1 = sum . runN 25 blink

blink :: Input -> Input
blink xs = M.fromListWith (+) zs
  where
    ys = M.toAscList xs
    zs = [(x', n) | (x, n) <- ys, x' <- doBlink x]

doBlink :: Int -> [Int]
doBlink x =
    if (x == 0) then [1]
    else if (even lenx) then [l, r]
    else [x * 2024]
  where 
    lenx = length (show x)
    divisor = 10 ^ (lenx `div` 2)
    l = x `div` divisor
    r = x `mod` divisor

-- ===========================================================================
-- Part 2

solve2 :: Input -> Int
solve2 = sum . runN 75 blink
