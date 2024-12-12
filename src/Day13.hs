{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day13 (Input, filename, parser, solve1, solve2) where

import Text.Trifecta (Parser)

import Parser (wholeString)

type Input = String

filename :: String
filename = "data/day13.txt"

-- ===========================================================================
-- Parser

parser :: Parser Input
parser = wholeString

-- ===========================================================================
-- Part 1

solve1 :: Input -> Int
solve1 _ = 0

-- ===========================================================================
-- Part 2

solve2 :: Input -> Int
solve2 _ = 0

