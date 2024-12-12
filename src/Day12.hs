{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day12 (Input, filename, parser, solve1, solve2) where

import Text.Trifecta (Parser)

import Parser (wholeString)

type Input = String

filename :: String
filename = "data/day12.txt"

-- ===========================================================================
-- Parser

parser :: Parser Input
parser = wholeString

-- ===========================================================================
-- Part 1

solve1 :: Input -> Int
solve1 = undefined

-- ===========================================================================
-- Part 2

solve2 :: Input -> Int
solve2 = undefined
