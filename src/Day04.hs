{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day04
       ( Input
       , filename
       , parser
       , solve1
       , solve2
       ) where

import Parser (wholeString)

import Text.Trifecta (Parser)

type Input = String

filename :: String
filename = "data/day04.txt"

parser :: Parser Input
parser = wholeString

solve1 :: Input -> Int
solve1 = undefined

solve2 :: Input -> Int
solve2 = undefined
