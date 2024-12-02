{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day03
       ( Input
       , filename
       , parser
       , solve1
       , solve2
       ) where

import Parser

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta (Parser)

type Input = [[Integer]]

filename :: String
filename = "data/day03.txt"

parser :: Parser Input
parser =  manyTill parserLine eof

parserLine :: Parser [Integer]
parserLine = manyTill (number <* clearSpaces) newline 

solve1 :: Input -> Int
solve1 = undefined

solve2 :: Input -> Int
solve2 = undefined