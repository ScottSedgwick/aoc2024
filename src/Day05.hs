{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day05
       ( Input
       , filename
       , parser
       , solve1
       , solve2
       ) where

import Parser (parse2dArray)

import qualified Data.Map as M
import Text.Trifecta (Parser)

type Input = M.Map (Int, Int) Char

filename :: String
filename = "data/day05.txt"

parser :: Parser Input
parser = parse2dArray

solve1 :: Input -> Int
solve1 = undefined

solve2 :: Input -> Int
solve2 = undefined
