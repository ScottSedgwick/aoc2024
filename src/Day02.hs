{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day02
       ( Input
       , filename
       , parser
       , solve1
       , solve2
       ) where

import Data.List (sort)
import Text.Trifecta

type Input = [(Integer, Integer)]

filename :: String
filename = "data/day02.txt"

parser :: Parser Input
parser = many $ do
       x <- integer
       _ <- many space
       y <- integer
       pure (x, y)

solve1 :: Input -> Integer
solve1 = undefined

solve2 :: Input -> Integer
solve2 = undefined