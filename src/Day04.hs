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

import Parser (parse2dArray)
import Utils (mapOnKeys, valAtEq)

import qualified Data.Map as M
import Text.Trifecta (Parser)

type Input = M.Map (Int, Int) Char

filename :: String
filename = "data/day04.txt"

parser :: Parser Input
parser = parse2dArray

solve1 :: Input -> Int
solve1 m = sum $ mapOnKeys (findXmas m) m

findXmas :: Input -> (Int, Int) -> Int
findXmas m (x,y) = sum $ map f [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
  where
    f (dx, dy) =
      if valAtEq m (x,y) 'X'
          && valAtEq m (x + dx, y + dy) 'M'
          && valAtEq m (x + dx * 2, y + dy * 2) 'A'
          && valAtEq m (x + dx * 3, y + dy * 3) 'S'
          then 1
          else 0

solve2 :: Input -> Int
solve2 m = sum $ mapOnKeys (findMas m) m

findMas :: Input -> (Int, Int) -> Int
findMas m (x, y) =
    if valAtEq m (x,y) 'A' 
        && ( ( (valAtEq m (x - 1, y - 1) 'M' && valAtEq m (x + 1, y + 1) 'S') && (valAtEq m (x + 1, y - 1) 'M' && valAtEq m (x - 1, y + 1) 'S') )
          || ( (valAtEq m (x + 1, y + 1) 'M' && valAtEq m (x - 1, y - 1) 'S') && (valAtEq m (x - 1, y + 1) 'M' && valAtEq m (x + 1, y - 1) 'S') )
          || ( (valAtEq m (x - 1, y - 1) 'M' && valAtEq m (x + 1, y + 1) 'S') && (valAtEq m (x - 1, y + 1) 'M' && valAtEq m (x + 1, y - 1) 'S') )
          || ( (valAtEq m (x + 1, y + 1) 'M' && valAtEq m (x - 1, y - 1) 'S') && (valAtEq m (x + 1, y - 1) 'M' && valAtEq m (x - 1, y + 1) 'S') )
          )
        then 1
        else 0
