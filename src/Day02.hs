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

import Parser

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta (Parser)

type Input = [[Integer]]

filename :: String
filename = "data/day02.txt"

parser :: Parser Input
parser =  manyTill parserLine eof

parserLine :: Parser [Integer]
parserLine = manyTill (number <* clearSpaces) newline 

solve1 :: Input -> Int
solve1 = length . filter isSafe

isSafe :: [Integer] -> Bool
isSafe xs = allChanging (-3) (-1) ys || allChanging 1 3 ys
    where
        ys = map (\(a,b) -> a - b) (zip xs (drop 1 xs))

allChanging :: Integer -> Integer -> [Integer] -> Bool
allChanging minVal maxVal = all (\y -> y <= maxVal && y >= minVal)

solve2 :: Input -> Int
solve2 = length . filter isDampSafe

isDampSafe :: [Integer] -> Bool
isDampSafe = any isSafe . damps

damps :: [Integer] -> [[Integer]]
damps xs = map f [0..length xs]
    where
        f x = take x xs ++ drop (x + 1) xs