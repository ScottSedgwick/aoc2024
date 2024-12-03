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

import Parser (wholeString)

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Text.Trifecta (Parser)

type Input = String
data Mul = Mul Int Int deriving stock (Show)

filename :: String
filename = "data/day03.txt"

parser :: Parser Input
parser = wholeString

-- 179834255
solve1 :: Input -> Int
solve1 xs = sum (map prod zs)
  where
    ys = splitOn "mul(" xs
    zs = catMaybes $ map getMul ys

prod :: Mul -> Int
prod (Mul x y) = x * y

getMul :: String -> Maybe Mul
getMul xs = if length ys < 2
            then Nothing
            else if length ws > 1
                 then Mul <$> x <*> y
                 else Nothing
  where
    ys = (splitOn "," xs) :: [String]
    x = readInt (take 1 ys)
    zs = headOr "" (drop 1 ys)
    ws = splitOn ")" zs
    y = readInt $ (take 1 ws)


readInt :: [String] -> Maybe Int
readInt (x:_) = if length x <= 3 && all isDigit x
             then readMaybe x
             else Nothing
readInt _ = Nothing

-- 80570939
solve2 :: Input -> Int
solve2 xs = sum ds
  where
    bs = splitOn "do()" xs
    cs = map trimAfterDont bs
    ds = map solve1 cs

trimAfterDont :: String -> String
trimAfterDont xs = headOr "" $ splitOn "don't()" xs

headOr :: a -> [a] -> a
headOr x [] = x
headOr _ (x:_) = x
