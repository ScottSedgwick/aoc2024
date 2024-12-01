{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day01
       ( Input
       , filename
       , parser
       , solve1
       , solve2
       ) where

import Data.List (sort)
import Text.Trifecta

type Input = [(Int, Int)]

filename :: String
filename = "data/day01.txt"

parser :: Parser Input
parser = many $ do
  x <- fromIntegral <$> integer
  _ <- many space
  y <- fromIntegral <$> integer
  pure (x, y)

solve1 :: Input -> Int
solve1 = solve1' . unzip

solve1' :: ([Int], [Int]) -> Int
solve1' (ys, zs) = sum diffs
  where
    pairs = zip (sort ys) (sort zs)
    diffs = map (\(a,b) -> abs (a - b)) pairs

solve2 :: Input -> Int
solve2 = solve2' . unzip

solve2' :: ([Int], [Int]) -> Int
solve2' (xs,ys) = foldr (\x y -> y + f x) 0 xs
  where
    f x = x * (length (filter (== x) ys))