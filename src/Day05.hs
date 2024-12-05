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

import Parser (number)
import Utils (headOr)

import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, member)
import Text.Parser.Char (anyChar, char, newline)
import Text.Parser.Combinators (many, manyTill)
import Text.Trifecta (Parser)

data Input = Input 
  { rules :: Set (Int, Int)
  , updates :: [[Int]]
  } deriving stock (Show)

filename :: String
filename = "data/day05.txt"

parser :: Parser Input
parser = do
    rs <- many intPair
    _ <- newline
    us <- many intsRow
    pure $ Input { rules = fromList rs, updates = us }

intPair :: Parser (Int, Int)
intPair = do
    x <- number
    _ <- char '|'
    y <- number
    _ <- newline
    pure (fromIntegral x, fromIntegral y)

intsRow :: Parser [Int]
intsRow = do
    xs <- manyTill anyChar newline
    let ys = splitOn "," xs
    pure $ map read ys

solve1 :: Input -> Int
solve1 inp = sum xs
  where
    rs = rules inp
    us = updates inp
    vs = filter (validRule rs) us
    xs = map middle vs

validRule :: Set (Int, Int) -> [Int] -> Bool
validRule _ [] = True
validRule m (x:xs) = f m [] xs x

f :: Set (Int, Int) -> [Int] -> [Int] -> Int -> Bool
f _ _ [] _ = True
f m pre pst x = all (after m x) pst && all (before m x) pre && f m (x:pre) (drop 1 pst) (headOr 0 pst)

after :: Set (Int, Int) -> Int -> Int -> Bool
after m x y = (x,y) `member` m

before :: Set (Int, Int) -> Int -> Int -> Bool
before m x y = (y,x) `member` m

middle :: [Int] -> Int
middle xs = headOr 0 $ drop (length xs `div` 2) xs

solve2 :: Input -> Int
solve2 inp = sum xs
  where
    rs = rules inp
    us = updates inp
    vs = filter (\u -> not (validRule rs u)) us
    ws = map (correctOrder rs) vs
    xs = map middle ws

correctOrder :: Set (Int, Int) -> [Int] -> [Int]
correctOrder m xs = sortBy (\x y -> if member (x,y) m then LT else GT) xs
