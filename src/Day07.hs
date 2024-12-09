{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day07 (Input, filename, parser, solve1, solve2) where

import qualified Data.IntSet as S
import Data.List.Split (splitOn)
import Text.Trifecta (Parser)

import Parser (wholeString)
import Utils (headOr)

type Input = [Op] 
    
data Op = Op
  { result :: Int
  , args :: [Int]
  } deriving stock (Show, Eq)

filename :: String
filename = "data/day07.txt"

parser :: Parser Input
parser = do
    xs <- wholeString
    let ys = splitOn "\n" xs
    pure $ map pOp ys

pOp :: String -> Op
pOp xs = Op { result = r, args = map read zs }
  where
    ys = splitOn ": " xs
    r = read (headOr "0" ys)
    zs = splitOn " " (headOr "0" $ drop 1 ys)

solve1 :: Input -> Int
solve1 xs = sum $ map result ys
  where
    ys = filter (canSolve ops1) xs

ops1 :: [(Int -> Int -> Int)]
ops1 = [(+), (*)]

canSolve :: [(Int -> Int -> Int)] -> Op -> Bool
canSolve ops op = S.member (result op) (solutions ops 0 S.empty (args op))

solutions :: [(Int -> Int -> Int)] -> Int -> S.IntSet -> [Int] -> S.IntSet
solutions _   val s []     = S.insert val s
solutions ops 0   s (x:xs) = solutions ops x s xs
solutions ops val s (x:xs) = S.unions $ map (\op -> solutions ops (op val x) s xs) ops

solve2 :: Input -> Int
solve2 xs = sum $ map result ys
  where
    ys = filter (canSolve ops2) xs

ops2 :: [(Int -> Int -> Int)]
ops2 = [(+), (*), barjoin]

barjoin :: Int -> Int -> Int
barjoin x y = read (show x <> show y)