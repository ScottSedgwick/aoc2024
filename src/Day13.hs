{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day13 (Input, filename, parser, solve1, solve2) where

import Numeric.LinearAlgebra.Data (Matrix, atIndex, matrix)
import Numeric.LinearAlgebra.HMatrix (linearSolve)
import Text.Trifecta (Parser, many, newline, string)

import Cartesian2
import Parser (number)

data Claw = Claw 
  { a :: Point2
  , b :: Point2
  , p :: Point2
  } deriving stock (Show, Eq)

type Input = [Claw]

filename :: String
filename = "data/day13.txt"

-- ===========================================================================
-- Parser

parser :: Parser Input
parser = many parseClaw

parseClaw :: Parser Claw
parseClaw = do
    _ <- string "Button A: X+"
    ax <- fromIntegral <$> number
    _ <- string ", Y+"
    ay <- fromIntegral <$> number
    _ <- newline
    _ <- string "Button B: X+"
    bx <- fromIntegral <$> number
    _ <- string ", Y+"
    by <- fromIntegral <$> number
    _ <- newline
    _ <- string "Prize: X="
    px <- fromIntegral <$> number
    _ <- string ", Y="
    py <- fromIntegral <$> number
    _ <- newline
    _ <- newline
    pure $ Claw { a = (ax, ay), b = (bx, by), p = (px, py)}

-- ===========================================================================
-- Part 1

-- I have to admit, I got a clue that this was a linear algebra problem, and that changed part 2 from unsolveable to trivial.

solve1 :: Input -> Int
solve1 = sum . map cost

cost :: Claw -> Int
cost (Claw (ax, ay) (bx, by) (px, py)) =
    case linearSolve steps endpoint of
        Just stepCounts -> getCost stepCounts (ax, ay) (bx, by) (px, py)
        Nothing -> 0
  where
    steps = matrix 2 [fromIntegral ax, fromIntegral bx, fromIntegral ay, fromIntegral by]
    endpoint = matrix 1 [fromIntegral px, fromIntegral py]

getCost :: Matrix Double -> Point2 -> Point2 -> Point2 -> Int
getCost stepCounts (ax, ay) (bx, by) (px, py) = 
    if (an * ax + bn * bx == px) && (an * ay + bn * by == py)  -- Check solution is valid, If not, return 0.
    then 3 * an + bn
    else 0
  where
    an = round (stepCounts `atIndex` (0,0))
    bn = round (stepCounts `atIndex` (1,0))

-- ===========================================================================
-- Part 2

solve2 :: Input -> Int
solve2 xs = solve1 (map f xs)
  where
    f (Claw (ax, ay) (bx, by) (px, py)) = 
        Claw { a = (ax, ay), b = (bx, by), p = (px + 10000000000000, py + 10000000000000) }

