{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day14 (Input, filename, parser, solve1, solve2) where

import qualified Data.Vector as V
import Statistics.Sample (stdDev)
import Text.Trifecta (Parser, integer, many, string)

import Cartesian2

type Input = [Robot]

data Robot = Robot
  { posn :: Point2
  , vel :: Point2
  } deriving stock (Show, Eq)
  
filename :: String
filename = "data/day14.txt"

-- ===========================================================================
-- Parser

parser :: Parser Input
parser = many parserRobot

parserRobot :: Parser Robot
parserRobot = do
    _ <- string "p="
    px <- integer
    _ <- string ","
    py <- integer
    _ <- string "v="
    vx <- integer
    _ <- string ","
    vy <- integer
    pure $ Robot { posn = (fromIntegral px, fromIntegral py), vel = (fromIntegral vx, fromIntegral vy) }

-- ===========================================================================
-- Part 1

fieldSize :: Point2
--fieldSize = (11,7)
fieldSize = (101, 103)

moveCount :: Int
moveCount = 100

solve1 :: Input -> Int
solve1 xs = zs
  where
    ys = moveBots moveCount xs
    zs = score fieldSize ys

moveBots :: Int -> Input -> [Point2]
moveBots n xs = map (moveBot fieldSize n) xs

moveBot :: Point2 -> Int -> Robot -> Point2
moveBot (fx, fy) c (Robot (rx, ry) (vx, vy)) = ((rx + (vx * c)) `mod` fx, (ry + (vy * c)) `mod` fy)

score :: Point2 -> [Point2] -> Int
score (fx, fy) ps = 
    product $ map length qs
  where
    midX = (fx - 1) `div` 2
    midY = (fy - 1) `div` 2
    q1 = filter (\(x,y) -> x < midX && y < midY) ps
    q2 = filter (\(x,y) -> x < midX && y > midY) ps
    q3 = filter (\(x,y) -> x > midX && y < midY) ps
    q4 = filter (\(x,y) -> x > midX && y > midY) ps
    qs = [q1, q2, q3, q4]

-- ===========================================================================
-- Part 2

solve2 :: Input -> Int
solve2 xs = checkTree 0 xs

limit :: Double
limit = 25

checkTree :: Int -> Input -> Int
checkTree n xs = if isTree (moveBots n xs)
                 then n
                 else checkTree (n + 1) xs

isTree :: [Point2] -> Bool
isTree ps = stdDev xs < limit && stdDev ys < limit
  where
    xs = V.fromList $ map (\(x,_) -> fromIntegral x) ps
    ys = V.fromList $ map (\(_,y) -> fromIntegral y) ps

