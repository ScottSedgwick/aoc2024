{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day18 (Input, filename, parser, solve1, solve2) where

import Algorithm.Search (dijkstra)
import qualified Data.Set as S
import Text.Trifecta (Parser, integer, many, string)

import Cartesian2

type Input = [Point2]
  
filename :: String
filename = "data/day18.txt"

-- ===========================================================================
-- Parser

parser :: Parser Input
parser = many parsePoint2

parsePoint2 :: Parser Point2
parsePoint2 = do
    x <- fromIntegral <$> integer
    _ <- string ","
    y <- fromIntegral <$> integer
    pure (x,y)

-- ===========================================================================
-- Part 1

data State = State
  { blocks :: S.Set Point2 
  , posn :: Point2
  , end :: Point2
  } deriving stock (Show, Eq, Ord)

endp :: Point2
-- endp = (6, 6)
endp = (70, 70)

n :: Int
-- n = 12
n = 1024

solve1 :: Input -> Int
solve1 xs = 
    case doSolve1 xs n of
        Nothing -> 0
        Just (c,_) -> c

doSolve1 :: Input -> Int -> Maybe (Int, [State])
doSolve1 xs x = dijkstra step cost finished ys
  where
    ys = State { blocks = S.fromList (take x xs), posn = (0,0), end = endp }
    
cost :: State -> State -> Int
cost _ _ = 1

-- Get all moves from the current posn that are not blocked
step :: State -> [State]
step xs = zs
  where
    ys = filter (validPosn xs) (neighbours (posn xs))
    zs = map (\y -> xs { posn = y} ) ys

validPosn :: State -> Point2 -> Bool
validPosn xs p = 
    S.notMember p (blocks xs) &&
    fst p >= 0 &&
    fst p <= fst (end xs) &&
    snd p >= 0 &&
    snd p <= snd (end xs)

finished :: State -> Bool
finished xs = posn xs == end xs

-- ===========================================================================
-- Part 2

solve2 :: Input -> Point2
solve2 xs = firstUnsolveable xs [n..]

firstUnsolveable :: Input -> [Int] -> Point2
firstUnsolveable _  []     = (0,0)
firstUnsolveable xs (y:ys) = 
    case doSolve1 xs y of
        Nothing -> xs !! (y - 1)
        _ -> firstUnsolveable xs ys

