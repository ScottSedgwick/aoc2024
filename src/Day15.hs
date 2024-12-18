{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day15 (Input, filename, parser, solve1, solve2) where

import Debug.Trace
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Text.Trifecta (Parser)

import Cartesian2
import Parser (wholeString, parseGrid2Str)

data Item = Wall | Robot | Box deriving stock (Eq)
instance Show Item where
    show Wall = "#"
    show Robot = "@"
    show Box = "O"

data Input = Room
  { robot :: Point2
  , items :: Grid Item
  , moves :: [Dir]
  } deriving stock (Show, Eq)
  
filename :: String
filename = "data/day15.txt"

-- ===========================================================================
-- Parser

parser :: Parser Input
parser = do
    xs <- wholeString
    let ys = splitOn "\n\n" xs
    let zs = parseGrid2Str (head ys)
    let ms = mapMaybe charToMove (head (tail ys))
    let its = M.fromList $ mapMaybe charToItem (M.toList zs)
    let bs = M.toList $ M.filter (\x -> x == Robot) its
    pure $ Room { robot = fst (head bs), items = its, moves = ms }

charToItem :: (Point2, Char) -> Maybe (Point2, Item)
charToItem (p, '#') = Just (p, Wall)
charToItem (p, '@') = Just (p, Robot)
charToItem (p, 'O') = Just (p, Box)
charToItem _ = Nothing

charToMove :: Char -> Maybe Dir
charToMove '^' = Just N
charToMove '>' = Just E
charToMove '<' = Just W
charToMove 'v' = Just S
charToMove _   = Nothing

-- ===========================================================================
-- Part 1

solve1 :: Input -> Int
solve1 (Room _ its []    ) = score its
solve1 (Room b its (m:ms)) = 
    trace "Board" $
    trace (showGrid '.' its) $ 
    trace ("Move: " <> show m) $
    solve1 r'
  where
    r' = move1 m (Room b its ms)

-- Find robot. Check if it can move - if not, continue
-- If it can, move everything it is touching in front of it one space, then continue
move1 :: Dir -> Input -> Input
move1 d (Room b its ms) = 
    if canMove d b its
    then Room { robot = b', items = its', moves = ms }
    else Room { robot = b, items = its, moves = ms }
  where
    b' = move d b
    its' = movethings d b its

-- True if, in the specified direction, from the specified point, there is a space (a point not in the map) before there is a wall.
canMove :: Dir -> Point2 -> M.Map Point2 Item -> Bool
canMove d p m = 
    case M.lookup p' m of
        Just Wall -> False
        Nothing -> True
        _ -> canMove d p' m
  where
    p' = move d p

-- Starting at the specified point, in the specified direction, move everything one position until we hit the first space.
-- Get list of points to move, from starting point to first space
-- In reverse order, move them in the map
movethings :: Dir -> Point2 -> M.Map Point2 Item -> M.Map Point2 Item
movethings d p m = trace "Moved" $ m2
  where
    ps = getthings d p m
    m2 = foldr (movething d) m ps

-- Move thing at p in direction d - insert in new location and delete from old location.
movething :: Dir -> Point2 -> M.Map Point2 Item -> M.Map Point2 Item
movething d p m = 
    case M.lookup p m of
        Nothing -> m
        Just v  -> M.delete p $ M.insert p' v m
  where
    p' = move d p

getthings :: Dir -> Point2 -> M.Map Point2 Item -> [Point2]
getthings d p m = 
    case M.lookup p' m of
        Nothing -> []
        _       -> p : getthings d p' m
  where
    p' = move d p

score :: M.Map Point2 Item -> Int
score xs = sum ys
  where
    ys = map (\(p,i) -> if (i == Box) then scorePoint p else 0) (M.toList xs)

scorePoint :: Point2 -> Int
scorePoint (x,y) = x + y * 104

-- ===========================================================================
-- Part 2

solve2 :: Input -> Int
solve2 _ = 0
