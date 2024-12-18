{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day16 (Input, filename, parser, solve1, solve2) where

import Debug.Trace
import Algorithm.Search (aStar, dijkstra)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Text.Trifecta (Parser)

import Cartesian2
import Parser (parseGrid2)

data Item = Start | End | Wall | Space deriving stock (Eq, Ord)
instance Show Item where
    show Start = "S"
    show End = "E"
    show Wall = "#"
    show Space = "."

data Input = Input
  { grid :: Grid Item
  , start :: Point2
  , end :: Point2
  } deriving stock (Show, Eq, Ord)

data Action = Null | Move | TurnL | TurnR deriving stock (Show, Eq, Ord)

data State = State
  { posn :: Point2
  , dirn :: Dir
  , action :: Action
  } deriving stock (Show, Eq, Ord)
  
filename :: String
filename = "data/day16.txt"

-- ===========================================================================
-- Parser

parser :: Parser Input
parser = do
    xs <- (fmap parseItem) <$> parseGrid2
    pure $ Input { grid = xs, start = get xs Start, end = get xs End }

get :: M.Map Point2 Item -> Item -> Point2
get g i = 
    case xs of
      []    -> (0,0)
      (x:_) -> fst x
  where
    xs = M.toList $ M.filter (\a -> a == i) g

parseItem :: Char -> Item
parseItem '#' = Wall
parseItem 'S' = Start
parseItem 'E' = End
parseItem '.' = Space
parseItem _ = error "Invalid Item"

-- ===========================================================================
-- Part 1

solve1 :: Input -> Int
solve1 xs = 
    case result of
        Nothing -> 0
        Just (c,_) -> c
  where
    result = dijkstra (step xs) cost (finished xs) (State { posn = start xs, dirn = E, action = Null })

-- score :: [State] -> Int
-- score xs = 
--     trace "Score" $
--     trace (show res)
--     trace "Path" $
--     trace (show xs) $ 
--     res
--   where
--     res = sum (map f xs)
--     f s = case (action s) of
--             Null -> 0
--             Move -> 1
--             TurnL -> 1000
--             TurnR -> 1000

cost :: State -> State -> Int
cost s1 s2 = 
    if posn s1 == posn s2 
    then 1000
    else 1

step :: Input -> State -> [State]
step xs ys = 
    catMaybes [s1, s2, s3]
  where
    gd = grid xs
    p0 = posn ys
    d0 = dirn ys
    p1 = move d0 p0
    s1 = checkPosn gd p1 (State { posn = p1, dirn = d0, action = Move})
    p2 = move (turnL d0) p0
    s2 = checkPosn gd p2 (State { posn = p0, dirn = (turnL d0), action = TurnL})
    p3 = move (turnR d0) p0
    s3 = checkPosn gd p3 (State { posn = p0, dirn = (turnR d0), action = TurnR})

checkPosn :: M.Map Point2 Item -> Point2 -> State -> Maybe State
checkPosn g p a =
    case M.lookup p g of
        Just Space -> Just a
        Just End -> Just a
        _ -> Nothing

turnL :: Dir -> Dir
turnL N = W
turnL E = N
turnL S = E
turnL W = S

turnR :: Dir -> Dir
turnR N = E
turnR E = S
turnR S = W
turnR W = N

finished :: Input -> State -> Bool
finished xs ys = (end xs) == (posn ys)

-- ===========================================================================
-- Part 2

solve2 :: Input -> Int
solve2 xs =
    case result of
        Nothing -> 0
        Just (_,ys) -> length ys
  where
    result = aStar (step xs) cost (estcost xs) (finished xs) (State { posn = start xs, dirn = E, action = Null })

estcost :: Input -> State -> Int
estcost xs ys = dist p ep
  where
    p = posn ys
    ep = end xs

dist :: Point2 -> Point2 -> Int
dist (x1, y1) (x2, y2) = (abs (x2 - x1)) + (abs (y2 - y1))

