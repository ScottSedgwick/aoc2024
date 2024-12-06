{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day06
       ( Input
       , filename
       , parser
       , solve1
       , solve2
       ) where

import Data.List (nub)
import Data.Maybe (isNothing)
import qualified Data.Set as S
import Text.Trifecta (Parser)

import Parser (parseGrid2)
import Cartesian2

type Input = Grid2
data Dirn = DUp | DDown | DLeft | DRight deriving stock (Show, Eq, Ord)

filename :: String
filename = "data/day06.txt"

parser :: Parser Input
parser = parseGrid2

solve1 :: Input -> Int
solve1 xs = length posns
  where
    posns = case buildPaths xs of
              Nothing -> []
              Just (ps) -> nub (map fst (S.toList ps))

buildPaths :: Input -> Maybe (S.Set (Point2, Dirn))
buildPaths xs = buildPath xs pos DUp (S.singleton (pos, DUp))
  where
    pos = case find xs (\a -> a == '^') of
            Nothing -> (1000,1000)
            Just p  -> p

buildPath :: Input -> Point2 -> Dirn -> S.Set (Point2, Dirn) -> Maybe (S.Set (Point2, Dirn))
buildPath xs p d path = 
    if not (member xs p')                                       -- We have left the space.
    then Just path
    else if xs ! p' == '#'                                      -- We would hit something - turn right.
         then buildPath xs p (turnRight d) path
         else if (S.member (p',d) path)                         -- We are in a position, facing a direction, that we have been in before.  This is a loop - terminate.
              then Nothing
              else buildPath xs p' d (S.insert (p',d) path)     -- Move and continue.
  where
    p' = nextPos p d

nextPos :: Point2 -> Dirn -> Point2
nextPos (x,y) DUp    = (x, y - 1)
nextPos (x,y) DRight = (x + 1, y)
nextPos (x,y) DDown  = (x, y + 1)
nextPos (x,y) DLeft  = (x - 1, y)

turnRight :: Dirn -> Dirn
turnRight DUp    = DRight
turnRight DRight = DDown
turnRight DDown  = DLeft
turnRight DLeft  = DUp

-- Iterate over all changes to the map where we can turn a single '.' to a '#' and see which ones do not terminate (return Nothing)
solve2 :: Input -> Int
solve2 xs = length (filter isNothing zs)
  where
    ys = newMaps xs
    zs = map buildPaths ys

-- Generate a list of maps with one position changed from '.' to '#'
newMaps :: Input -> [Input]
newMaps xs = foldrWithKey (\k a b -> if a == '.' then (adjust (\_ -> '#') k xs):b else b) [] xs