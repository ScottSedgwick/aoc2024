{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day09 (Input, filename, parser, solve1, solve2) where

import qualified Data.List as L
import qualified Data.Map as M
import Text.Trifecta (Parser)

import Parser (wholeString)
import Utils (gauss)

data Node = Node
  { nodeId :: Int
  , nodeLen :: Int
  , nodeSpace :: Int
  , nodePos :: Int
  } deriving stock (Show, Eq)

type Input = [Node]

filename :: String
filename = "data/day09.txt"

-- ===========================================================================
-- Parser

parser :: Parser Input
parser = do
    xs <- wholeString
    pure $ calcPos 0 (buildNodes 0 xs)

buildNodes :: Int -> String -> Input
buildNodes _ [] = []
buildNodes n (x:[]) = [ Node { nodeId = n, nodeLen = read [x], nodeSpace = 0, nodePos = 0 } ]
buildNodes n (x:y:xs) = Node { nodeId = n, nodeLen = read [x], nodeSpace = read [y], nodePos = 0 } : buildNodes (n + 1) xs

calcPos :: Int -> Input -> Input
calcPos _ [] = []
calcPos n (x:xs) = x { nodePos = n } : calcPos (n + nodeLen x + nodeSpace x) xs

-- ===========================================================================
-- Part 1

solve1 :: Input -> Int
solve1 xs = total ys
  where
    ys = pack xs

pack :: Input -> Input
pack [] = []
pack (x:xs) = if nodeSpace x == 0
              then x : pack xs
              else pack' x xs

pack' :: Node -> Input -> Input
pack' x [] = [x {nodeSpace = 0}]
pack' x xs = if nodeLen y < nodeSpace x
             then pack (x' : y' : init xs)
             else x' : pack (y'' : (init xs <> [y''']))
  where
    x' = x { nodeSpace = 0 }
    y = last xs
    y' = y { nodeSpace = nodeSpace x - nodeLen y }
    y'' = y { nodeSpace = 0, nodeLen = nodeSpace x }
    y''' = y { nodeLen = nodeLen y - nodeSpace x }

total :: Input -> Int
total xs = mulList 0 ys
  where
    ys = buildList xs

buildList :: Input -> [Int]
buildList [] = []
buildList (x:xs) = L.replicate (nodeLen x) (nodeId x) <> L.replicate (nodeSpace x) 0 <> buildList xs

mulList :: Int -> [Int] -> Int
mulList _ [] = 0
mulList n (x:xs) = (n * x) + (mulList (n + 1) xs) 

-- ===========================================================================
-- Part 2

-- Map nodePos (nodeId, nodeLen)
inputToFiles :: Input -> M.Map Int (Int, Int)
inputToFiles = L.foldr (\a b -> M.insert (nodePos a) (nodeId a, nodeLen a) b) M.empty

-- Map freePos freeLen
inputToFree :: Input -> M.Map Int Int
inputToFree = L.foldr (\a b -> if (nodeSpace a == 0) then b else M.insert (nodePos a + nodeLen a) (nodeSpace a) b) M.empty 

solve2 :: Input -> Int
solve2 xs = total2 files'
  where
    files = inputToFiles xs
    free = inputToFree xs
    (files', _) = foldl' pack2 (files, free) (reverse $ M.keys files)

total2 :: M.Map Int (Int, Int) -> Int
total2 files = sum $ map f (M.toList files)
  where
    f (nPos, (nId, nLen)) = nId * gauss nPos (nPos + nLen - 1)

pack2 :: (M.Map Int (Int, Int), M.Map Int Int) -> Int -> (M.Map Int (Int, Int), M.Map Int Int)
pack2 (files, free) offset =
    case [(k, v) | (k, v) <- M.assocs free, k < offset, v >= fileSize] of
      [] -> (files, free)
      (k, v) : _ -> (M.insert k (fileId, fileSize) (M.delete offset files), free2)
        where
          free1 = M.delete k free
          free2 | v == fileSize = free1
                | otherwise = M.insert (k + fileSize) (v - fileSize) free1
  where
    (fileId, fileSize) = files M.! offset
