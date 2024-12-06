{-# LANGUAGE AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses #-}
module Cartesian2 where

import qualified Data.IntMap as M

type Point2 = (Int, Int)

tokey :: Point2 -> Int
tokey (x, y) = x * 1000 + y

fromkey :: Int -> Point2
fromkey n = (n `div` 1000, n `mod` 1000)

type Grid2 = M.IntMap Char

at :: Grid2 -> Point2 -> Maybe Char
at g p = M.lookup (tokey p) g

(!) :: Grid2 -> Point2 -> Char
(!) g p = g M.! (tokey p)

member :: Grid2 -> Point2 -> Bool
member g p = M.member (tokey p) g

count :: Grid2 -> (Char -> Bool) -> Int
count g f = M.size $ M.filter f g

find :: Grid2 -> (Char -> Bool) -> Maybe Point2
find g f = case M.toList (M.filter f g) of
                [] -> Nothing
                ((k,_):_) -> Just (fromkey k) 

adjust :: (Char -> Char) -> Point2 -> Grid2 -> Grid2
adjust f p g = M.adjust f (tokey p) g

foldrWithKey :: (Point2 -> Char -> b -> b) -> b -> Grid2 -> b
foldrWithKey f bs g = M.foldrWithKey (\k c b -> f (fromkey k) c b) bs g