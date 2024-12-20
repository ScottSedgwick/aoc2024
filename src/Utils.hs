module Utils (gauss, headOr, readInt, mapOnKeys, runN, valAtEq) where

import Data.Char (isDigit)
import qualified Data.Map as M
import Text.Read (readMaybe)

gauss :: Int -> Int -> Int
gauss lo hi = (hi * (hi + 1) - lo * (lo - 1)) `div` 2

-- Safe head. Returns a default value if the list is empty.
headOr :: a -> [a] -> a
headOr x [] = x
headOr _ (x:_) = x

mapOnKeys :: (k -> b) -> M.Map k a -> M.Map k b
mapOnKeys f m = M.mapWithKey (\k _ -> f k) m

readInt :: [String] -> Maybe Int
readInt (x:_) = if length x <= 3 && all isDigit x
             then readMaybe x
             else Nothing
readInt _ = Nothing

-- Checks to see if the value in a map at a certain key exists, and is equal to a specified value.
valAtEq :: (Eq a, Ord k) => M.Map k a -> k -> a -> Bool
valAtEq m k c = 
    case M.lookup k m of
        (Just v) -> c == v
        Nothing -> False

runN :: Int -> (a -> a) -> a -> a
runN 0 _ a = a
runN n f a = runN (n - 1) f (f a)
