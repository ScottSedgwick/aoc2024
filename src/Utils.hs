module Utils (headOr, readInt, mapOnKeys, valAtEq) where

import Data.Char (isDigit)
import qualified Data.Map as M
import Text.Read (readMaybe)

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

valAtEq :: (Eq a, Ord k) => M.Map k a -> k -> a -> Bool
valAtEq m k c = 
    case M.lookup k m of
        (Just v) -> c == v
        Nothing -> False
