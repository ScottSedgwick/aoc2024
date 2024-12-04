module Utils (headOr, readInt) where

import Data.Char (isDigit)
import Text.Read (readMaybe)

headOr :: a -> [a] -> a
headOr x [] = x
headOr _ (x:_) = x

readInt :: [String] -> Maybe Int
readInt (x:_) = if length x <= 3 && all isDigit x
             then readMaybe x
             else Nothing
readInt _ = Nothing