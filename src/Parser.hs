{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Parser (number, clearSpaces, parseGrid2, parseGrid2Str, numbers, wholeString) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Text.Parser.Char (anyChar, char, digit)
import Text.Parser.Combinators (many, skipMany)
import Text.Trifecta (Parser)

import Cartesian2 (Grid2)

number :: Parser Integer
number = read <$> many digit

clearSpaces :: Parser ()
clearSpaces = skipMany (char ' ')

wholeString :: Parser String
wholeString = many anyChar

numbers :: String -> Parser [Int]
numbers sep = map read <$> splitOn sep <$> wholeString

-- Parses a text file. Splits on newlines.
-- Returns a 2d array, where the top left character is at (0, 0)
parseGrid2 :: Parser Grid2
parseGrid2 = parseGrid2Str <$> wholeString

parseGrid2Str :: String -> Grid2
parseGrid2Str xs = indexYs 0 M.empty ys
  where 
    ys = splitOn "\n" xs 

indexYs :: Int -> Grid2 -> [String] -> Grid2
indexYs _ m [] = m
indexYs y m (c:cs) = indexYs (y + 1) (indexXs y 0 m c) cs

indexXs :: Int -> Int -> Grid2 -> String -> Grid2
indexXs _ _ m [] = m
indexXs y x m (c:cs) = indexXs y (x + 1) (M.insert (x,y) c m) cs
