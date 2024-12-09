{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Parser (number, clearSpaces, parseGrid2, wholeString) where

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

-- Parses a text file. Splits on newlines.
-- Returns a 2d array, where the top left character is at (0, 0)
parseGrid2 :: Parser Grid2
parseGrid2 = do
    xs <- wholeString
    let ys = splitOn "\n" xs
    pure $ indexYs 0 M.empty ys

indexYs :: Int -> Grid2 -> [String] -> Grid2
indexYs _ m [] = m
indexYs y m (c:cs) = indexYs (y + 1) (indexXs y 0 m c) cs

indexXs :: Int -> Int -> Grid2 -> String -> Grid2
indexXs _ _ m [] = m
indexXs y x m (c:cs) = indexXs y (x + 1) (M.insert (x,y) c m) cs
