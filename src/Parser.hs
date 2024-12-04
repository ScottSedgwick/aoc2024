{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Parser (number, clearSpaces, parse2dArray, wholeString) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Text.Parser.Char (anyChar, char, digit)
import Text.Parser.Combinators (many, skipMany)
import Text.Trifecta (Parser)

number :: Parser Integer
number = read <$> many digit

clearSpaces :: Parser ()
clearSpaces = skipMany (char ' ')

wholeString :: Parser String
wholeString = many anyChar

parse2dArray :: Parser (M.Map (Int, Int) Char)
parse2dArray = do
    xs <- wholeString
    let ys = splitOn "\n" xs
    pure $ indexYs 0 M.empty ys

indexYs :: Int -> M.Map (Int, Int) Char -> [String] -> M.Map (Int, Int) Char
indexYs _ m [] = m
indexYs y m (c:cs) = indexYs (y + 1) (indexXs y 0 m c) cs

indexXs :: Int -> Int -> M.Map (Int, Int) Char -> String -> M.Map (Int, Int) Char
indexXs _ _ m [] = m
indexXs y x m (c:cs) = indexXs y (x + 1) (M.insert (x,y) c m) cs