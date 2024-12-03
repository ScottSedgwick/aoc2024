{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Parser (number, clearSpaces, wholeString) where

import Text.Parser.Char (anyChar, char, digit)
import Text.Parser.Combinators (many, skipMany)
import Text.Trifecta (Parser)

number :: Parser Integer
number = read <$> many digit

clearSpaces :: Parser ()
clearSpaces = skipMany (char ' ')

wholeString :: Parser String
wholeString = many anyChar