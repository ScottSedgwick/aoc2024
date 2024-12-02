{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Parser where

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Trifecta (Parser)

number :: Parser Integer
number = read <$> many digit

clearSpaces :: Parser ()
clearSpaces = skipMany (char ' ')