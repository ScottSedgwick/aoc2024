{- |
Copyright: (c) 2024 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day17 (Input, filename, parser, solve1, solve2) where

import Debug.Trace
import Data.Bits (xor)
import Data.Maybe (catMaybes)
import qualified Data.List as L
import qualified Data.Vector as V
import Text.Trifecta (Parser, integer, many, optional, string)

data Opcode = Adv   -- 0. Register A `div` (2 ^ Operand) -> Register A, Increment Pointer
            | Bxl   -- 1. Register B `xor` Operand -> Register B, Increment Pointer
            | Bst   -- 2. Operand `mod` 8 -> Register B, Increment Pointer
            | Jnz   -- 3. If Register A == 0 then Increment Pointer else Operand -> Instruction Pointer
            | Bxc   -- 4. Register B `xor` Register C -> Register B, Increment Pointer (ignore operand)
            | Out   -- 5. Output Operand `div` 8  (Output is a list of Int)
            | Bdv   -- 6. Register A `div` (2 ^ Operand) -> Register B, Increment Pointer
            | Cdv   -- 7. Register A `div` (2 ^ Operand) -> Register C, Increment Pointer
            deriving stock (Show, Eq, Ord)

type Operation = (Opcode, Int)

data Input = Computer
  { regA :: Int
  , regB :: Int
  , regC :: Int
  , program :: V.Vector Operation
  , pointer :: Int
  } deriving stock (Show, Eq)
  
filename :: String
filename = "data/day17.txt"

-- ===========================================================================
-- Parser

parser :: Parser Input
parser = do
    _ <- string "Register A: "
    a <- fromIntegral <$> integer
    _ <- string "Register B: "
    b <- fromIntegral <$> integer
    _ <- string "Register C: "
    c <- fromIntegral <$> integer
    _ <- string "Program: "
    xs <- many $ do
        x <- fromIntegral <$> integer
        _ <- optional (string ",")
        pure x
    pure $ Computer 
      { regA = a
      , regB = b
      , regC = c
      , program = V.fromList (parseOperations xs)
      , pointer = 0 
      }
    
parseOperations :: [Int] -> [Operation]
parseOperations (a:b:xs) = parseOperation a b : parseOperations xs
parseOperations _ = []

parseOperation :: Int -> Int -> Operation
parseOperation a b =
    case a of
        0 -> (Adv, b)
        1 -> (Bxl, b)
        2 -> (Bst, b)
        3 -> (Jnz, b)
        4 -> (Bxc, b)
        5 -> (Out, b)
        6 -> (Bdv, b)
        _ -> (Cdv, b)

-- ===========================================================================
-- Part 1

solve1 :: Input -> String
solve1 xs = L.intercalate "," $ map show $ catMaybes $ doSolve1 xs

doSolve1 :: Input -> [Maybe Int]
doSolve1 xs = 
    if pointer xs >= V.length (program xs)
    then []
    else output : doSolve1 ys
  where
    (ys,output) = doOp xs

doOp :: Input -> (Input, Maybe Int)
doOp xs =
    case (program xs) V.! (pointer xs) of
        (Adv, x) -> (xs { regA = ((regA xs) `div` (2 ^ combo xs x)), pointer = (pointer xs) + 1 }, Nothing)
        (Bxl, x) -> (xs { regB = x `xor` (regB xs), pointer = (pointer xs) + 1 }, Nothing)
        (Bst, x) -> (xs { regB = combo xs x `mod` 8, pointer = (pointer xs) + 1 }, Nothing)
        (Jnz, x) -> (if ((regA xs) == 0)
                    then xs { pointer = (pointer xs) + 1 }
                    else xs { pointer = x `div` 2 }, Nothing)
        (Bxc, _) -> (xs { regB = (regB xs) `xor` (regC xs), pointer = (pointer xs) + 1 }, Nothing)
        (Out, x) -> (xs { pointer = (pointer xs) + 1}, Just (combo xs x `mod` 8))
        (Bdv, x) -> (xs { regB = ((regA xs) `div` (2 ^ combo xs x)), pointer = (pointer xs) + 1 }, Nothing)
        (Cdv, x) -> (xs { regC = ((regA xs) `div` (2 ^ combo xs x)), pointer = (pointer xs) + 1 }, Nothing)

combo :: Input -> Int -> Int
combo _ 0 = 0
combo _ 1 = 1
combo _ 2 = 2
combo _ 3 = 3
combo xs 4 = regA xs
combo xs 5 = regB xs
combo xs 6 = regC xs
combo _ x = error $ "Invalid combo operand: " <> show x

-- ===========================================================================
-- Part 2

solve2 :: Input -> Int
solve2 xs = 
    trace ("Xs: " <> show ps) $ 
    doSolve2 xs ps [x * 8 + o1 | x <- [0..]]
  where
    (o1:_) = opToInt ((program xs) V.! 0)
    ps = getProg (program xs)

doSolve2 :: Input -> [Int] -> [Int] -> Int
doSolve2 xs ops (y:ys) =
    -- trace ("Check: " <> show y) $
    -- trace ("Ps: " <> show ps) $
    if ps == ops
    then y
    else doSolve2 xs ops ys
  where
    ps = catMaybes (doSolve1 (xs { regA = y}))

getProg :: V.Vector Operation -> [Int]
getProg = concat . map opToInt . V.toList

opToInt :: Operation -> [Int]
opToInt (Adv,x) = [0,x]
opToInt (Bxl,x) = [1,x]
opToInt (Bst,x) = [2,x]
opToInt (Jnz,x) = [3,x]
opToInt (Bxc,x) = [4,x]
opToInt (Out,x) = [5,x]
opToInt (Bdv,x) = [6,x]
opToInt (Cdv,x) = [7,x]
