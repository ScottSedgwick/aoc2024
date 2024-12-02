module Main (main) where

import Day02 (filename, parser, solve1, solve2)
import Text.Trifecta (parseFromFile)

main :: IO ()
main = do
    xs <- parseFromFile parser filename
    print xs
    case xs of
        Nothing -> putStrLn "Error - halted."
        Just x -> do
            print $ solve1 x
            print $ solve2 x
