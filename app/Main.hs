module Main (main) where

import Day01 (filename, parser, solve1, solve2)
import Text.Trifecta (parseFromFile)

main :: IO ()
main = do
    xs <- parseFromFile parser filename 
    case xs of
        Nothing -> putStrLn "Error?"
        Just x -> do
            print $ solve1 x
            print $ solve2 x
