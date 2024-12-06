module Main (main) where

import Day06 (filename, parser, solve1, solve2)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.Trifecta (parseFromFile)

main :: IO ()
main = do
    start <- getCurrentTime
    print start
    xs <- parseFromFile parser filename
    print xs
    case xs of
        Nothing -> putStrLn "Error - halted."
        Just x -> do
            putStr "Part 1: "
            print $ solve1 x
            putStr "Part 2: "
            print $ solve2 x
    end <- getCurrentTime
    let diff = diffUTCTime end start
    putStr "Time Taken: "
    print diff
