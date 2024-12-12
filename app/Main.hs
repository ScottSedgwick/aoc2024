module Main (main) where

import Day12 (filename, parser, solve1, solve2)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.Trifecta (parseFromFile)

main :: IO ()
main = do
    start <- getCurrentTime
    logMsg "Started at: " start
    xs <- parseFromFile parser filename
    case xs of
        Nothing -> putStrLn "Error parsing data - halted."
        Just x -> do
            logMsg "Data: " x
            logMsg "Part 1: " $ solve1 x
            logMsg "Part 2: " $ solve2 x
    end <- getCurrentTime
    let diff = diffUTCTime end start
    logMsg "Time Taken: " diff

logMsg :: Show a => String -> a -> IO()
logMsg msg x = do
    putStr msg
    print x