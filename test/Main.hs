module Main (main) where

import Test.Hspec
import qualified Test.Day12 as T

main :: IO()
main = hspec $ do
    T.spec