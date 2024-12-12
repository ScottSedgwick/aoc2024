module Main (main) where

import Test.Hspec
import qualified Test.Day12 as D12

main :: IO()
main = hspec $ do
    D12.spec