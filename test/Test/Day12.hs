module Test.Day12 (spec) where

import qualified Data.Set as S
import Test.Hspec
import Day12

spec :: Spec
spec = do
    describe "Day 12" $ do
        it "Test getEdges for single point" $ do
            getEdges (S.fromList [(1,1)]) `shouldBe` 4
        it "Test getEdges for two points" $ do
            getEdges (S.fromList [(1,1), (1,2)]) `shouldBe` 4