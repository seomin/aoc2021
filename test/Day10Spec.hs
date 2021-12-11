module Day10Spec
  ( spec
  ) where

import Control.Monad.IO.Class (liftIO)
import Day10
import Support
import Test.Hspec

spec = do
  describe "part 1" $ do
    it "can solve the example" $ do
      exampleData <- liftIO $ readFile "test/input/10.txt"
      part1 day10 (lines exampleData) `shouldBe` 26397
  describe "part 2" $ do
    it "can solve the example" $ do
      exampleData <- liftIO $ readFile "test/input/10.txt"
      part2 day10 (lines exampleData) `shouldBe` 288957
