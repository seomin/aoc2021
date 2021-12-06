module Day06Spec
  ( spec
  ) where

import Control.Monad.IO.Class (liftIO)
import Day06
import Support
import Test.Hspec

spec = do
  describe "part 1" $ do
    it "can solve the example with 18 days" $ do
      exampleData <- liftIO $ readFile "test/input/06.txt"
      part1 day06 (lines exampleData) `shouldBe` 26
    it "can solve the example with 80 days" $ do
      exampleData <- liftIO $ readFile "test/input/06.txt"
      part1 day06 (lines exampleData) `shouldBe` 5934
  describe "part 2" $ do
    it "can solve the example" $ do
      exampleData <- liftIO $ readFile "test/input/06.txt"
      part2 day06 (lines exampleData) `shouldBe` 26984457539
