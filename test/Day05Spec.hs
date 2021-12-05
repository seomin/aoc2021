module Day05Spec
  ( spec
  ) where

import Control.Monad.IO.Class (liftIO)
import Day05
import Support
import Test.Hspec

spec = do
  describe "part 1" $ do
    it "can solve the example" $ do
      exampleData <- liftIO $ readFile "test/input/05.txt"
      part1 day05 (lines exampleData) `shouldBe` 5
  describe "part 2" $ do
    it "can solve the example" $ do
      exampleData <- liftIO $ readFile "test/input/05.txt"
      part2 day05 (lines exampleData) `shouldBe` 12
