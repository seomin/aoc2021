module Day11Spec
  ( spec
  ) where

import Control.Monad.IO.Class (liftIO)
import Day11
import Support
import Test.Hspec

spec = do
  describe "part 1" $ do
    it "can solve the example" $ do
      exampleData <- liftIO $ readFile "test/input/11.txt"
      part1 day11 (lines exampleData) `shouldBe` 1656
  describe "part 2" $ do
    it "can solve the example" $ do
      exampleData <- liftIO $ readFile "test/input/11.txt"
      part2 day11 (lines exampleData) `shouldBe` 195
