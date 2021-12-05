module Day04Spec
  ( spec
  ) where

import Control.Monad.IO.Class (liftIO)
import Day04
import Support
import Test.Hspec

spec = do
  describe "part 1" $ do
    it "can solve the example" $ do
      exampleData <- liftIO $ readFile "test/input/04.txt"
      part1 day04 (lines exampleData) `shouldBe` 4512
  describe "part 2" $ do
    it "can solve the example" $ do
      exampleData <- liftIO $ readFile "test/input/04.txt"
      part2 day04 (lines exampleData) `shouldBe` 1924
