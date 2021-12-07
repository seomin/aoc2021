module Day07Spec
  ( spec
  ) where

import Control.Monad.IO.Class (liftIO)
import Day07
import Support
import Test.Hspec

spec = do
  describe "part 1" $ do
    it "can solve the example" $ do
      exampleData <- liftIO $ readFile "test/input/07.txt"
      part1 day07 (lines exampleData) `shouldBe` 37
  describe "part 2" $ do
    it "can solve the example" $ do
      exampleData <- liftIO $ readFile "test/input/07.txt"
      part2 day07 (lines exampleData) `shouldBe` 168
