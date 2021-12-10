module Day09Spec
  ( spec
  ) where

import Control.Monad.IO.Class (liftIO)
import Day09
import Support
import Test.Hspec

spec = do
  describe "part 1" $ do
    it "can solve the example" $ do
      exampleData <- liftIO $ readFile "test/input/09.txt"
      part1 day09 (lines exampleData) `shouldBe` 15
  -- describe "part 2" $ do
  --   it "can solve the example" $ do
  --     exampleData <- liftIO $ readFile "test/input/09.txt"
  --     part2 day09 (lines exampleData) `shouldBe` 1134
