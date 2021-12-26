module Day12Spec
  ( spec
  ) where

import Control.Monad.IO.Class (liftIO)
import Day12
import Support
import Test.Hspec

spec = do
  describe "part 1" $ do
    it "can solve the short example" $ do
      exampleData <- liftIO $ readFile "test/input/12-small.txt"
      part1 day12 (lines exampleData) `shouldBe` 10
    it "can solve the medium example" $ do
      exampleData <- liftIO $ readFile "test/input/12-medium.txt"
      part1 day12 (lines exampleData) `shouldBe` 19
    it "can solve the large example" $ do
      exampleData <- liftIO $ readFile "test/input/12-large.txt"
      part1 day12 (lines exampleData) `shouldBe` 226
  describe "part 2" $ do
    it "can solve the short example" $ do
      exampleData <- liftIO $ readFile "test/input/12-small.txt"
      part2 day12 (lines exampleData) `shouldBe` 36
    it "can solve the medium example" $ do
      exampleData <- liftIO $ readFile "test/input/12-medium.txt"
      part2 day12 (lines exampleData) `shouldBe` 103
    it "can solve the large example" $ do
      exampleData <- liftIO $ readFile "test/input/12-large.txt"
      part2 day12 (lines exampleData) `shouldBe` 3509
