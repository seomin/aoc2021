module Day08Spec
  ( spec
  ) where

import Control.Monad.IO.Class (liftIO)
import Day08
import Support
import Test.Hspec

spec = do
  describe "part 1" $ do
    it "can solve the example" $ do
      exampleData <- liftIO $ readFile "test/input/08.txt"
      part1 day08 (lines exampleData) `shouldBe` 26
  describe "part 2" $ do
    it "can solve the short example" $ do
      let exampleData =
            "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
      part2 day08 [exampleData] `shouldBe` 5353
    it "can solve the long example" $ do
      exampleData <- liftIO $ readFile "test/input/08.txt"
      part2 day08 (lines exampleData) `shouldBe` 61229
