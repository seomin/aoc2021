module Day02Spec
  ( spec
  ) where

import Day02
import Support
import Test.Hspec

spec = do
  describe "part 1" $ do
    it "can solve the example" $ do part1 day02 exampleData `shouldBe` 150
  describe "part 2" $ do
    it "can solve the example" $ do part2 day02 exampleData `shouldBe` 900

exampleData =
  ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
