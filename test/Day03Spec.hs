module Day03Spec
  ( spec
  ) where

import Day03
import Support
import Test.Hspec

spec = do
  describe "part 1" $ do
    it "can solve the example" $ do part1 day03 exampleData `shouldBe` 198
  describe "part 2" $ do
    it "can solve the example" $ do part2 day03 exampleData `shouldBe` 230

exampleData =
  [ "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ]
