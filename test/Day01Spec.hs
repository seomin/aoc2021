module Day01Spec
  ( spec
  ) where

import Day01
import Support
import Test.Hspec

spec = do
  describe "part 1" $ do
    it "can solve the example" $ do part1 day01 (lines exampleData) `shouldBe` 7
  describe "part 2" $ do
    it "can solve the example" $ do part2 day01 (lines exampleData) `shouldBe` 5

exampleData =
  "199\n\
\200\n\
\208\n\
\210\n\
\200\n\
\207\n\
\240\n\
\269\n\
\260\n\
\263"
