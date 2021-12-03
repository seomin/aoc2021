module Day03
  ( day03
  ) where

import Control.Monad (liftM2)
import Data.Char (digitToInt)
import Support

day03 = Day "03" "input/input.03" solution1 solution2

solution1 :: Solution
solution1 =
  fromIntegral .
  liftM2 (*) fst snd . foldl dupInc (0, 0) . foldl balance (repeat 0)

balance :: [Int] -> String -> [Int]
balance (i:is) ('0':cs) = (i + 1) : balance is cs
balance (i:is) ('1':cs) = (i - 1) : balance is cs
balance _ _ = []

dupInc :: (Int, Int) -> Int -> (Int, Int)
dupInc (epsilon, gamma) agg
  | agg > 0 = (epsilon * 2 + 1, gamma * 2)
  | otherwise = (epsilon * 2, gamma * 2 + 1)

solution2 :: Solution
solution2 = error "Not implemented"
