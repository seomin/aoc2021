module Day03
  ( day03
  ) where

import Control.Monad (liftM2)
import Data.Char (digitToInt)
import Support

day03 = Day "03" "input/input.03" solution1 solution2

solution1 :: Solution
solution1 xs =
  fromIntegral $
  liftM2 (*) fst snd $
  foldl dupInc (0, 0) $ foldl1 (zipWith (+)) $ map (map $ sign . digitToInt) xs

sign :: Int -> Int
sign = (1 -) . (* 2)

dupInc :: (Int, Int) -> Int -> (Int, Int)
dupInc (epsilon, gamma) agg
  | agg > 0 = (epsilon * 2 + 1, gamma * 2)
  | otherwise = (epsilon * 2, gamma * 2 + 1)

solution2 :: Solution
solution2 = error "Not implemented"
