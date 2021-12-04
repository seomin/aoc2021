module Day03
  ( day03
  ) where

import Control.Monad (liftM2)
import Data.Char (digitToInt)
import Data.Function ((&))
import Data.List (sort)
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
solution2 xs = fromIntegral $ b2d oxygen * b2d co2
  where
    sorted = sort xs
    oxygen = findOxygen 0 sorted
    co2 = findCO2 0 sorted

b2d :: String -> Int
b2d s = b2d' s 0
  where
    b2d' [] acc = acc
    b2d' ('0':ss) acc = b2d' ss $ 2 * acc
    b2d' ('1':ss) acc = b2d' ss $ 2 * acc + 1
    b2d' _ _ = error "Unreachable"

findCO2 :: Int -> [String] -> String
findCO2 _ [x] = x
findCO2 i xs =
  if length zeros > length ones
    then findCO2 (i + 1) ones
    else findCO2 (i + 1) zeros
  where
    (zeros, ones) = span (zeroAt i) xs

findOxygen :: Int -> [String] -> String
findOxygen _ [x] = x
findOxygen i xs =
  if length zeros > length ones
    then findOxygen (i + 1) zeros
    else findOxygen (i + 1) ones
  where
    (zeros, ones) = span (zeroAt i) xs

zeroAt i s = s !! i == '0'
