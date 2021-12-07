{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day07
  ( day07
  ) where

import Data.List (sort)
import Data.List.Split (splitOn)
import Support

day07 = Day "07" "input/07.txt" solution1 solution2

solution1 :: Solution
solution1 [input] = sum $ map (abs . (median -)) positions
  where
    positions = parsePostions input
    median = sort positions !! (length positions `div` 2)

parsePostions :: String -> [Integer]
parsePostions = map read . splitOn ","

solution2 :: Solution
solution2 [input] =
  takeSmallest [sum [distance . abs . (i -) $ p | p <- positions] | i <- [0 ..]]
  where
    positions = parsePostions input

takeSmallest :: [Integer] -> Integer
takeSmallest [a] = a
takeSmallest (a:b:rest)
  | a <= b = a
  | otherwise = takeSmallest (b : rest)

distance :: Integer -> Integer
distance n = (n * n + n) `div` 2
