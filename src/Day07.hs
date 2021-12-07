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
    positions = map read $ splitOn "," input
    median = sort positions !! (length positions `div` 2)

solution2 :: Solution
solution2 = error "Not implemented"
