{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day06
  ( day06
  , solve
  ) where

import Data.List.Split (splitOn)
import Support

day06 = Day "06" "input/06.txt" solution1 solution2

solution1 :: Solution
solution1 [input] = solve input 80

solve :: String -> Int -> Integer
solve input days = sum $ simulate slots days
  where
    fishes = (map read $ splitOn "," input) :: [Int]
    slots = foldl (\ss f -> setAt ss f (ss !! f + 1)) (replicate 9 0) fishes

simulate :: [Integer] -> Int -> [Integer]
simulate slots 0 = slots
simulate slots steps =
  simulate
    [ slots !! 1
    , slots !! 2
    , slots !! 3
    , slots !! 4
    , slots !! 5
    , slots !! 6
    , slots !! 7 + head slots
    , slots !! 8
    , head slots
    ]
    (steps - 1)

setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

solution2 :: Solution
solution2 [input] = solve input 256
