module Lib
  ( lvl1a
  , lvl1b
  ) where

countIncreases :: [Integer] -> Int
-- countIncreases xs = length $ filter (> 0) $ zipWith (-) (tail xs) xs
countIncreases = length . filter (< 0) . (zipWith (-) <*> tail)

lvl1a :: String -> Int
lvl1a = countIncreases . map read . lines

lvl1b :: String -> Int
lvl1b =
  countIncreases .
  ((zipWith3 (\a b c -> a + b + c) <*> tail) =<< tail) . map read . lines
