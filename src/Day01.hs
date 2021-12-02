module Day01
  ( day01
  ) where

import Support

day01 = Day "01" "input/input.01" solution1 solution2

solution1 :: Solution
solution1 = fromIntegral . countIncreases . map read

solution2 :: Solution
solution2 = fromIntegral . countIncreases . window3 . map read

countIncreases :: (Ord a, Num a) => [a] -> Int
-- countIncreases xs = length $ filter (> 0) $ zipWith (-) (tail xs) xs
countIncreases = length . filter (< 0) . (zipWith (-) <*> tail)

window3 :: Num d => [d] -> [d]
-- window3 xs = zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail $ tail xs)
window3 = (zipWith3 (\a b c -> a + b + c) =<< tail) =<< tail
