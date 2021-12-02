module Lib
  ( lvl1a
  , lvl1b
  ) where

countIncreases :: (Ord a, Num a) => [a] -> Int
-- countIncreases xs = length $ filter (> 0) $ zipWith (-) (tail xs) xs
countIncreases = length . filter (< 0) . (zipWith (-) <*> tail)

lvl1a :: String -> Int
lvl1a = countIncreases . map read . lines

window3 :: Num d => [d] -> [d]
window3 = (zipWith3 (\a b c -> a + b + c) <*> tail) =<< tail

lvl1b :: String -> Int
lvl1b = countIncreases . window3 . map read . lines
