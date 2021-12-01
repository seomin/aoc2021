module Lib
  ( lvl1a,
    lvl1b,
  )
where

countIncreases :: [Integer] -> Int
countIncreases xs = length $ filter (> 0) $ zipWith (-) (tail xs) xs

lvl1a :: String -> Int
lvl1a = countIncreases . map read . lines

lvl1b :: String -> Int
lvl1b s = countIncreases w3
  where
    xs = map read $ lines s
    w3 = zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail $ tail xs)
