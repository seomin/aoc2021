module Lib
    ( lvl1a
    , lvl1b
    ) where

lvl1a :: String -> String
lvl1a s = show $ 
        length $
        filter (>0) $
        zipWith (-) (tail ints) ints
    where ints = (map read $ lines s) :: [Integer]

lvl1b :: String -> String
lvl1b s = show $ 
        length $
        filter (>0) $
        zipWith (-) (tail w3) w3
    where 
        xs = (map read $ lines s) :: [Integer]
        w3 = zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail $ tail xs)
