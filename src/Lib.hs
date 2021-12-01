module Lib
    ( lvl1
    ) where

lvl1 :: String -> String
lvl1 s = show $ 
        length $
        filter (>0) $
        zipWith (-) (tail ints) ints
    where ints = (map read $ lines s) :: [Integer]
