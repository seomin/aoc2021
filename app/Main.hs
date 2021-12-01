module Main where

import Lib

main :: IO ()
main = do
    input <- readFile "input.01.a"
    putStrLn $ lvl1 input
