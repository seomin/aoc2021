module Main where

import Lib

main :: IO ()
main = do
    input <- readFile "input/input.01"
    putStrLn $ "Level 1a: " ++ show (lvl1a input)

    putStrLn $ "Level 1b: " ++ show (lvl1b input)
