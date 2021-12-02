module Main where

import Lib

main :: IO ()
main
  -- input1 <- readFile "input/input.01"
  -- putStrLn $ "Level 1a: " ++ show (lvl1a input1)
  -- putStrLn $ "Level 1b: " ++ show (lvl1b input1)
 = do
  input2 <- readFile "input/input.02"
  putStrLn $ "Level 2a: " ++ show (level2a input2)
  putStrLn $ "Level 2b: " ++ show (level2b input2)
