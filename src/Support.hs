module Support
  ( Day(..)
  , Solution
  , run
  , time
  ) where

import System.CPUTime (getCPUTime)
import Text.Printf (printf)

type Solution = [String] -> Integer

data Day =
  Day
    { idx :: String
    , inputFile :: FilePath
    , part1 :: Solution
    , part2 :: Solution
    }

run :: Day -> IO ()
run d = do
  putStrLn $ "[[Running Day " ++ idx d ++ "]]"
  time $ do
    input <- readFile $ inputFile d
    let solution1 = part1 d $ lines input
    putStrLn $ "  Solution for part1: " ++ show solution1
  time $ do
    input <- readFile $ inputFile d
    let solution2 = part2 d $ lines input
    putStrLn $ "  Solution for part2: " ++ show solution2
  putStrLn ""

time :: IO t -> IO t
time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ 12)
  printf "   [Computation took: %0.3f sec]\n" (diff :: Double)
  return v
