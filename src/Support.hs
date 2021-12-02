module Support
  ( Day(..)
  , Solution
  , run
  ) where

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
  input <- readFile $ inputFile d
  let solution1 = part1 d $ lines input
  putStrLn $ "  Solution for part1: " ++ show solution1
  let solution2 = part2 d $ lines input
  putStrLn $ "  Solution for part2: " ++ show solution2
  putStrLn ""
