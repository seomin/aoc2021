module Day10
  ( day10
  ) where

import Data.Maybe (mapMaybe)
import Support

day10 = Day "10" "input/10.txt" solution1 solution2

solution1 :: Solution
solution1 = fromIntegral . sum . map score . mapMaybe (corrupted "")

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score _ = error "unreachable"

corrupted :: String -> String -> Maybe Char
corrupted stack (c:cs)
  | c == '(' = corrupted (')' : stack) cs
  | c == '[' = corrupted (']' : stack) cs
  | c == '{' = corrupted ('}' : stack) cs
  | c == '<' = corrupted ('>' : stack) cs
  | otherwise =
    if c == head stack
      then corrupted (tail stack) cs
      else Just c
corrupted _ [] = Nothing -- Incomplete

solution2 :: Solution
solution2 = error "Not implemented"
