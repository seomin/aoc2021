module Day10
  ( day10
  ) where

import Data.List (sort)
import Data.Maybe (mapMaybe)
import Foreign (callocArray0)
import Support

day10 = Day "10" "input/10.txt" solution1 solution2

solution1 :: Solution
solution1 = fromIntegral . sum . map score . mapMaybe (corrupted . parse "")

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score _ = error "unreachable"

corrupted :: ParseResult -> Maybe Char
corrupted (Corrupted c) = Just c
corrupted _ = Nothing

data ParseResult
  = Valid
  | Corrupted Char
  | Incomplete String

parse :: String -> String -> ParseResult
parse stack (c:cs)
  | c == '(' = parse (')' : stack) cs
  | c == '[' = parse (']' : stack) cs
  | c == '{' = parse ('}' : stack) cs
  | c == '<' = parse ('>' : stack) cs
  | otherwise =
    if c == head stack
      then parse (tail stack) cs
      else Corrupted c
parse [] [] = Valid
parse stack [] = Incomplete stack

incomplete :: ParseResult -> Maybe String
incomplete (Incomplete s) = Just s
incomplete _ = Nothing

solution2 :: Solution
solution2 = median . map scoreCompletion . mapMaybe (incomplete . parse "")

scoreCompletion :: String -> Integer
scoreCompletion =
  foldl (\acc i -> 5 * acc + fromIntegral i) 0 . map scoreCompletionChars

scoreCompletionChars :: Char -> Int
scoreCompletionChars c =
  case c of
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4
    _ -> error "unreachable"

median :: [Integer] -> Integer
median is = sort is !! (length is `div` 2)
