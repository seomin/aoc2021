module Day08
  ( day08
  , (~=)
  ) where

import Data.Foldable (find)
import Data.List ((\\), intersect, sort, union)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

import Support

day08 = Day "08" "input/08.txt" solution1 solution2

solution1 :: Solution
solution1 =
  fromIntegral .
  length .
  filter (`elem` [2, 3, 4, 7]) .
  concatMap (map length . words . (!! 1) . splitOn "|")

solution2 :: Solution
solution2 = sum . map (fromIntegral . decodeLine)

type Decoder = String -> Char

decodeLine :: String -> Int
decodeLine line = read $ map decoder $ words digitString
  where
    [patternString, digitString] = splitOn "|" line
    signals = words patternString
    decoder = buildDecoder signals

buildDecoder :: [String] -> Decoder
buildDecoder signals = decodeDigit
  where
    seven = patternOfLength 3 signals
    one = patternOfLength 2 signals
    four = patternOfLength 4 signals
    eight = "abcdefg"
    sixer = patternsOfLength 6 signals
    bd = four \\ one
    cde = eight \\ intersect3 sixer
    [a] = seven \\ one
    [d] = bd `intersect` cde
    [b] = bd \\ [d]
    ce = cde \\ [d]
    fiver = patternsOfLength 5 signals
    [g] = (intersect3 fiver \\ [a]) \\ [d]
    [e] = (eight \\ [a, b, d, g]) \\ one
    f =
      if occurs (head one) signals == 9
        then head one
        else one !! 1
    [c] = one \\ [f]
    decodeDigit digit
      | digit ~= [a, b, c, e, f, g] = '0'
      | digit ~= [c, f] = '1'
      | digit ~= [a, c, d, e, g] = '2'
      | digit ~= [a, c, d, f, g] = '3'
      | digit ~= [b, c, d, f] = '4'
      | digit ~= [a, b, d, f, g] = '5'
      | digit ~= [a, b, d, e, f, g] = '6'
      | digit ~= [a, c, f] = '7'
      | digit ~= [a, b, c, d, e, f, g] = '8'
      | digit ~= [a, b, c, d, f, g] = '9'
      | otherwise =
        error $ "unreachable: " ++ digit ++ ", " ++ show [a, b, c, d, e, f, g]

(~=) :: String -> String -> Bool
(~=) left right = length (left `union` right) == length (left `intersect` right)

occurs :: Char -> [String] -> Int
occurs char = length . filter (char `elem`)

intersect3 :: [String] -> String
intersect3 [x, y, z] = x `intersect` y `intersect` z
intersect3 _ = error "unreachable"

patternsOfLength :: Int -> [String] -> [String]
patternsOfLength l = filter ((== l) . length)

patternOfLength :: Int -> [String] -> String
patternOfLength l = fromJust . find ((== l) . length)
