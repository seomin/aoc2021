module Day09
  ( day09
  ) where

import Data.Char (digitToInt)
import Data.List (zip5)
import Data.Maybe (mapMaybe)
import Support

day09 = Day "09" "input/09.txt" solution1 solution2

solution1 :: Solution
solution1 = sum . map (fromIntegral . (+ 1)) . lowPoints . map (map digitToInt)

lowPoints :: [[Int]] -> [Int]
lowPoints lines =
  concat $
  zipWith3
    lowPointsLine
    (repeat 10 : lines)
    lines
    (tail lines ++ repeat (repeat 10))

lowPointsLine :: [Int] -> [Int] -> [Int] -> [Int]
lowPointsLine tops line bots =
  mapMaybe maybeLow (zip5 tops (10 : line) line (tail line ++ repeat 10) bots)

maybeLow :: (Int, Int, Int, Int, Int) -> Maybe Int
maybeLow (top, left, this, right, bot) =
  if all (> this) [top, left, right, bot]
    then Just this
    else Nothing

solution2 :: Solution
solution2 = error "Not implemented"
