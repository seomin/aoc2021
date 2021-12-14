module Day09 where

import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Char (digitToInt)
import Data.List (sort, zip5)
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShowId)
import GHC.Arr (Array, (!), (//), array, bounds, inRange, indices)
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
  mapMaybe maybeLow $ zip5 tops (10 : line) line (tail line ++ repeat 10) bots

maybeLow :: (Int, Int, Int, Int, Int) -> Maybe Int
maybeLow (top, left, this, right, bot) =
  if all (> this) [top, left, right, bot]
    then Just this
    else Nothing

solution2 :: Solution
solution2 lines =
  product $
  traceShowId $
  take 3 $ reverse $ sort $ evalState (findBasins $ indices field) field
  where
    field = parseField lines

findBasins :: [Point] -> State (Array Point Char) [Integer]
findBasins [] = return []
findBasins (p:ps) = do
  count <- findBasin p
  counts <- findBasins ps
  return (count : counts)

findBasin :: Point -> State (Array Point Char) Integer
findBasin p = do
  a <- get
  if inRange (bounds a) p && free (a ! p)
    then do
      put $ a // [(p, 'x')]
      t <- findBasin (fst p, snd p - 1)
      l <- findBasin (fst p - 1, snd p)
      r <- findBasin (fst p + 1, snd p)
      b <- findBasin (fst p, snd p + 1)
      return (t + l + r + b + 1)
    else return 0

free :: Char -> Bool
free c = c /= '9' && c /= 'x'

type Point = (Int, Int)

parseField :: [String] -> Array Point Char
parseField lines =
  array
    ((0, 0), (width - 1, height - 1))
    [((x, y), c) | (y, line) <- zip [0 ..] lines, (x, c) <- zip [0 ..] line]
  where
    width = length $ head lines
    height = length lines
