module Day05
  ( day05
  ) where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
  ( Map
  , empty
  , filter
  , foldl
  , insertWith
  , size
  )
import Support
import System.Posix.Internals (lstat)

day05 = Day "05" "input/05.txt" solution1 solution2

solution1 :: Solution
solution1 =
  fromIntegral .
  Map.size .
  Map.filter (> 1) .
  foldl collect Map.empty . concatMap toCoords . filter notDiag . parseLines

parseLines :: [String] -> [Line]
parseLines ls = do
  l <- ls
  let [c1, c2] = splitOn " -> " l
      [x1, y1] = splitOn "," c1
      [x2, y2] = splitOn "," c2
  return $ Line (read x1) (read y1) (read x2) (read y2)

toCoords :: Line -> [(Int, Int)]
toCoords (Line x1 y1 x2 y2)
  | x1 == x2 = [(x1, y) | y <- [y1 .. y2] ++ [y2 .. y1]]
  | y1 == y2 = [(x, y1) | x <- [x1 .. x2] ++ [x2 .. x1]]
  | otherwise =
    zip [x1,(signum (x2 - x1) + x1) .. x2] [y1,(signum (y2 - y1) + y1) .. y2]

collect :: Map.Map (Int, Int) Int -> (Int, Int) -> Map.Map (Int, Int) Int
collect m k = Map.insertWith (+) k 1 m

data Line =
  Line
    { x1 :: Int
    , y1 :: Int
    , x2 :: Int
    , y2 :: Int
    }

notDiag :: Line -> Bool
notDiag (Line x1 y1 x2 y2) = x1 == x2 || y1 == y2

solution2 :: Solution
solution2 =
  fromIntegral .
  Map.size .
  Map.filter (> 1) . foldl collect Map.empty . concatMap toCoords . parseLines
