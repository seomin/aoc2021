module Day12 where

import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.Char (isUpper)
import Data.Fixed (E0)
import Data.List (elemIndex, findIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import GHC.Arr (Array, (!), (//), array, listArray)
import Support

day12 = Day "12" "input/12.txt" solution1 solution2

solution1 :: Solution
solution1 input = findPaths graph size True startIndex endIndex
  where
    (graph, size, startIndex, endIndex) = parse input

type Point = (Int, Int)

data Passage
  = None
  | MaybeOneMoreTime
  | Single
  | Multiple
  deriving (Eq, Show)

dumpGraph :: Array Point Passage -> Int -> String
dumpGraph g s =
  unlines
    [unwords [show (g ! (x, y)) | y <- [0 .. (s - 1)]] | x <- [0 .. (s - 1)]]

findPaths :: Array Point Passage -> Int -> Bool -> Int -> Int -> Integer
findPaths graph size usedSingleTwice from end =
  sum $ map findPaths' [0 .. (size - 1)]
  where
    findPaths' to =
      case graph ! (from, to) of
        None -> 0
        MaybeOneMoreTime ->
          if usedSingleTwice
            then 0
            else findPaths graph size True to end
        Single ->
          if end == to
            then 1
            else findPaths
                   (graph //
                    [ ( (from', to)
                      , if graph ! (from', to) == None
                          then None
                          else MaybeOneMoreTime)
                    | from' <- [0 .. (size - 1)]
                    ])
                   size
                   usedSingleTwice
                   to
                   end
        Multiple -> findPaths graph size usedSingleTwice to end

parse :: [String] -> (Array Point Passage, Int, Int, Int)
parse ls =
  ( listArray ((0, 0), (size - 1, size - 1)) (replicate (size * size) None) //
    ass
  , size
  , fromJust $ elemIndex "start" mapping
  , fromJust $ elemIndex "end" mapping)
  where
    (ass, mapping) = runState (buildMapping ls) []
    size = length mapping

buildMapping :: [String] -> State [String] [(Point, Passage)]
buildMapping [] = return []
buildMapping (s:ss) = do
  let (a:b:_) = splitOn "-" s
  ai <- numberNode a
  bi <- numberNode b
  rest <- buildMapping ss
  return $ makeAss ai bi b : makeAss bi ai a : rest
  where
    numberNode :: String -> State [String] Int
    numberNode x = do
      table <- get
      case elemIndex x table of
        Nothing -> do
          put (table ++ [x])
          return (length table)
        Just i -> return i

makeAss :: Int -> Int -> String -> (Point, Passage)
makeAss x y to =
  ( (x, y)
  , if isUpper $ head to
      then Multiple
      else if to == "start"
             then None
             else Single)

solution2 :: Solution
solution2 input = findPaths graph size False startIndex endIndex
  where
    (graph, size, startIndex, endIndex) = parse input
