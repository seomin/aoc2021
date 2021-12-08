{-# LANGUAGE TupleSections #-}

module Day04
  ( day04
  , parseBoards
  , play
  , winner
  ) where

import Data.List (partition)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Support

day04 = Day "04" "input/04.txt" solution1 solution2

solution1 :: Solution
solution1 (numberString:boardString) = result winner
  where
    numbers = map read $ splitOn "," numberString
    boards = parseBoards boardString
    winner = head $ play numbers boards
solution1 _ = error "unreachable"

type Cell = (Int, Bool)

data Board =
  Board
    { cells :: [Cell]
    , won :: Bool
    , winningNumber :: Maybe Int
    }
  deriving (Show)

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards ("":bs) = parseBoards bs
parseBoards (a:b:c:d:e:rest) = board : parseBoards rest
  where
    cs = parseLine a ++ parseLine b ++ parseLine c ++ parseLine d ++ parseLine e
    board = Board cs False Nothing
    parseLine :: String -> [(Int, Bool)]
    parseLine = map ((, False) . read) . words
parseBoards _ = error "unreachable"

play :: [Int] -> [Board] -> [Board]
play [] boards = []
play (call:calls) boards = winners ++ play calls losers
  where
    nextBoards = map (markBoard call) boards
    (winners, losers) = partition won nextBoards

winner :: [Cell] -> Bool
winner b = winningRow b || winningCol b

winningCol :: [Cell] -> Bool
winningCol cs =
  or [and [snd $ cs !! (i + j) | j <- [0,5 .. 20]] | i <- [0 .. 4]]

winningRow :: [Cell] -> Bool
winningRow [] = False
winningRow cs = (all snd . take 5) cs || winningRow (drop 5 cs)

markBoard :: Int -> Board -> Board
markBoard _ board@Board {won = True} = board
markBoard call board =
  board {cells = marked, won = wonNow, winningNumber = maybeCall}
  where
    marked = mark call $ cells board
    wonNow = winner marked
    maybeCall =
      if wonNow
        then Just call
        else Nothing

mark :: Int -> [Cell] -> [Cell]
mark number [] = []
mark number ((n, m):rest)
  | number == n = (n, True) : rest
  | otherwise = (n, m) : mark number rest

result :: Board -> Integer
result board = fromIntegral $ unmarked * fromMaybe 0 (winningNumber board)
  where
    unmarked = sum . map fst . filter (not . snd) $ cells board

solution2 :: Solution
solution2 (numberString:boardString) = result lastWinner
  where
    numbers = map read $ splitOn "," numberString
    boards = parseBoards boardString
    allWinners = play numbers boards
    lastWinner = last allWinners
solution2 _ = error "unreachable"
